-module(dashboard_db).
-include("dashboard.hrl").

-export([init/0, start_db/0, stop_db/0, handle_insert/3, handle_read/1, insert_dashboard/2]).

% test
-export([delete_messages/1, read_dashboards/1, is_deleted/1]).

init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:change_table_copy_type(schema, node(), disc_copies),
  mnesia:create_table(dashboards, [{type, bag}, {disc_copies, [node()]}, {attributes, record_info(fields, dashboards)}]),
  mnesia:create_table(messages, [{type, bag}, {disc_copies, [node()]}, {attributes, record_info(fields, messages)}]).

start_db() ->
  mnesia:start().

stop_db() ->
  mnesia:stop().

% Dashbboard functions
is_deleted(GameId) ->
  Match = fun() ->
    mnesia:match_object({dashboards, deleted, GameId})
          end,
  case mnesia:transaction(Match) of
    {atomic, []} -> io:format("[INFO] No match found ~n"),
      false;
    {atomic, Result} -> io:format("[INFO] Dashboard found: ~p ~n", [Result]),
      true;
    _ -> false
  end.

extract_dashboards([]) ->
  [];
extract_dashboards([H | T]) ->
  {dashboards, _, GameId} = H,
  [GameId | extract_dashboards(T)].

read_dashboards(Status) ->
  Read = fun() ->
    mnesia:read(dashboards, Status)
         end,
  case mnesia:transaction(Read) of
    {atomic, Result} -> io:format("[INFO] Retreived dashboards: ~p ~n", [Result]),
      extract_dashboards(Result);
    _ -> error
  end.

insert_dashboard(GameId, Status) ->
  Record = #dashboards{status = Status, game = GameId},
  Write = fun() ->
    mnesia:write(Record)
          end,
  case mnesia:transaction(Write) of
    {atomic, ok} -> io:format("[INFO] Dashboard inserted: ~p ~n", [Record]);
    _ -> error
  end.

% Messages functions
extract_messages([])->
  [];
extract_messages([H | T])->
  {messages, _, Username, Time, Message} = H,
  [{calendar:gregorian_seconds_to_datetime(Time), Username, Message} | extract_messages(T)].

read_messages(GameId) ->
  Read = fun() ->
    mnesia:read(messages, GameId)
         end,
  case mnesia:transaction(Read) of
    {atomic, Result} -> extract_messages(Result);
    _ -> io:format("[ERROR] Error reading messages from the dashboard: ~p ~n", [GameId]),
      error
  end.

handle_read(GameId) ->
  case is_deleted(GameId) of
    true -> error;
    false -> read_messages(GameId)
  end.

insert_message(GameId, Username, Message) ->
  Time = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  Record = #messages{game_id = GameId, username = Username, time = Time, message = Message},
  Fun = fun() ->
    mnesia:write(Record)
        end,
  case mnesia:transaction(Fun) of
    {atomic, ok} -> read_messages(GameId);
    _ -> io:format("[ERROR] Error inserting message in the dashboard: ~p ~n", [GameId]),
      error
  end.

handle_insert(GameId, Username, Message) ->
  case is_deleted(GameId) of
    true -> error;
    false -> insert_message(GameId, Username, Message)
  end.

delete_messages(GameId) ->
  Delete = fun() ->
    mnesia:delete({messages, GameId})
           end,
  case mnesia:transaction(Delete) of
    {atomic, ok} -> success;
    _ -> io:format("[ERROR] Error deleting messages of the dashboard: ~p ~n", [GameId]),
      error
  end.