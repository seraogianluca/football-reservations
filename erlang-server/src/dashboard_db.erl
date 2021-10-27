-module(dashboard_db).
-include("dashboard.hrl").

-export([init/0, start_db/0, stop_db/0, insert_message/3, read_messages/1, delete_messages/1]).

% test
-export([]).

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

% Messages

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

delete_messages(GameId) ->
  Delete = fun() ->
    mnesia:delete({messages, GameId})
           end,
  case mnesia:transaction(Delete) of
    {atomic, ok} -> success;
    _ -> io:format("[ERROR] Error deleting messages of the dashboard: ~p ~n", [GameId]),
      error
  end.

