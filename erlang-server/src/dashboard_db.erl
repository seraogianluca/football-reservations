-module(dashboard_db).
-include("dashboard.hrl").

-export([init/0, start_db/0, stop_db/0, insert_message/3, read_messages/1]).

init() ->
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:change_table_copy_type(schema, node(), disc_copies),
  mnesia:create_table(messages, [{type, bag}, {disc_copies, [node()]}, {attributes, record_info(fields, messages)}]).

start_db() ->
  mnesia:start().

stop_db() ->
  mnesia:stop().

insert_message(GameId, Username, Message) ->
  Time = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  Record = #messages{game_id = GameId, username = Username, time = Time, message = Message},
  Fun = fun() ->
    mnesia:write(Record)
        end,
  case mnesia:transaction(Fun) of
    {atomic, ok} -> success;
    _ -> error
  end.

read_messages(GameId) ->
  Read = fun() ->
    mnesia:read(GameId)
         end,
  mnesia:transaction(Read).
