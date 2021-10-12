-module(dashboard_server).
-import(dashboard_db, [start_db/0, stop_db/0, insert_message/3, read_messages/1]).

-export([init/1, handle_cast/2, handle_call/3, start_server/0, stop_server/0]).
-behaviour(gen_server).

start_server() ->
    start_db(),
    gen_server:start({local, dashboard_server}, ?MODULE, [],[]).

stop_server() ->
    stop_db(),
    gen_server:stop(dashboard_server).

init(_Args) ->
    {ok, {}}.

handle_cast(_, _) ->
    not_implemented.

handle_call(Request, _, _) ->
    case Request of
        {insert, GameId, Username, Message} ->
            Response = insert_message(GameId, Username, Message),
            {reply, Response, {}};
        {read, GameId} ->
            io:format("sono in read"),
            Response = read_messages(GameId),
            {reply, Response, {}};
        _ ->
            {reply, bad_request, {}}
    end.