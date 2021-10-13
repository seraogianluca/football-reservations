-module(dashboard_server).
-behaviour(gen_server).

-import(dashboard_db, [start_db/0, stop_db/0, insert_message/3, read_messages/1]).

-export([init/1, start_server/0, stop_server/0, call_server/1]).
-export([handle_cast/2, handle_call/3]).


start_server() ->
    start_db(),
    gen_server:start({local, dashboard_server}, ?MODULE, [],[]).

stop_server() ->
    stop_db(),
    gen_server:stop(dashboard_server).

call_server(Content) ->
    gen_server:call(dashboard_server, Content).

init(_Args) ->
    {ok, {}}.

handle_cast(_, _) ->
    not_implemented.

handle_call(Request, _, _) ->
    case Request of
        {insert, GameId, Username, Message} ->
            io:format("[INFO] request received~n"),
            Response = insert_message(GameId, Username, Message),
            {reply, Response, _ = '_'};
        {read, GameId} ->
            io:format("sono in read"),
            Response = read_messages(GameId),
            {reply, Response, _ = '_'};
        _ ->
            {reply, bad_request, _ = '_'}
    end.