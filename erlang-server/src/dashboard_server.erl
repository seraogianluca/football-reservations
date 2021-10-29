-module(dashboard_server).
-import(dashboard_db, [start_db/0, stop_db/0, handle_insert/3, handle_read/1, insert_dashboard/2]).

-export([start_server/0, stop_server/0]).
-export([init/1, handle_cast/2, handle_call/3]).
-behavior(gen_server).

start_server() ->
    start_db(),
    gen_server:start({local, dashboard_server}, ?MODULE, [],[]).

stop_server() ->
    stop_db(),
    gen_server:stop(dashboard_server).

init(_Args) ->
    {ok, {}}.

handle_call(Request, _From, _) ->
    case Request of
        {insert, GameId, Username, Message} ->
            io:format("[INFO] Insert request received~n"),
            Response = handle_insert(GameId, Username, Message),
            {reply, Response, {}};
        {read, GameId} ->
            io:format("[INFO] Read request received~n"),
            Response = handle_read(GameId),
            {reply, Response, {}};
        {delete, GameId} ->
            io:format("[INFO] Delete request received~n"),
            Response = insert_dashboard(GameId, deleted),
            {reply, Response, {}};
        _ ->
            {reply, bad_request, {}}
    end.

handle_cast(_, _) ->
    not_implemented.