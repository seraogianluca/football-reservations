-module(dashboard_server).
-import(dashboard_db, [start_db/0, stop_db/0, insert_message/3, read_messages/1, delete_messages/1]).

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
            io:format("[INFO] insert received~n"),
            Response = insert_message(GameId, Username, Message),
            {reply, Response, {}};
        {read, GameId} ->
            io:format("[INFO] read received~n"),
            Response = read_messages(GameId),
            {reply, Response, {}};
        {delete, GameId} ->
            io:format("[INFO] delete received~n"),
            Response = delete_messages(GameId),
            {reply, Response, {}};
        _ ->
            {reply, bad_request, {}}
    end.

handle_cast(_, _) ->
    not_implemented.