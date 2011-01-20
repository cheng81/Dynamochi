%% @author Francesco Zanitti <frza@itu.dk>

-module(dynamochi).
-author('Francesco Zanitti <frza@itu.dk>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
        
%% @spec start() -> ok
%% @doc Start the dynamochi server.
start() ->
    dynamochi_deps:ensure(),
    ensure_started(crypto),
    application:start(dynamochi).

%% @spec stop() -> ok
%% @doc Stop the dynamochi server.
stop() ->
    Res = application:stop(dynamochi),
    application:stop(crypto),
    Res.
