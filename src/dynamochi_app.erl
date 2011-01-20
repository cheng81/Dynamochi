%% @author Francesco Zanitti <frza@itu.dk>

%% @doc Callbacks for the dynamochi application.

-module(dynamochi_app).
-author('Francesco Zanitti <frza@itu.dk>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for dynamochi.
start(_Type, _StartArgs) ->
    dynamochi_deps:ensure(),
    dynamochi_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for dynamochi.
stop(_State) ->
    ok.
