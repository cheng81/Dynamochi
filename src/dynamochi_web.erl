%% @author Francesco Zanitti <frza@itu.dk>

%% @doc Web server for dynamochi.

-module(dynamochi_web).
-author('Francesco Zanitti <frza@itu.dk>').

-export([start/1, stop/0, loop/2]).

%% External API

start(Options) ->
	io:format("starting dynamochi_web~n",[]),
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req, DocRoot)
           end,
	dynamochi_server:start(void),
    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
	dynamochi_server:stop(),
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
	% first try to make the dynamic handlers deal with the request
	case dynamochi_server:handle(Req) of
		% no matching handler found
		not_found ->
			"/" ++ Path = Req:get(path),
			case is_get_head(Req:get(method)) of
				% if it's a GET request, try to serve a (global) static file
				true -> Req:serve_file(Path,DocRoot);
				% give up, return 404
				false -> Req:not_found()
			end;
		% the request has been handled by some dynamic handler
		_ -> ok
	end.

%% Internal API

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

is_get_head('GET') -> true;
is_get_head('HEAD') -> true;
is_get_head(_Other) -> false.
