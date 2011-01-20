%% @author Francesco Zanitti <frza@itu.dk>

-module (dynamochi_server).
-author('Francesco Zanitti <frza@itu.dk>').

-include ("../include/dynamochi.hrl").

-behaviour (gen_server).

-export ([start/1, stop/0]).
-export ([handle/1, register/1, remove/1, get_callbacks/0]).
-export ([handle_by_server/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%% external API
start(_) ->
	io:format("starting dm_server~n",[]),
	gen_server:start({local,?MODULE},?MODULE,[],[]).
stop() ->
	gen_server:call(?MODULE,{stop}).

handle(Req) ->
	%
	% some remarks:
	% handling the request should be done in the current process,
	% because the module mochiweb_request store some request info
	% in the process dictionary, so those info most likely will
	% not be available in the gen_server:call function!
	%
	% so far, trying to handle the request in the call function
	% resulted in only the first request to be properly handled,
	% subsequenty requests had trouble (e.g. the path always was 
	% the one of the first request!)
	%
	% update:
	% calling Req:cleanup() when finishing the handle call
	% seems to do the job, but there is some consideration to be done
	% here, for example this may results in some troubles with
	% long-polling requests.
	% In that case, simply switch to this implementation changing
	% the loop method in the dynamochi_web module
	%
	"/"++Path = Req:get(path),
	case gen_server:call(?MODULE,{get_callback,Path}) of
		% no suitable handler found
		not_found -> not_found;
		% handler found
		{#dynamochi_reg{doc_root=Root,handler=Mod},StrippedPath} ->
			case is_static_file( is_get_head(Req:get(method)),Path,Root) of
				% not a file, dynamic dispatch to the handler module
				false -> Mod:handle( #dynamochi_req{method=Req:get(method), path=StrippedPath, request=Req} );
				% static file, serve it
				true -> Req:serve_file(Path,Root)
			end
	end.

handle_by_server(Req) ->
	gen_server:call(?MODULE,{handle,Req}).

register(Registration) when is_record(Registration,dynamochi_reg) ->
	io:format("register ~p~n",[Registration]),
	gen_server:call(?MODULE,{add_callback,Registration});
register(RegistrationAbstractModule) ->
	RegInfo = RegistrationAbstractModule:registration(),
	RegInfo1 = RegInfo#dynamochi_reg{handler=RegistrationAbstractModule},
	gen_server:call(?MODULE,{add_callback,RegInfo1}).

remove(Callback) when is_tuple(Callback) ->
	gen_server:call(?MODULE,{remove_callback,Callback});
remove(CallbackContext) ->
	gen_server:call(?MODULE,{remove_callback_context,CallbackContext}).

get_callbacks() ->
	gen_server:call(?MODULE,{get_callbacks}).

%% gen_server
init([]) ->
	{ok, []}.

handle_call( {add_callback,Callback}, _From, State ) ->
	{reply, ok, [remove_leading_slash(Callback)|State]};

handle_call( {remove_callback,Callback}, _From, State ) ->
	{reply, ok, internal_remove_callback( remove_leading_slash(Callback),State )};

handle_call( {remove_callback_context, Context}, _From, State ) ->
	{reply, ok, internal_remove_context( remove_leading_slash(Context),State )};

handle_call( {stop}, _From, State ) ->
	io:format("exiting dm_server~n",[]),
	{stop, stop_request, ok, State};

handle_call( {get_callback,Path}, _From, State ) ->
	{reply, get_callback(Path,State), State};

handle_call( {get_callbacks}, _From, State ) ->
	{reply, State, State};

handle_call( {handle,Req}, _From, State ) ->
	"/"++Path = Req:get(path),
	Reply = case get_callback(Path,State) of
		not_found -> not_found;
		{#dynamochi_reg{doc_root=Root,handler=Mod},StrippedPath} ->
			case is_static_file( is_get_head(Req:get(method)),Path,Root) of
				% not a file, dynamic dispatch to the handler module
				false -> Mod:handle( #dynamochi_req{method=Req:get(method), path=StrippedPath, request=Req} );
				% static file, serve it
				true -> Req:serve_file(Path,Root)
			end,
			ok
	end,
	% ok, the request ended his lifecycle, cleanup
	Req:cleanup(),
	{reply, Reply, State};

handle_call( Any, From, State ) ->
	io:format("dm_server cannot understand msg ~p from ~p~n",[Any,From]),
	{noreply,State}.

handle_cast( _Msg, State ) ->
	{noreply,State}.
handle_info( _Msg, State ) ->
	{noreply,State}.
terminate( _Reason, _State ) ->
	ok.
code_change( _OldVsn, State, _Extra ) ->
	{ok,State}.

%% internal api
is_get_head('GET') -> true;
is_get_head('HEAD') -> true;
is_get_head(_Other) -> false.

is_static_file(false,_Path,_DocRoot) -> false; % no GET/HEAD request
is_static_file(true,_Path,"") -> false; %no docroot defined
is_static_file(true,Path,DocRoot) ->
	case mochiweb_util:safe_relative_path(Path) of
		undefined -> io:format("unsafe relative path"), false;
		RelPath ->
			FullPath = filename:join([DocRoot,RelPath]),
			io:format("fullpath: ~s~n",[FullPath]),
			case filelib:is_dir(FullPath) of
				false ->
					case file:read_file_info(FullPath) of
						{ok,_} -> true;
						{error,_} -> io:format("not a file~n",[]), false
					end;
				true -> io:format("is dir~n",[]), true
			end
	end.

remove_leading_slash( #dynamochi_reg{context="/"++Path}=Registration ) ->
	Registration#dynamochi_reg{context=Path};
remove_leading_slash( Registration ) when is_record(Registration,dynamochi_reg) -> 
	Registration;
remove_leading_slash( "/"++Context ) -> Context;
remove_leading_slash( Context ) -> Context.

internal_remove_context( Context,State ) ->
	i_r_ctxt(Context,State,[]).
i_r_ctxt(_,[],Out) -> Out;
i_r_ctxt(Name,[#dynamochi_reg{context=Name}|T],Out) -> i_r_ctxt(Name,T,Out);
i_r_ctxt(Name,[H|T],Out) -> i_r_ctxt(Name,T,[H|Out]).

internal_remove_callback( Callback,State ) ->
	i_r_c(Callback,State,[]).
i_r_c(_,[],Out) -> Out;
i_r_c(C,[C|T],Out) -> i_r_c(C,T,Out);
i_r_c(C,[H|T],Out) -> i_r_c(C,T,[H|Out]).

get_callback( _Path, [] ) -> not_found;
get_callback( Path, [#dynamochi_reg{context=Ctxt}=C|T]) ->
	case strip_context(Path,Ctxt) of
		false -> get_callback(Path,T);
		Stripped -> {C,Stripped}
	end.

strip_context(Path, Context) ->
	case starts_with(Path,Context) of
		false -> false;
		"/"++Stripped -> Stripped;
		"" -> "";
		Any -> io:format("not the right context: ~s!=~s (~s)~n",[Path,Context,Any]), false
	end.

starts_with(Str,[]) -> Str;
starts_with([],_ToMatch) -> false;
starts_with([H|T1],[H|T2]) -> starts_with(T1,T2);
starts_with([_H1|_],[_H2|_]) -> false.
