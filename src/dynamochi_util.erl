%% @author Francesco Zanitti <frza@itu.dk>

-module (dynamochi_util).
-author('Francesco Zanitti <frza@itu.dk>').

-include ("../include/dynamochi.hrl").

-export ([register/3, create_handler_function/1, create_handler_function/2]).

register( Context, DocRoot, Handler ) when is_function(Handler) ->
	dynamochi_server:register( dynamochi_generic_handler:new(Context,DocRoot,Handler) );
register( Context, DocRoot, Handler ) ->
	dynamochi_server:register( #dynamochi_reg{context=Context,doc_root=DocRoot,handler=Handler} ).

create_handler_function( Fun ) when is_function(Fun,3) ->
	fun (#dynamochi_req{method=M,path=P,request=R}) ->
		Fun(M,P,R)
	end.

create_handler_function( Module,Function ) ->
	fun (#dynamochi_req{method=M,path=P,request=R}) ->
		apply(Module,Function,[M,P,R])
	end.