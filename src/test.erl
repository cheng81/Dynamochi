-module (test).

-include ("../include/dynamochi.hrl").

-export ([start/0, sample_handle/3]).

start() ->
	application:start(crypto),
	application:start(dynamochi),
	dynamochi_server:register( dynamochi_generic_handler:new("test1","",fun (I) -> handle_fun1(I) end) ),
	dynamochi_server:register( dynamochi_generic_handler:new("test12","",fun (I) -> handle_fun2(I) end) ),
	%dynamochi_server:register( dynamochi_generic_handler:new("test3","",generic_handle_fun(fun handle/3)) )
	
	dynamochi_util:register( "test3","",generic_handle_fun(fun handle/3) ),
	dynamochi_util:register( "test4","",dynamochi_util:create_handler_function(?MODULE,sample_handle))
	.

handle_fun1( #dynamochi_req{request=R,path=P} ) ->
	R:ok({"text/plain","hi everybody (1) ! "++P}).

handle_fun2( #dynamochi_req{request=R,path=P} ) ->
	R:ok({"text/plain","hi everybody (2)! "++P}).

generic_handle_fun( Fun ) ->
	fun ( #dynamochi_req{method=M,path=P,request=R} ) ->
		case Fun( M,P,R ) of
			{Mime,Text} -> R:ok({Mime,Text});
			Text		-> R:ok({"text/plain",Text})
		end
	end.

handle('GET', "foo", _) ->
	"oh, I see that you are foo!";
handle('GET', Any, _) ->
	{
	"text/html",
	"<html><head><title>Hi "++Any++"!</title></head><body><h1>Hi, "++Any++"!</h1></body></html>"
	}.
sample_handle('GET',Any,R) ->
	R:ok({
	"text/html",
	"<html><head><title>Hi "++Any++"!</title></head><body><h3>Hi, "++Any++"!</h3><p>powered by DynaMochi</p></body></html>"
	}).