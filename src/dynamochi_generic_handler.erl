-module (dynamochi_generic_handler,[Context,DocRoot,HandleFun]).
-include ("../include/dynamochi.hrl").

-behaviour (dynamochi_handler).

-export ([handle/1, registration/0]).

handle(DynamochiReq) ->
	HandleFun(DynamochiReq).

registration() ->
	#dynamochi_reg{context=Context,doc_root=DocRoot}.
	%the handler field will be filled by dynamochi_server:register function