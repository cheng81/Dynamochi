-module (dynamochi_handler).

-export ([behaviour_info/1]).

behaviour_info( callbacks ) ->
	[{handle/1}];

behaviour_info( _ ) -> undefined.