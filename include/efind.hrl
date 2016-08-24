-record(scanner, {root                           :: string(),
		  scanner                        :: undefined | pid(),
		  dirs=true                      :: boolean(),
		  files=true                     :: boolean(),
		  accept_fn = fun(_) -> true end :: function(),
		  finished=false                 :: boolean(),
		  result_type=basic              :: basic | names}).
