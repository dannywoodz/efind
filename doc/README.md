

# efind #

Copyright (c) 2016 Danny Woods

__Version:__ 1.0

__Authors:__ Danny Woods ([`dannywoodz@yahoo.co.uk`](mailto:dannywoodz@yahoo.co.uk)).

`efind` is a utility library in the spirit of the Unix `find` tool


# Examples #

[`efind:find/1`](efind.md#find-1) and [`efind:find/2`](efind.md#find-2) scan the selected root and return a list of results.

```erlang

  RootDir = "...".
  ListOfTuples = efind:find(RootDir). % Each tuple is {file, Filename} or {dir, Dirname}
  ListOfNames = efind:find(RootDir, [{result_type,names}]).
  ListOfFileNames = efind:find(RootDir, [{result_type,names},{dirs,false}]).
  ListOfDirNames = efind:find(RootDir, [{result_type,names},{files,false}]).
  ListOfXmlFiles = efind:find(RootDir,
  		 [{result_type,names},
		 {dirs,false},
		 {accept_fn, fun(Name) -> filename:extension(Name) == ".xml" end}]).

```

[`efind:scan/1`](efind.md#scan-1) and [`efind:scan/2`](efind.md#scan-2) are lazy versions of `find`.  In concert
with [`efind:next/1`](efind.md#next-1), they can be used to walk the filesystem on-demand.  A scanner that is
drained of all content will return finished from [`efind:next/1`](efind.md#next-1) and is
terminated automatically.  However, if you wish to abandon scanning early, you _must_ call
[`efind:close/1`](efind.md#close-1) to release the scanner.   You'll leak a process per invocation otherwise.

```erlang

  RootDir = "...".
  Scanner = efind:scan(RootDir),
  Fn = fun(finished, _F) ->
	  	io:format("Done~n"),
	  (Entry, F) ->
	  	io:format("~p~n", [Entry]),
		F(efind:next(Scanner), F)
	end,
  Fn(efind:next(Scanner), Fn).

```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="efind.md" class="module">efind</a></td></tr></table>

