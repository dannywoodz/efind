-module(test_find).
-include_lib("eunit/include/eunit.hrl").

-spec shell(string(), [string()]) -> integer().
shell(Command, Args) ->
    OSProcess = open_port({spawn_executable, os:find_executable(Command)},
                          [{args, Args}, {line,1024}, stderr_to_stdout, exit_status]),
    Drain = fun(Fn, Lines) ->
                    receive
                        {OSProcess, {data, {eol, Line}}} -> Fn(Fn,[string:strip(Line, right, $\n)|Lines]);
                        {OSProcess, {exit_status, Status}} -> {Status, lists:reverse(Lines)}
                    end
            end,
    Drain(Drain, []).

-spec touch(string()) -> ok.
touch(Filename) ->
    {ok, IO} = file:open(Filename, [write,sync]),
    ok = file:close(IO).

-spec remove_recursively(string()) -> boolean().
remove_recursively(Directory) ->
    shell("rm", ["-r", Directory]) =:= 0.

-spec with_empty_temp_directory(fun((string())->any())) -> any().
with_empty_temp_directory(Fn1) ->
    {0, [DirectoryName]} = shell("mktemp", ["-d"]),
    try
        Fn1(DirectoryName)
    after
        remove_recursively(DirectoryName)
    end.

-spec with_temp_directory_containing_files(list(string()), fun((string())->any())) -> any().
with_temp_directory_containing_files(Filenames, Fn2) ->
    with_empty_temp_directory(fun(Dir) ->
                                      [ touch(filename:join(Dir, Filename)) || Filename <- Filenames ],
                                      Fn2(Dir, Filenames)
                              end).
    

empty_dir_test() ->
    with_empty_temp_directory(fun(Dir) ->
                                      Scanner = efind:scan(Dir),
                                      ?assertNot(efind:finished(Scanner)),
                                      {{dir, Dir}, Scanner2} = efind:next(Scanner),
                                      ?assertNot(efind:finished(Scanner2)),
                                      {finished, Scanner3} = efind:next(Scanner2),
                                      ?assert(efind:finished(Scanner3)),
                                      ok
                              end).

empty_dir_no_dirs_test() ->
    with_empty_temp_directory(fun(Dir) ->
                                      Scanner = efind:scan(Dir,[{dirs,false}]),
                                      ?assertNot(efind:finished(Scanner)),
                                      {finished, Scanner2} = efind:next(Scanner),
                                      ?assert(efind:finished(Scanner2)),
                                      ok
                              end).

dirs_no_files_test() ->
    with_temp_directory_containing_files(
      ["temp.txt"],
      fun(Dir, _Filenames) ->
              has_all(
                efind:find(Dir, [{files, false}]),
                [{dir,Dir}])
      end).

files_and_dirs_test() ->
    with_temp_directory_containing_files(
      ["temp-1.txt","temp-2.xml"],
      fun(Dir, _Filenames) ->
              has_all(
                efind:find(Dir),
                [{dir, Dir}, {file, filename:join(Dir,"temp-1.txt")}, {file, filename:join(Dir, "temp-2.xml")}])
      end).    

files_no_dirs_test() ->
    with_temp_directory_containing_files(
      ["temp.txt"],
      fun(Dir, _Filenames) ->
              has_all(
                efind:find(Dir, [{dirs, false}]),
                [{file,filename:join(Dir, "temp.txt")}])
      end).
    
with_accept_fn_test() ->
    with_temp_directory_containing_files(
      ["temp-1.txt","temp-2.xml"],
      fun(Dir, _Filenames) ->
              has_all(
                efind:find(Dir,[{accept_fn, fun({_Type,Name}) -> filename:extension(Name) == ".xml" end}]),
                [{file, filename:join(Dir, "temp-2.xml")}])
      end).

with_accept_fn_names_only_test() ->
    with_temp_directory_containing_files(
      ["temp-1.txt","temp-2.xml"],
      fun(Dir, _Filenames) ->
              has_all(
                efind:find(Dir,[{result_type, names}, {accept_fn, fun(Name) -> filename:extension(Name) == ".xml" end}]),
                [filename:join(Dir, "temp-2.xml")])
      end).

has_all(_Actual, []) ->
    ok;
has_all(Actual, [Expected|Others]) ->
    ?assert(lists:member(Expected, Actual)),
    has_all(Actual, Others).

using_finished_scanner_test() ->
    with_empty_temp_directory(fun(Dir) ->
                                      Scanner = efind:scan(Dir,[{dirs,false}]),
                                      {finished, Scanner2} = efind:next(Scanner),
                                      ?assert(efind:finished(Scanner2)),
                                      ?assertExit(finished, efind:next(Scanner2))
                              end).

close_partially_realised_scanner_test() ->
    with_empty_temp_directory(fun(Dir) ->
                                      Scanner = efind:scan(Dir),
                                      {finished, Scanner2} = efind:close(Scanner),
                                      ?assert(efind:finished(Scanner2))
                              end).

bad_boolean_config_test() ->
    ?assertExit({kaboom, must_be_boolean}, efind:find("",[{dirs,kaboom}])).

bad_result_type_config_test() ->
    ?assertExit({kaboom, must_be_one_of, [names, basic]}, efind:find("", [{result_type, kaboom}])).

close_finished_scanner_test() ->
    with_empty_temp_directory(fun(Dir) ->
                                      Scanner = efind:scan(Dir,[{dirs,false}]),
                                      {finished, Scanner2} = efind:next(Scanner),
                                      {finished, _Scanner3} = efind:close(Scanner2)
                              end).
