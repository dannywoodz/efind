-module(test_find).
-include("../include/efind.hrl").
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
                                      ?assertNot(Scanner#scanner.finished),
                                      {{dir, Dir}, Scanner2} = efind:next(Scanner),
                                      ?assertNot(Scanner2#scanner.finished),
                                      {finished, Scanner3} = efind:next(Scanner2),
                                      ?assert(Scanner3#scanner.finished),
                                      ok
                              end).

empty_dir_no_dirs_test() ->
    with_empty_temp_directory(fun(Dir) ->
                                      Scanner = efind:scan(Dir,[{dirs,false}]),
                                      ?assertNot(Scanner#scanner.finished),
                                      {finished, Scanner2} = efind:next(Scanner),
                                      ?assert(Scanner2#scanner.finished),
                                      ok
                              end).

dirs_no_files_test() ->
    with_temp_directory_containing_files(
      ["temp.txt"],
      fun(Dir, _Filenames) ->
              [{dir,Dir}] = efind:find(Dir, [{files, false}])
      end).

files_and_dirs_test() ->
    with_temp_directory_containing_files(
      ["temp-1.txt","temp-2.xml"],
      fun(Dir, _Filenames) ->
              Expected = [{dir, Dir}, {file, filename:join(Dir,"temp-1.txt")}, {file, filename:join(Dir, "temp-2.xml")}],
              Expected = efind:find(Dir)
      end).    

files_no_dirs_test() ->
    with_temp_directory_containing_files(
      ["temp.txt"],
      fun(Dir, _Filenames) ->
              Expected = [{file,filename:join(Dir, "temp.txt")}],
              Expected = efind:find(Dir, [{dirs, false}])
      end).
    
with_accept_fn_test() ->
    with_temp_directory_containing_files(
      ["temp-1.txt","temp-2.xml"],
      fun(Dir, _Filenames) ->
              Expected = [{file, filename:join(Dir, "temp-2.xml")}],
              Expected = efind:find(Dir,[{accept_fn, fun({_Type,Name}) -> filename:extension(Name) == ".xml" end}])
      end).

with_accept_fn_names_only_test() ->
    with_temp_directory_containing_files(
      ["temp-1.txt","temp-2.xml"],
      fun(Dir, _Filenames) ->
              Expected = [filename:join(Dir, "temp-2.xml")],
              Expected = efind:find(Dir,[{result_type, names}, {accept_fn, fun(Name) -> filename:extension(Name) == ".xml" end}])
      end).

using_finished_scanner_test() ->
    with_empty_temp_directory(fun(Dir) ->
                                      Scanner = efind:scan(Dir,[{dirs,false}]),
                                      {finished, #scanner{finished=true}=Scanner2} = efind:next(Scanner),
                                      ?assertExit(finished, efind:next(Scanner2))
                              end).

bad_boolean_config_test() ->
    ?assertExit({kaboom, must_be_boolean}, efind:find("",[{dirs,kaboom}])).

bad_result_type_config_test() ->
    ?assertExit({kaboom, must_be_one_of, [names, basic]}, efind:find("", [{result_type, kaboom}])).
