%%% @author Danny Woods <dannywoodz@yahoo.co.uk>
%%%
%%% @doc efind is a utility module in the spirit of the Unix 'find' tool.
%%% Its primary purpose is to provide a sequence of names of files and
%%% directories, including means to filter and refine that set.
%%%
%%% It has two main modes of operation: eager and lazy.
%%%
%%% The eager functions, {@link find/1} and {@link find/2}, scan the
%%% directory tree and return the results as a fully-realised list.
%%%
%%% The lazy functions, {@link scan/1} and {@link scan/2}, create a
%%% <em>scanner</em> that can be fed into a call to {@link next/1} to
%%% yield the next match <em>and a new scanner</em>.  The important
%%% distinction with this function is that only the current entry in
%%% the filesystem is read, allowing for huge directory trees to be read
%%% until some match is found without reading more than is necessary.
%%% @end

-module(efind).
-include("../include/efind.hrl").
-include_lib("kernel/include/file.hrl").
-export([find/1, find/2, scan/1, scan/2, next/1]).

%% ============================================================================
%% PUBLIC API
%% ============================================================================

%%% Eager Loading

%% @equiv find(BaseDirectory,[])
-spec find(string()) -> [] | list({file,string()} | {dir,string()}).
find(BaseDirectory) ->
    find(BaseDirectory, []).

%% @doc For the given base directory, returns a list of directories and files beneath (starting with the base itself).
%% Behaviour scan be adjusted by feeding appropriate configuration into Options:
%% <dl>
%%  <dt>{result_type,basic}</dt>
%%  <dd>The default behaviour: the values returned are 2-tuples of the form {file, Filename} or {dir, Dirname}.</dd>
%%  <dt>{result_type,names}</dt>
%%  <dd>the values returned are strings.</dd>
%%  <dt>{dirs,false}</dt>
%%  <dd>Do not include directories in the results.  They are still descended into for files</dd>
%%  <dt>{files,false}</dt>
%%  <dd>Only directories are returned</dd>
%%  <dt>{accept_fn, PredicateFn}</dt>
%%  <dd>Called for each candidate return value.  If it evaluates to `false', the entry is excluded.  This function is called with an argument appropriate to `result_type' (e.g. a 2-tuple, or a string if specified as `names')</dd>
%% </dl>
%% Defaults are: <code>[{result_type,basic},{dirs,true},{files,true},{accept_fn, fun(_) -> true end}]</code>
%% @end

-spec find(string(),list(tuple())) -> [] | list({file,string()} | {dir,string()}) | list(string()).
find(BaseDirectory, Options) ->
    Scanner = scan(BaseDirectory, Options),
    collect(next(Scanner), []).

%% @equiv scan(BaseDirectory, [])
-spec scan(string()) -> #scanner{}.
scan(Directory) ->
    scan(Directory, []).

%% @doc Returns a scanner that starts in the given directory.  The same options specified in {@link find/2}
%% are relevant here.
%% <pre lang="erlang">
%%   Scanner = efind:scan(RootDirectory).
%% </pre>
-spec scan(string(),list(tuple())) -> #scanner{}.
scan(Directory, Options) ->
    Scanner = #scanner{root=Directory,
                       dirs=dirs_opt(Options),
                       files=files_opt(Options),
                       accept_fn=accept_fn_opt(Options),
                       result_type=result_type_opt(Options),
                       scanner=self() % Soon-to-be-replaced
                      },
    ScannerProcess = spawn_link(fun() -> scanner([Directory], Scanner#scanner{scanner=self()}) end),
    Scanner#scanner{scanner=ScannerProcess}.

%% @doc
%% Takes a scanner and yields the next value from it.  Returns a 2-tuple, where
%% the first element is appropriate to `result_type', and the second element is
%% a scanner to be used in any subsequent call to `next'.
%% <pre lang="erlang">
%%   Scanner = efind:scanner(os:getenv("HOME")).
%%   {{dir, Home}, Scanner2} = efind:next(Scanner).
%%   {{Type, Name}, Scanner3} = efind:next(Scanner2).
%% </pre>
%% <strong>The scanner used in the original call should not be used afterward</strong>.
%% When exhausted, yields the tuple `{finished, FinalScanner}'.  Calling `next' on that
%% scanner is an exit-able offense.

-spec next(#scanner{}) -> {{dir,string()}, #scanner{}} | {{file,string()}, #scanner{}} | {finished, #scanner{}}.
next(#scanner{finished=true}) ->
    exit(finished);
next(#scanner{scanner=ScannerProcess,finished=false}=Scanner) ->
    ScannerProcess ! {self(), next},
    receive
        {ScannerProcess, finished} -> {finished, Scanner#scanner{finished=true}};
        {ScannerProcess, FSEntry} -> {FSEntry, Scanner}
    end.

%% ============================================================================
%% private
%% ============================================================================

-spec option(atom(), list(tuple()), any(), fun((any())->boolean())) -> any().
option(Option, ListOfTuples, Default, ValidatorFn) ->
    case lists:keyfind(Option, 1, ListOfTuples) of
        {Option, Value} -> ValidatorFn(Value);
        false -> Default
    end.

-spec must_be_boolean(any()) -> boolean().
must_be_boolean(Value) ->
    case is_boolean(Value) of
        true -> Value;
        false -> exit({Value, must_be_boolean})
    end.

-spec must_be_one_of(any(), list()) -> any().
must_be_one_of(Value, List) ->
    case lists:member(Value, List) of
        true -> Value;
        false -> exit({Value, must_be_one_of, List})
    end.

-spec identity(any()) -> any().
identity(V) ->
    V.

-spec dirs_opt(list(tuple())) -> boolean().
dirs_opt(ListOfTuples) ->
    option(dirs, ListOfTuples, true, fun must_be_boolean/1).
    
-spec files_opt(list(tuple())) -> boolean().
files_opt(ListOfTuples) ->
    option(files, ListOfTuples, true, fun must_be_boolean/1).
    
-spec accept_fn_opt(list(tuple())) -> fun(({dir,string()}|{file,string()})->boolean()).
accept_fn_opt(ListOfTuples) ->
    option(accept_fn, ListOfTuples, fun(_AlwaysAccept) -> true end, fun identity/1).

-spec result_type_opt(list(tuple())) -> names | basic.
result_type_opt(ListOfTuples) ->
    option(result_type, ListOfTuples, basic, fun(Value) -> must_be_one_of(Value, [names, basic]) end).

-spec collect({finished,#scanner{}} | {{file,string()},#scanner{}} | {{dir,string()},#scanner{}},
              list({file,string()} | {dir,string()})) ->
                     list({file,string()} | {dir,string()}) | list(string()).
collect({finished, _Scanner}, Results) ->
    lists:reverse(Results);
collect({Spec, Scanner}, Results) ->
    collect(next(Scanner), [Spec|Results]).

-spec finished() -> ok.
finished() ->
    block_until_asked_for_next(finished).

-spec scanner(list(string()), #scanner{}) -> ok.
scanner([], _Scanner) ->
    finished();
scanner([BaseDirectory|OtherDirectories], Scanner) ->
    offer_dir(BaseDirectory, Scanner),
    {Files, Directories} = files_and_directories_in(BaseDirectory),
    offer_files(Files, Scanner),
    scanner(OtherDirectories ++ Directories, Scanner).

-spec files_and_directories_in(string()) -> {[string()],[string()]}.
files_and_directories_in(BaseDirectory) ->
    {ok, Contents} = file:list_dir(BaseDirectory),
    PathBuilder = full_path(BaseDirectory),
    Paths = [ PathBuilder(P) || P <- Contents, string:substr(P,1,1) /= "." ],
    {Directories, Files} = lists:partition(fun is_real_directory/1, Paths),
    {Files,Directories}.

% Why not filelib:is_dir/1?  Because it considers a symlink to a directory
% to be a directory, and links back into the same tree would be Not Nice.
-spec is_real_directory(string()) -> boolean().
is_real_directory(Directory) ->
    {ok, #file_info{type=Type}} = file:read_link_info(Directory),
    Type =:= directory.

-spec full_path(string()) -> fun((string()) ->string()).
full_path(BaseDirectory) ->
    fun(Name) -> filename:join(BaseDirectory, Name) end.

-spec offer_files([string()], #scanner{}) -> ok.
offer_files([], _Scanner) ->
    ok;
offer_files([Filename|Filenames], #scanner{files=true, accept_fn=AcceptFn}=Scanner) ->
    FileEntry = entry(file, Filename, Scanner),
    case AcceptFn(FileEntry) of
        true -> block_until_asked_for_next(FileEntry);
        false -> ok
    end,
    offer_files(Filenames, Scanner);
offer_files(_Filenames, #scanner{files=false}) ->
    ok.

-spec offer_dir(string(), #scanner{}) -> ok.
offer_dir(DirectoryName, #scanner{dirs=true,accept_fn=AcceptFn}=Scanner) ->
    DirectoryEntry = entry(dir, DirectoryName, Scanner),
    case AcceptFn(DirectoryEntry) of
        true -> block_until_asked_for_next(DirectoryEntry);
        false -> ok
    end;
offer_dir(_DirectoryName, _Scanner) ->
    ok.

-spec entry(file | dir, string(), #scanner{}) -> string() | {file, string()} | {dir, string()}.
entry(_Type, Name, #scanner{result_type=names}) ->
    Name;
entry(Type, Name, #scanner{result_type=basic}) ->
    {Type, Name}.

-spec block_until_asked_for_next({file,string()} | {dir,string()} | finished) -> ok.
block_until_asked_for_next(FileSystemEntityOrFinished) ->
    receive
        {Requester,next} -> Requester ! {self(), FileSystemEntityOrFinished};
        AnythingElse -> exit({unexpected, AnythingElse})
    end,
    ok.
