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
%%% The lazy functions, {@link scan/1} and {@link scan/2}, cache single
%%% directories at a time and exhaust those before reading additional
%%% directories, making it suitable for working with large trees.
%%% @end

-module(efind).
-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").
-export([find/1, find/2, scan/1, scan/2, next/1, finished/1, close/1]).
-export([init/1, handle_cast/2, handle_info/2, handle_call/3, terminate/2, code_change/3]).
-record(scanner, {root                           :: string(),
                  files=true                     :: boolean(),
                  pending_files = []             :: list({file,string()}) | list(string()),
                  dirs=true                      :: boolean(),
                  pending_dirs  = []             :: list({dir,string()}) | list(string()),
                  accept_fn = fun(_) -> true end :: function(),
                  finished=false                 :: boolean(),
                  result_type=basic              :: basic | names}).

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
    collect(Scanner, []).

%% @equiv scan(BaseDirectory, [])
-spec scan(string()) -> pid().
scan(Directory) ->
    scan(Directory, []).

%% @doc Returns a scanner that starts in the given directory.  The same options specified in {@link find/2}
%% are relevant here.
%% <pre lang="erlang">
%%   Scanner = efind:scan(RootDirectory).
%% </pre>
-spec scan(string(),list(tuple())) -> pid().
scan(Directory, Options) ->
    {ok, Scanner} = gen_server:start_link(?MODULE, [#scanner{root=Directory,
                                                             dirs=dirs_opt(Options),
                                                             files=files_opt(Options),
                                                             accept_fn=accept_fn_opt(Options),
                                                             pending_dirs=[Directory],
                                                             result_type=result_type_opt(Options)}], []),
    Scanner.

%% @doc
%% Takes a scanner and yields the next value from it.  Returns (by default), a 2-tuple,
%% with the first element being 'file' or 'dir', and the second being the path to the
%% resource.  The path alone can be returned by setting the 'result_type' option to
%%  'names'.  When the scanner is exhausted, next/1 returned the atom 'finished'.
%% <pre lang="erlang">
%%   Scanner = efind:scanner(os:getenv("HOME")).
%%   {dir, Home}  = efind:next(Scanner).
%%   {Type, Name} = efind:next(Scanner).
%% </pre>

-spec next(pid()) -> {dir,string()} | {file,string()} | string() | finished.
next(Scanner) ->
    case gen_server:call(Scanner, next) of
        again -> next(Scanner);
        Result -> Result
    end.

-spec finished(pid()) -> boolean().
finished(Scanner) ->
    case catch gen_server:call(Scanner, finished) of
        {'EXIT',{noproc,_}} -> true;
        Result -> Result
    end.

-spec close(pid()) -> finished.
close(Scanner) ->
    gen_server:call(Scanner, close).

%% ============================================================================
%% private
%% ============================================================================

init([Scanner]) ->
    {ok, Scanner}.

handle_call(next, _From, #scanner{pending_dirs=[], files=false}=State) ->
    {stop, normal, finished, State};
handle_call(next, _From, #scanner{pending_files=[], pending_dirs=[]}=State) ->
    {reply, finished, State};
handle_call(next, _From, #scanner{pending_files=[], pending_dirs=[Dir|Dirs], dirs=true}=State) ->
    {Files,SubDirs} = files_and_directories_in(Dir),
    {reply, entry(dir, Dir, State), State#scanner{pending_files=Files, pending_dirs=Dirs ++ SubDirs}};
handle_call(next, _From, #scanner{pending_files=[], pending_dirs=[Dir|Dirs], dirs=false}=State) ->
    {Files,SubDirs} = files_and_directories_in(Dir),
    {reply, again, State#scanner{pending_files=Files, pending_dirs=Dirs ++ SubDirs}};
handle_call(next, _From, #scanner{pending_files=[File|Files], files=true}=State) ->
    {reply, entry(file, File, State), State#scanner{pending_files=Files}};
handle_call(next, _From, #scanner{pending_dirs=[Dir|Dirs], files=false}=State) ->
    {reply, entry(dir, Dir, State), State#scanner{pending_files=[], pending_dirs=Dirs}};
handle_call(finished, _From, #scanner{pending_files=[], pending_dirs=[]}=State) ->
    {reply, true, State};
handle_call(finished, _From, State) ->
    {reply, false, State};
handle_call(close, _From, State) ->
    {stop, normal, finished, State}.

handle_info(_Message, State) ->
    {noreply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Other) ->
    {ok, State}.

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

-spec collect(pid(), list({file,string()} | {dir,string()}) | list(string())) -> list({file,string()} | {dir,string()}) | list(string()).
collect(Scanner, Results) ->
    case next(Scanner) of
        finished -> Results;
        Result -> collect(Scanner, [Result|Results])
    end.

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

-spec entry(file | dir, string(), #scanner{}) -> string() | {file, string()} | {dir, string()}.
entry(_Type, Name, #scanner{result_type=names}) ->
    Name;
entry(Type, Name, #scanner{result_type=basic}) ->
    {Type, Name}.
