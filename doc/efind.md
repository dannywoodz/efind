

# Module efind #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

efind is a utility module in the spirit of the Unix 'find' tool.

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Danny Woods ([`dannywoodz@yahoo.co.uk`](mailto:dannywoodz@yahoo.co.uk)).

<a name="description"></a>

## Description ##

Its primary purpose is to provide a sequence of names of files and
directories, including means to filter and refine that set.

It has two main modes of operation: eager and lazy.

The eager functions, [`find/1`](#find-1) and [`find/2`](#find-2), scan the
directory tree and return the results as a fully-realised list.

The lazy functions, [`scan/1`](#scan-1) and [`scan/2`](#scan-2), create a
_scanner_ that can be fed into a call to [`next/1`](#next-1) to
yield the next match _and a new scanner_.  The important
distinction with this function is that only the current entry in
the filesystem is read, allowing for huge directory trees to be read
until some match is found without reading more than is necessary.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#find-1">find/1</a></td><td>Equivalent to <a href="#find-2"><tt>find(BaseDirectory, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td>For the given base directory, returns a list of directories and files beneath (starting with the base itself).</td></tr><tr><td valign="top"><a href="#finished-1">finished/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#next-1">next/1</a></td><td>
Takes a scanner and yields the next value from it.</td></tr><tr><td valign="top"><a href="#scan-1">scan/1</a></td><td>Equivalent to <a href="#scan-2"><tt>scan(BaseDirectory, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#scan-2">scan/2</a></td><td>Returns a scanner that starts in the given directory.</td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(Scanner::pid()) -&gt; finished
</code></pre>
<br />

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVsn, State, Other) -> any()`

<a name="find-1"></a>

### find/1 ###

<pre><code>
find(BaseDirectory::string()) -&gt; [] | [{file, string()} | {dir, string()}]
</code></pre>
<br />

Equivalent to [`find(BaseDirectory, [])`](#find-2).

<a name="find-2"></a>

### find/2 ###

<pre><code>
find(BaseDirectory::string(), Options::[tuple()]) -&gt; [] | [{file, string()} | {dir, string()}] | [string()]
</code></pre>
<br />

For the given base directory, returns a list of directories and files beneath (starting with the base itself).
Behaviour scan be adjusted by feeding appropriate configuration into Options:



<dt>{result_type,basic}</dt>




<dd>The default behaviour: the values returned are 2-tuples of the form {file, Filename} or {dir, Dirname}.</dd>




<dt>{result_type,names}</dt>




<dd>the values returned are strings.</dd>




<dt>{dirs,false}</dt>




<dd>Do not include directories in the results.  They are still descended into for files</dd>




<dt>{files,false}</dt>




<dd>Only directories are returned</dd>




<dt>{accept_fn, PredicateFn}</dt>




<dd>Called for each candidate return value.  If it evaluates to <code>false</code>, the entry is excluded.  This function is called with an argument appropriate to <code>result_type</code> (e.g. a 2-tuple, or a string if specified as <code>names</code>)</dd>



Defaults are: `[{result_type,basic},{dirs,true},{files,true},{accept_fn, fun(_) -> true end}]`

<a name="finished-1"></a>

### finished/1 ###

<pre><code>
finished(Scanner::pid()) -&gt; boolean()
</code></pre>
<br />

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, From, Scanner) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Message, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Message, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="next-1"></a>

### next/1 ###

<pre><code>
next(Scanner::pid()) -&gt; {dir, string()} | {file, string()} | string() | finished
</code></pre>
<br />

Takes a scanner and yields the next value from it.  Returns (by default), a 2-tuple,
with the first element being 'file' or 'dir', and the second being the path to the
resource.  The path alone can be returned by setting the 'result_type' option to
'names'.  When the scanner is exhausted, next/1 returned the atom 'finished'.

```erlang

    Scanner = efind:scanner(os:getenv("HOME")).
    {dir, Home}  = efind:next(Scanner).
    {Type, Name} = efind:next(Scanner).
```

<a name="scan-1"></a>

### scan/1 ###

<pre><code>
scan(Directory::string()) -&gt; pid()
</code></pre>
<br />

Equivalent to [`scan(BaseDirectory, [])`](#scan-2).

<a name="scan-2"></a>

### scan/2 ###

<pre><code>
scan(Directory::string(), Options::[tuple()]) -&gt; pid()
</code></pre>
<br />

Returns a scanner that starts in the given directory.  The same options specified in [`find/2`](#find-2)
are relevant here.

```erlang

    Scanner = efind:scan(RootDirectory).
```

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

