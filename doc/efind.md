

# Module efind #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

efind is a utility module in the spirit of the Unix 'find' tool.

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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#find-1">find/1</a></td><td>Equivalent to <a href="#find-2"><tt>find(BaseDirectory, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#find-2">find/2</a></td><td>For the given base directory, returns a list of directories and files beneath (starting with the base itself).</td></tr><tr><td valign="top"><a href="#next-1">next/1</a></td><td>
Takes a scanner and yields the next value from it.</td></tr><tr><td valign="top"><a href="#scan-1">scan/1</a></td><td>Equivalent to <a href="#scan-2"><tt>scan(BaseDirectory, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#scan-2">scan/2</a></td><td>Returns a scanner that starts in the given directory.</td></tr></table>


<a name="functions"></a>

## Function Details ##

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

<a name="next-1"></a>

### next/1 ###

<pre><code>
next(Scanner::#scanner{}) -&gt; {{dir, string()}, #scanner{}} | {{file, string()}, #scanner{}} | {finished, #scanner{}}
</code></pre>
<br />

Takes a scanner and yields the next value from it.  Returns a 2-tuple, where
the first element is appropriate to `result_type`, and the second element is
a scanner to be used in any subsequent call to `next`.

```erlang

    Scanner = efind:scanner(os:getenv("HOME")).
    {{dir, Home}, Scanner2} = efind:next(Scanner).
    {{Type, Name}, Scanner3} = efind:next(Scanner2).
```


<strong>The scanner used in the original call should not be used afterward</strong>
.
When exhausted, yields the tuple `{finished, FinalScanner}`.  Calling `next` on that
scanner is an exit-able offense.

<a name="scan-1"></a>

### scan/1 ###

<pre><code>
scan(Directory::string()) -&gt; #scanner{}
</code></pre>
<br />

Equivalent to [`scan(BaseDirectory, [])`](#scan-2).

<a name="scan-2"></a>

### scan/2 ###

<pre><code>
scan(Directory::string(), Options::[tuple()]) -&gt; #scanner{}
</code></pre>
<br />

Returns a scanner that starts in the given directory.  The same options specified in [`find/2`](#find-2)
are relevant here.

```erlang

    Scanner = efind:scan(RootDirectory).
```

