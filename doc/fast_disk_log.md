

# Module fast_disk_log #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-filename">filename()</a> ###


<pre><code>
filename() = <a href="file.md#type-name_all">file:name_all()</a>
</code></pre>




### <a name="type-name">name()</a> ###


<pre><code>
name() = atom() | binary() | string()
</code></pre>




### <a name="type-open_option">open_option()</a> ###


<pre><code>
open_option() = {auto_close, boolean()} | {pool_size, pos_integer()}
</code></pre>




### <a name="type-open_options">open_options()</a> ###


<pre><code>
open_options() = [<a href="#type-open_option">open_option()</a>]
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#log-2">log/2</a></td><td></td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#open-3">open/3</a></td><td></td></tr><tr><td valign="top"><a href="#sync-1">sync/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

<pre><code>
close(Name::<a href="#type-name">name()</a>) -&gt; ok | {error, no_such_log}
</code></pre>
<br />

<a name="log-2"></a>

### log/2 ###

<pre><code>
log(Name::<a href="#type-name">name()</a>, Bin::binary()) -&gt; ok | {error, no_such_log}
</code></pre>
<br />

<a name="open-2"></a>

### open/2 ###

<pre><code>
open(Name::<a href="#type-name">name()</a>, Filename::<a href="#type-filename">filename()</a>) -&gt; ok | {error, name_already_open}
</code></pre>
<br />

<a name="open-3"></a>

### open/3 ###

<pre><code>
open(Name::<a href="#type-name">name()</a>, Filename::<a href="#type-filename">filename()</a>, Opts::<a href="#type-open_options">open_options()</a>) -&gt; ok | {error, name_already_open}
</code></pre>
<br />

<a name="sync-1"></a>

### sync/1 ###

<pre><code>
sync(Name::<a href="#type-name">name()</a>) -&gt; ok | {error, no_such_log}
</code></pre>
<br />

