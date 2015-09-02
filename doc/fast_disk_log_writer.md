

# Module fast_disk_log_writer #
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
name() = binary() | string()
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


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-5">init/5</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-4">start_link/4</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-5"></a>

### init/5 ###

<pre><code>
init(Parent::pid(), Name::atom(), Logger::<a href="#type-name">name()</a>, Filename::<a href="#type-filename">filename()</a>, Opts::<a href="#type-open_options">open_options()</a>) -&gt; ok | no_return()
</code></pre>
<br />

<a name="start_link-4"></a>

### start_link/4 ###

<pre><code>
start_link(Name::atom(), Logger::<a href="#type-name">name()</a>, Filename::<a href="#type-filename">filename()</a>, Opts::<a href="#type-open_options">open_options()</a>) -&gt; {ok, pid()}
</code></pre>
<br />

