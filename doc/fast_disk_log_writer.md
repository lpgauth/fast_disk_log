

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

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-3">init/3</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-2">start_link/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-3"></a>

### init/3 ###

<pre><code>
init(Parent::pid(), Name::atom(), Filename::<a href="#type-filename">filename()</a>) -&gt; ok | no_return()
</code></pre>
<br />

<a name="start_link-2"></a>

### start_link/2 ###

<pre><code>
start_link(Name::atom(), Filename::<a href="#type-filename">filename()</a>) -&gt; {ok, pid()}
</code></pre>
<br />

