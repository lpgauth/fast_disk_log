

# Module fast_disk_log_utils #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#env-2">env/2</a></td><td></td></tr><tr><td valign="top"><a href="#error_msg-2">error_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-3">lookup/3</a></td><td></td></tr><tr><td valign="top"><a href="#random-1">random/1</a></td><td></td></tr><tr><td valign="top"><a href="#warning_msg-2">warning_msg/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="env-2"></a>

### env/2 ###

<pre><code>
env(Key::atom(), Default::term()) -&gt; term()
</code></pre>
<br />

<a name="error_msg-2"></a>

### error_msg/2 ###

<pre><code>
error_msg(Format::string(), Data::[term()]) -&gt; ok
</code></pre>
<br />

<a name="lookup-3"></a>

### lookup/3 ###

<pre><code>
lookup(Key::atom(), List::[{atom(), term()}], Default::term()) -&gt; term()
</code></pre>
<br />

<a name="random-1"></a>

### random/1 ###

<pre><code>
random(N::pos_integer()) -&gt; pos_integer()
</code></pre>
<br />

<a name="warning_msg-2"></a>

### warning_msg/2 ###

<pre><code>
warning_msg(Format::string(), Data::[term()]) -&gt; ok
</code></pre>
<br />

