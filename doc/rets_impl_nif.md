

# Module rets_impl_nif #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_ets_ns`](gen_ets_ns.md).

<a name="types"></a>

## Data Types ##




### <a name="type-key">key()</a> ###



<pre><code>
key() = <a href="lets.md#type-key">lets:key()</a>
</code></pre>





### <a name="type-object">object()</a> ###



<pre><code>
object() = <a href="lets.md#type-object">lets:object()</a>
</code></pre>





### <a name="type-opts">opts()</a> ###



<pre><code>
opts() = <a href="lets.md#type-opts">lets:opts()</a>
</code></pre>





### <a name="type-pos">pos()</a> ###



<pre><code>
pos() = <a href="lets.md#type-pos">lets:pos()</a>
</code></pre>





### <a name="type-tid">tid()</a> ###



<pre><code>
tid() = <a href="lets.md#type-lets_tid">lets:lets_tid()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td></td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td></td></tr><tr><td valign="top"><a href="#delete_all_objects-1">delete_all_objects/1</a></td><td></td></tr><tr><td valign="top"><a href="#destroy-2">destroy/2</a></td><td></td></tr><tr><td valign="top"><a href="#first-1">first/1</a></td><td></td></tr><tr><td valign="top"><a href="#first-2">first/2</a></td><td></td></tr><tr><td valign="top"><a href="#first_iter-1">first_iter/1</a></td><td></td></tr><tr><td valign="top"><a href="#first_iter-2">first_iter/2</a></td><td></td></tr><tr><td valign="top"><a href="#info_memory-1">info_memory/1</a></td><td></td></tr><tr><td valign="top"><a href="#info_size-1">info_size/1</a></td><td></td></tr><tr><td valign="top"><a href="#insert-2">insert/2</a></td><td></td></tr><tr><td valign="top"><a href="#insert_new-2">insert_new/2</a></td><td></td></tr><tr><td valign="top"><a href="#last-1">last/1</a></td><td></td></tr><tr><td valign="top"><a href="#last-2">last/2</a></td><td></td></tr><tr><td valign="top"><a href="#last_iter-1">last_iter/1</a></td><td></td></tr><tr><td valign="top"><a href="#last_iter-2">last_iter/2</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td></td></tr><tr><td valign="top"><a href="#lookup_element-3">lookup_element/3</a></td><td></td></tr><tr><td valign="top"><a href="#member-2">member/2</a></td><td></td></tr><tr><td valign="top"><a href="#next-2">next/2</a></td><td></td></tr><tr><td valign="top"><a href="#next-3">next/3</a></td><td></td></tr><tr><td valign="top"><a href="#next_iter-2">next_iter/2</a></td><td></td></tr><tr><td valign="top"><a href="#next_iter-3">next_iter/3</a></td><td></td></tr><tr><td valign="top"><a href="#notify-4">notify/4</a></td><td><p>Register the specified process to be sent the specified
message when the table is destroyed and return true.  Otherwise,
return false.</p>.</td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#prev-2">prev/2</a></td><td></td></tr><tr><td valign="top"><a href="#prev-3">prev/3</a></td><td></td></tr><tr><td valign="top"><a href="#prev_iter-2">prev_iter/2</a></td><td></td></tr><tr><td valign="top"><a href="#prev_iter-3">prev_iter/3</a></td><td></td></tr><tr><td valign="top"><a href="#repair-2">repair/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Gen_tid::<a href="#type-tid">tid()</a>) -&gt; true
</code></pre>
<br />

__See also:__ [lets:delete/1](lets.md#delete-1).
<a name="delete-2"></a>

### delete/2 ###


<pre><code>
delete(Gen_tid::<a href="#type-tid">tid()</a>, Key::<a href="#type-key">key()</a>) -&gt; true
</code></pre>
<br />

__See also:__ [lets:delete/2](lets.md#delete-2).
<a name="delete_all_objects-1"></a>

### delete_all_objects/1 ###


<pre><code>
delete_all_objects(Gen_tid::<a href="#type-tid">tid()</a>) -&gt; true
</code></pre>
<br />

__See also:__ [lets:delete_all_objects/1](lets.md#delete_all_objects-1).
<a name="destroy-2"></a>

### destroy/2 ###


<pre><code>
destroy(Tid::<a href="#type-tid">tid()</a>, Opts::<a href="#type-opts">opts()</a>) -&gt; true
</code></pre>
<br />

__See also:__ [lets:destroy/2](lets.md#destroy-2).
<a name="first-1"></a>

### first/1 ###


<pre><code>
first(Gen_tid::<a href="#type-tid">tid()</a>) -&gt; <a href="#type-key">key()</a> | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:first/1](lets.md#first-1).
<a name="first-2"></a>

### first/2 ###


<pre><code>
first(Gen_tid::<a href="#type-tid">tid()</a>, N::pos_integer()) -&gt; [<a href="#type-key">key()</a>] | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:first/1](lets.md#first-1).
<a name="first_iter-1"></a>

### first_iter/1 ###


<pre><code>
first_iter(Gen_tid::<a href="#type-tid">tid()</a>) -&gt; <a href="#type-object">object()</a> | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:first/1](lets.md#first-1).
<a name="first_iter-2"></a>

### first_iter/2 ###


<pre><code>
first_iter(Gen_tid::<a href="#type-tid">tid()</a>, N::pos_integer()) -&gt; [<a href="#type-object">object()</a>] | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:first/1](lets.md#first-1).
<a name="info_memory-1"></a>

### info_memory/1 ###


<pre><code>
info_memory(Gen_tid::<a href="#type-tid">tid()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

__See also:__ [lets:info/1](lets.md#info-1).
<a name="info_size-1"></a>

### info_size/1 ###


<pre><code>
info_size(Gen_tid::<a href="#type-tid">tid()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

__See also:__ [lets:info/1](lets.md#info-1).
<a name="insert-2"></a>

### insert/2 ###


<pre><code>
insert(Gen_tid::<a href="#type-tid">tid()</a>, Object::<a href="#type-object">object()</a> | [<a href="#type-object">object()</a>]) -&gt; true
</code></pre>
<br />

__See also:__ [lets:insert/2](lets.md#insert-2).
<a name="insert_new-2"></a>

### insert_new/2 ###


<pre><code>
insert_new(Gen_tid::<a href="#type-tid">tid()</a>, Object::<a href="#type-object">object()</a> | [<a href="#type-object">object()</a>]) -&gt; true
</code></pre>
<br />

__See also:__ [lets:insert_new/2](lets.md#insert_new-2).
<a name="last-1"></a>

### last/1 ###


<pre><code>
last(Gen_tid::<a href="#type-tid">tid()</a>) -&gt; <a href="#type-key">key()</a> | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:last/1](lets.md#last-1).
<a name="last-2"></a>

### last/2 ###


<pre><code>
last(Gen_tid::<a href="#type-tid">tid()</a>, N::pos_integer()) -&gt; [<a href="#type-key">key()</a>] | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:last/1](lets.md#last-1).
<a name="last_iter-1"></a>

### last_iter/1 ###


<pre><code>
last_iter(Gen_tid::<a href="#type-tid">tid()</a>) -&gt; <a href="#type-object">object()</a> | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:last/1](lets.md#last-1).
<a name="last_iter-2"></a>

### last_iter/2 ###


<pre><code>
last_iter(Gen_tid::<a href="#type-tid">tid()</a>, N::pos_integer()) -&gt; [<a href="#type-object">object()</a>] | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:last/1](lets.md#last-1).
<a name="lookup-2"></a>

### lookup/2 ###


<pre><code>
lookup(Gen_tid::<a href="#type-tid">tid()</a>, Key::<a href="#type-key">key()</a>) -&gt; [<a href="#type-object">object()</a>]
</code></pre>
<br />

__See also:__ [lets:lookup/2](lets.md#lookup-2).
<a name="lookup_element-3"></a>

### lookup_element/3 ###


<pre><code>
lookup_element(Gen_tid::<a href="#type-tid">tid()</a>, Key::<a href="#type-key">key()</a>, Pos::<a href="#type-pos">pos()</a>) -&gt; term()
</code></pre>
<br />

__See also:__ [lets:lookup_element/3](lets.md#lookup_element-3).
<a name="member-2"></a>

### member/2 ###


<pre><code>
member(Gen_tid::<a href="#type-tid">tid()</a>, Key::<a href="#type-key">key()</a>) -&gt; true | false
</code></pre>
<br />

__See also:__ [lets:member/2](lets.md#member-2).
<a name="next-2"></a>

### next/2 ###


<pre><code>
next(Gen_tid::#gen_tid{}, Key::<a href="#type-key">key()</a>) -&gt; <a href="#type-key">key()</a> | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:next/2](lets.md#next-2).
<a name="next-3"></a>

### next/3 ###


<pre><code>
next(Gen_tid::#gen_tid{}, Key::<a href="#type-key">key()</a>, N::pos_integer()) -&gt; [<a href="#type-key">key()</a>] | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:next/2](lets.md#next-2).
<a name="next_iter-2"></a>

### next_iter/2 ###


<pre><code>
next_iter(Gen_tid::<a href="#type-tid">tid()</a>, Key::<a href="#type-key">key()</a>) -&gt; <a href="#type-object">object()</a> | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:next/2](lets.md#next-2).
<a name="next_iter-3"></a>

### next_iter/3 ###


<pre><code>
next_iter(Gen_tid::<a href="#type-tid">tid()</a>, Key::<a href="#type-key">key()</a>, N::pos_integer()) -&gt; [<a href="#type-object">object()</a>] | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:next/2](lets.md#next-2).
<a name="notify-4"></a>

### notify/4 ###


<pre><code>
notify(Gen_tid::<a href="#type-tid">tid()</a>, Event::when_destroyed, Pid::pid(), Msg::term()) -&gt; true | false
</code></pre>
<br />

<p>Register the specified process to be sent the specified
message when the table is destroyed and return true.  Otherwise,
return false.</p>

<a name="open-2"></a>

### open/2 ###


<pre><code>
open(Tid::<a href="#type-tid">tid()</a>, Opts::<a href="#type-opts">opts()</a>) -&gt; <a href="#type-tid">tid()</a>
</code></pre>
<br />

__See also:__ [lets:new/2](lets.md#new-2).
<a name="prev-2"></a>

### prev/2 ###


<pre><code>
prev(Gen_tid::#gen_tid{}, Key::<a href="#type-key">key()</a>) -&gt; <a href="#type-key">key()</a> | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:prev/2](lets.md#prev-2).
<a name="prev-3"></a>

### prev/3 ###


<pre><code>
prev(Gen_tid::#gen_tid{}, Key::<a href="#type-key">key()</a>, N::pos_integer()) -&gt; [<a href="#type-key">key()</a>] | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:prev/2](lets.md#prev-2).
<a name="prev_iter-2"></a>

### prev_iter/2 ###


<pre><code>
prev_iter(Gen_tid::<a href="#type-tid">tid()</a>, Key::<a href="#type-key">key()</a>) -&gt; <a href="#type-object">object()</a> | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:prev/2](lets.md#prev-2).
<a name="prev_iter-3"></a>

### prev_iter/3 ###


<pre><code>
prev_iter(Gen_tid::<a href="#type-tid">tid()</a>, Key::<a href="#type-key">key()</a>, N::pos_integer()) -&gt; [<a href="#type-object">object()</a>] | '$end_of_table'
</code></pre>
<br />

__See also:__ [lets:prev/2](lets.md#prev-2).
<a name="repair-2"></a>

### repair/2 ###


<pre><code>
repair(Tid::<a href="#type-tid">tid()</a>, Opts::<a href="#type-opts">opts()</a>) -&gt; true
</code></pre>
<br />

__See also:__ [lets:repair/2](lets.md#repair-2).
