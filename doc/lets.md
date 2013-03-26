

# Module lets #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)



<a name="types"></a>

## Data Types ##




### <a name="type-cont">cont()</a> ###


__abstract datatype__: `cont()`




### <a name="type-db_opts">db_opts()</a> ###



<pre><code>
db_opts() = {db, [{path, <a href="file.md#type-filename">file:filename()</a>} | create_if_missing | {create_if_missing, boolean()} | error_if_exists | {error_if_exists, boolean()} | paranoid_checks | {paranoid_checks, boolean()} | {write_buffer_size, pos_integer()} | {max_open_files, pos_integer()} | {block_cache_size, pos_integer()} | {block_size, pos_integer()} | {block_restart_interval, pos_integer()} | {filter_policy, no | {bloom, pos_integer()}}]}
</code></pre>





### <a name="type-db_read_opts">db_read_opts()</a> ###



<pre><code>
db_read_opts() = {db_read, [verify_checksums | {verify_checksums, boolean()} | fill_cache | {fill_cache, boolean()}]}
</code></pre>





### <a name="type-db_write_opts">db_write_opts()</a> ###



<pre><code>
db_write_opts() = {db_write, [sync | {sync, boolean()}]}
</code></pre>





### <a name="type-ets_opt">ets_opt()</a> ###



<pre><code>
ets_opt() = set | ordered_set | named_table | {keypos, pos_integer()} | public | protected | private | compressed | async
</code></pre>





### <a name="type-impl_opt">impl_opt()</a> ###



<pre><code>
impl_opt() = drv | nif | ets
</code></pre>





### <a name="type-item">item()</a> ###



<pre><code>
item() = owner | name | named_table | type | keypos | protection | compressed | async | memory | size
</code></pre>





### <a name="type-key">key()</a> ###



<pre><code>
key() = <a href="gen_ets_ns.md#type-key">gen_ets_ns:key()</a>
</code></pre>





### <a name="type-lets_tab">lets_tab()</a> ###



<pre><code>
lets_tab() = <a href="gen_ets_ns.md#type-gen_tab">gen_ets_ns:gen_tab()</a>
</code></pre>





### <a name="type-lets_tid">lets_tid()</a> ###



<pre><code>
lets_tid() = <a href="gen_ets_ns.md#type-gen_tid">gen_ets_ns:gen_tid()</a>
</code></pre>





### <a name="type-limit">limit()</a> ###



<pre><code>
limit() = pos_integer()
</code></pre>





### <a name="type-match">match()</a> ###



<pre><code>
match() = term()
</code></pre>





### <a name="type-match_pattern">match_pattern()</a> ###



<pre><code>
match_pattern() = <a href="gen_ets_ns.md#type-match_pattern">gen_ets_ns:match_pattern()</a>
</code></pre>





### <a name="type-match_spec">match_spec()</a> ###



<pre><code>
match_spec() = <a href="gen_ets_ns.md#type-match_spec">gen_ets_ns:match_spec()</a>
</code></pre>





### <a name="type-name">name()</a> ###



<pre><code>
name() = <a href="gen_ets_ns.md#type-gen_name">gen_ets_ns:gen_name()</a>
</code></pre>





### <a name="type-object">object()</a> ###



<pre><code>
object() = <a href="gen_ets_ns.md#type-object">gen_ets_ns:object()</a>
</code></pre>





### <a name="type-opts">opts()</a> ###



<pre><code>
opts() = [<a href="#type-ets_opt">ets_opt()</a> | <a href="#type-impl_opt">impl_opt()</a> | <a href="#type-db_opts">db_opts()</a> | <a href="#type-db_read_opts">db_read_opts()</a> | <a href="#type-db_write_opts">db_write_opts()</a>]
</code></pre>





### <a name="type-pos">pos()</a> ###



<pre><code>
pos() = pos_integer()
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-0">all/0</a></td><td><p>Returns a list of all tables at the node.</p>.</td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td><p>Deletes the entire table <code>Tab</code>.</p>.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td><p>Deletes all objects with the key <code>Key</code> from the table <code>Tab</code>.</p>.</td></tr><tr><td valign="top"><a href="#delete_all_objects-1">delete_all_objects/1</a></td><td><p>Delete all objects in the table <code>Tab</code>. The operation is
guaranteed to be atomic and isolated.  This function only applies
to the <code>ets</code> implementation.</p>.</td></tr><tr><td valign="top"><a href="#destroy-2">destroy/2</a></td><td><p>Destroy the contents of the specified table.  This function
only applies to <code>driver</code> and <code>nif</code> implementations.</p>.</td></tr><tr><td valign="top"><a href="#first-1">first/1</a></td><td><p>Returns the first key <code>Key</code> in the table <code>Tab</code>.  If the table
is empty, <code><em>$end_of_table</em></code> will be returned.</p>.</td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td><p>Fold from left to right over the elements of the table.</p>.</td></tr><tr><td valign="top"><a href="#foldr-3">foldr/3</a></td><td><p>Fold from right to left over the elements of the table.</p>.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td><p>Returns information about the table <code>Tab</code> as a list of <code>{Item,
Value}</code> tuples.</p>.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td><p>Returns the information associated with <code>Item</code> for the table <code>Tab</code>.</p>


<pre><code>Valid +Item+ options are:</code></pre>

<ul>
<li>
<p>
<code>owner</code>
</p>
</li>
<li>
<p>
<code>name</code>
</p>
</li>
<li>
<p>
<code>named_table</code> <em>only the ets implementation</em>
</p>
</li>
<li>
<p>
<code>type</code>
</p>
</li>
<li>
<p>
<code>keypos</code>
</p>
</li>
<li>
<p>
<code>protection</code>
</p>
</li>
<li>
<p>
<code>compressed</code>
</p>
</li>
<li>
<p>
<code>async</code> <em>only the drv implementation</em>
</p>
</li>
<li>
<p>
<code>memory</code> <em>only the ets implementation</em>
</p>
</li>
<li>
<p>
<code>size</code> <em>only the ets implementation</em>
</p>
</li>
</ul>.</td></tr><tr><td valign="top"><a href="#insert-2">insert/2</a></td><td><p>Inserts the object or all of the objects in the list
<code>ObjOrObjs</code> into the table <code>Tab</code>.</p>.</td></tr><tr><td valign="top"><a href="#insert_new-2">insert_new/2</a></td><td><p>This function works exactly like <code>insert/2</code>, with the
exception that instead of overwriting objects with the same key, it
simply returns false.  This function only applies to the <code>ets</code>
implementation.</p>.</td></tr><tr><td valign="top"><a href="#last-1">last/1</a></td><td><p>Returns the last key <code>Key</code> in the table <code>Tab</code>.  If the table
is empty, <code><em>$end_of_table</em></code> will be returned.</p>.</td></tr><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td><p>Returns a list of all objects with the key <code>Key</code> in the table
<code>Tab</code>.</p>.</td></tr><tr><td valign="top"><a href="#lookup_element-3">lookup_element/3</a></td><td><p>Returns the <code>Pos</code>:th element of the object with the key <code>Key</code>
in the table <code>Tab</code>.</p>.</td></tr><tr><td valign="top"><a href="#match-1">match/1</a></td><td><p>Continues a match started with <code>match/3</code>.</p>.</td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td><p>Matches the objects in the table <code>Tab</code> against the pattern
<code>Pattern</code>.</p>.</td></tr><tr><td valign="top"><a href="#match-3">match/3</a></td><td><p>Matches the objects in the table <code>Tab</code> against the pattern
<code>Pattern</code> and returns a limited (<code>Limit</code>) number of matching
objects.</p>.</td></tr><tr><td valign="top"><a href="#match_delete-2">match_delete/2</a></td><td><p>Deletes all objects which match the pattern <code>Pattern</code> from the
table <code>Tab</code>.</p>.</td></tr><tr><td valign="top"><a href="#match_object-1">match_object/1</a></td><td><p>Continues a match started with <code>match_object/3</code>.</p>.</td></tr><tr><td valign="top"><a href="#match_object-2">match_object/2</a></td><td><p>Matches the objects in the table <code>Tab</code> against the pattern
<code>Pattern</code>.</p>.</td></tr><tr><td valign="top"><a href="#match_object-3">match_object/3</a></td><td><p>Matches the objects in the table <code>Tab</code> against the pattern
<code>Pattern</code> and returns a limited (<code>Limit</code>) number of matching
objects.</p>.</td></tr><tr><td valign="top"><a href="#member-2">member/2</a></td><td><p>Returns <code>true</code> if one or more elements in the table <code>Tab</code> has
the key <code>Key</code>, <code>false</code> otherwise.</p>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td><p>Creates a new table and returns a table identifier which can
be used in subsequent operations.  The table identifier can be sent
to other processes so that a table can be shared between different
processes within a node.</p>


<pre><code>Valid LETS properties for +Options+ are:</code></pre>

<ul>
<li>
<p>
<code>set</code> The table is a set table - one key, one object, no order
among objects. This is the default table type.
</p>
</li>
<li>
<p>
<code>ordered_set</code> The table is an ordered_set table - one key, one
object, ordered in Erlang term order, which is the order implied
by the <code><</code> and <code>></code> operators.
</p>
</li>
<li>
<p>
<code>named_table</code> If this option is present, the name <code>Name</code> is
associated with the table identifier.
</p>
</li>
<li>
<p>
<code>{keypos,pos_integer()}</code> Specfies which element in the stored
tuples should be used as key. By default, it is the first
element, i.e. <code>Pos=1</code>.
</p>
</li>
<li>
<p>
<code>public</code> Any process may read or write to the table.
</p>
</li>
<li>
<p>
<code>protected</code> The owner process can read and write to the table.
Other processes can only read the table. This is the default
setting for the access rights.
</p>
</li>
<li>
<p>
<code>private</code> Only the owner process can read or write to the table.
</p>
</li>
<li>
<p>
<code>compressed</code> If this option is present, the table data will be
stored in a compressed format.
</p>
</li>
<li>
<p>
<code>async</code> If this option is present, the emulator's async thread
pool will be used when accessing the table data.<em>only the drv
implementation</em>
</p>
</li>
<li>
<p>
<code>drv</code> If this option is present, the table data will be stored
with LevelDB backend via an Erlang Driver.  This is the default
setting for the table implementation.
</p>
</li>
<li>
<p>
<code>nif</code> If this option is present, the table data will be stored
with LevelDB backend via an Erlang NIF.
</p>
</li>
<li>
<p>
<code>ets</code> If this option is present, the table data will be stored
with ETS as the backend.
</p>
</li>
<li>
<p>
<code>{db, [db_opts()]}</code> LevelDB database options.
</p>
</li>
<li>
<p>
<code>{db_read, [db_read_opts()]}</code> LevelDB read options.
</p>
</li>
<li>
<p>
<code>{db_write, [db_write_opts()]}</code> LevelDB write options.
</p>


<pre><code>Valid LevelDB database properties for +db_opts()+ are:</code></pre>

</li>
<li>
<p>
<code>{path, file:filename()}</code> Open the database with the specified
path.  The default is <code>Name</code>.
</p>
</li>
<li>
<p>
<code>create_if_missing | {create_if_missing, boolean()}</code> If <code>true</code>,
the database will be created if it is missing.  The default is
<code>false</code>.
</p>
</li>
<li>
<p>
<code>error_if_exists | {error_if_exists, boolean()}</code> If <code>true</code>, an
error is raised if the database already exists. The default is
<code>false</code>.
</p>
</li>
<li>
<p>
<code>paranoid_checks | {paranoid_checks, boolean()}</code> If <code>true</code>, the
implementation will do aggressive checking of the data it is
processing and will stop early if it detects any errors. The
default is <code>false</code>.
</p>
</li>
<li>
<p>
<code>{write_buffer_size, pos_integer()}</code> The default is 4MB.
</p>
</li>
<li>
<p>
<code>{max_open_files, pos_integer()}</code> The default is 1000.
</p>
</li>
<li>
<p>
<code>{block_cache_size, pos_integer()}</code> The default is 8MB.
</p>
</li>
<li>
<p>
<code>{block_size, pos_integer()}</code> The default is 4K.
</p>
</li>
<li>
<p>
<code>{block_restart_interval, pos_integer()}</code> The default is 16.
</p>
</li>
<li>
<p>
<code>{filter_policy, no | {bloom, pos_integer()}}</code> The default is <code>no</code>.
</p>


<pre><code>Valid LevelDB read properties for +db_read_opts()+ are:</code></pre>

</li>
<li>
<p>
<code>verify_checksums | {verify_checksums, boolean()}</code> If <code>true</code>, all
data read from underlying storage will be verified against
corresponding checksums. The default is <code>false</code>.
</p>
</li>
<li>
<p>
<code>fill_cache | {fill_cache, boolean()}</code> If <code>true</code>, the data read
should be cached in memory. The default is <code>true</code>.
</p>


<pre><code>Valid LevelDB write properties for +db_write_opts()+ are:</code></pre>

</li>
<li>
<p>
<code>sync | {sync, boolean()}</code> If <code>true</code>, the write will be flushed
from the operating system buffer cache before the write is
considered complete. The default is <code>false</code>.
</p>
</li>
</ul>.</td></tr><tr><td valign="top"><a href="#next-2">next/2</a></td><td><p>Returns the next key <code>Key2</code>, following the key <code>Key1</code> in the
table <code>Tab</code>.  If there is no next key, <code><em>$end_of_table</em></code> is
returned.</p>.</td></tr><tr><td valign="top"><a href="#prev-2">prev/2</a></td><td><p>Returns the previous key <code>Key2</code>, following the key <code>Key1</code> in
the table <code>Tab</code>.  If there is no previous key, <code><em>$end_of_table</em></code> is
returned.</p>.</td></tr><tr><td valign="top"><a href="#repair-2">repair/2</a></td><td><p>If a table cannot be opened, you may attempt to call this
method to resurrect as much of the contents of the table as
possible.  Some data may be lost, so be careful when calling this
function on a table that contains important information. This
function only applies to <code>driver</code> and <code>nif</code> implementations.</p>.</td></tr><tr><td valign="top"><a href="#select-1">select/1</a></td><td><p>Continues a select started with <code>select/3</code>.</p>.</td></tr><tr><td valign="top"><a href="#select-2">select/2</a></td><td><p>Matches the objects in the table <code>Tab</code> against the spec
<code>Spec</code>.</p>.</td></tr><tr><td valign="top"><a href="#select-3">select/3</a></td><td><p>Matches the objects in the table <code>Tab</code> against the spec <code>Spec</code>
and returns a limited (<code>Limit</code>) number of matching objects.</p>.</td></tr><tr><td valign="top"><a href="#select_count-2">select_count/2</a></td><td><p>Counts all objects which match the spec <code>Spec</code> from the
table <code>Tab</code> and returns the number matched.</p>.</td></tr><tr><td valign="top"><a href="#select_delete-2">select_delete/2</a></td><td><p>Deletes all objects which match the spec <code>Spec</code> from the
table <code>Tab</code> and returns the number deleted.</p>.</td></tr><tr><td valign="top"><a href="#select_reverse-1">select_reverse/1</a></td><td><p>Continues a select reverse started with <code>select_reverse/3</code>.</p>.</td></tr><tr><td valign="top"><a href="#select_reverse-2">select_reverse/2</a></td><td><p>Matches in reverse the objects in the table <code>Tab</code> against the
spec <code>Spec</code>.</p>.</td></tr><tr><td valign="top"><a href="#select_reverse-3">select_reverse/3</a></td><td><p>Matches in reverse the objects in the table <code>Tab</code> against the
spec <code>Spec</code> and returns a limited (<code>Limit</code>) number of matching
objects.</p>.</td></tr><tr><td valign="top"><a href="#tab2list-1">tab2list/1</a></td><td><p>Returns a list of all objects in the table <code>Tab</code>. The
operation is <strong>not</strong> guaranteed to be atomic and isolated.</p>.</td></tr><tr><td valign="top"><a href="#tid-1">tid/1</a></td><td><p>Returns a table[8217,115,32,105,100,101,110,116,105,102,105,101,114,46]</p>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all-0"></a>

### all/0 ###


<pre><code>
all() -&gt; [<a href="#type-lets_tab">lets_tab()</a>]
</code></pre>

<br></br>


<p>Returns a list of all tables at the node.</p>


__See also:__ [ets:all/0](ets.md#all-0).
<a name="delete-1"></a>

### delete/1 ###


<pre><code>
delete(Tab::<a href="#type-lets_tab">lets_tab()</a>) -&gt; true
</code></pre>

<br></br>


<p>Deletes the entire table <code>Tab</code>.</p>


__See also:__ [ets:delete/1](ets.md#delete-1).
<a name="delete-2"></a>

### delete/2 ###


<pre><code>
delete(Tab::<a href="#type-lets_tab">lets_tab()</a>, Key::<a href="#type-key">key()</a>) -&gt; true
</code></pre>

<br></br>


<p>Deletes all objects with the key <code>Key</code> from the table <code>Tab</code>.</p>


__See also:__ [ets:delete/2](ets.md#delete-2).
<a name="delete_all_objects-1"></a>

### delete_all_objects/1 ###


<pre><code>
delete_all_objects(Tab::<a href="#type-lets_tab">lets_tab()</a>) -&gt; true
</code></pre>

<br></br>


<p>Delete all objects in the table <code>Tab</code>. The operation is
guaranteed to be atomic and isolated.  This function only applies
to the <code>ets</code> implementation.</p>


__See also:__ [ets:delete_all_objects/1](ets.md#delete_all_objects-1).
<a name="destroy-2"></a>

### destroy/2 ###


<pre><code>
destroy(Name::<a href="#type-name">name()</a>, Opts::<a href="#type-opts">opts()</a>) -&gt; true
</code></pre>

<br></br>


<p>Destroy the contents of the specified table.  This function
only applies to <code>driver</code> and <code>nif</code> implementations.</p>

<a name="first-1"></a>

### first/1 ###


<pre><code>
first(Tab::<a href="#type-lets_tab">lets_tab()</a>) -&gt; <a href="#type-key">key()</a> | '$end_of_table'
</code></pre>

<br></br>


<p>Returns the first key <code>Key</code> in the table <code>Tab</code>.  If the table
is empty, <code><em>$end_of_table</em></code> will be returned.</p>


__See also:__ [ets:first/1](ets.md#first-1).
<a name="foldl-3"></a>

### foldl/3 ###


<pre><code>
foldl(Fun, Acc0::term(), Tab::<a href="#type-lets_tab">lets_tab()</a>) -&gt; Acc1::term()
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Element::term(), AccIn::term()) -&gt; AccOut::term())</code></li></ul>

<p>Fold from left to right over the elements of the table.</p>


__See also:__ [ets:foldl/3](ets.md#foldl-3).
<a name="foldr-3"></a>

### foldr/3 ###


<pre><code>
foldr(Fun, Acc0::term(), Tab::<a href="#type-lets_tab">lets_tab()</a>) -&gt; Acc1::term()
</code></pre>

<ul class="definitions"><li><code>Fun = fun((Element::term(), AccIn::term()) -&gt; AccOut::term())</code></li></ul>

<p>Fold from right to left over the elements of the table.</p>


__See also:__ [ets:foldr/3](ets.md#foldr-3).
<a name="info-1"></a>

### info/1 ###


<pre><code>
info(Tab::<a href="#type-lets_tab">lets_tab()</a>) -&gt; [{<a href="#type-item">item()</a>, term()}]
</code></pre>

<br></br>


<p>Returns information about the table <code>Tab</code> as a list of <code>{Item,
Value}</code> tuples.</p>


__See also:__ [info/2](#info-2).
<a name="info-2"></a>

### info/2 ###


<pre><code>
info(Tab::<a href="#type-lets_tab">lets_tab()</a>, Item::<a href="#type-item">item()</a>) -&gt; term()
</code></pre>

<br></br>


<p>Returns the information associated with <code>Item</code> for the table <code>Tab</code>.</p>


<pre><code>Valid +Item+ options are:</code></pre>

<ul>
<li>
<p>
<code>owner</code>
</p>
</li>
<li>
<p>
<code>name</code>
</p>
</li>
<li>
<p>
<code>named_table</code> <em>only the ets implementation</em>
</p>
</li>
<li>
<p>
<code>type</code>
</p>
</li>
<li>
<p>
<code>keypos</code>
</p>
</li>
<li>
<p>
<code>protection</code>
</p>
</li>
<li>
<p>
<code>compressed</code>
</p>
</li>
<li>
<p>
<code>async</code> <em>only the drv implementation</em>
</p>
</li>
<li>
<p>
<code>memory</code> <em>only the ets implementation</em>
</p>
</li>
<li>
<p>
<code>size</code> <em>only the ets implementation</em>
</p>
</li>
</ul>


__See also:__ [ets:info/2](ets.md#info-2).
<a name="insert-2"></a>

### insert/2 ###


<pre><code>
insert(Tab::<a href="#type-lets_tab">lets_tab()</a>, ObjOrObjs::<a href="#type-object">object()</a> | [<a href="#type-object">object()</a>]) -&gt; true
</code></pre>

<br></br>


<p>Inserts the object or all of the objects in the list
<code>ObjOrObjs</code> into the table <code>Tab</code>.</p>


__See also:__ [ets:insert/2](ets.md#insert-2).
<a name="insert_new-2"></a>

### insert_new/2 ###


<pre><code>
insert_new(Tab::<a href="#type-lets_tab">lets_tab()</a>, ObjOrObjs::<a href="#type-object">object()</a> | [<a href="#type-object">object()</a>]) -&gt; true
</code></pre>

<br></br>


<p>This function works exactly like <code>insert/2</code>, with the
exception that instead of overwriting objects with the same key, it
simply returns false.  This function only applies to the <code>ets</code>
implementation.</p>


__See also:__ [ets:insert_new/2](ets.md#insert_new-2).
<a name="last-1"></a>

### last/1 ###


<pre><code>
last(Tab::<a href="#type-lets_tab">lets_tab()</a>) -&gt; <a href="#type-key">key()</a> | '$end_of_table'
</code></pre>

<br></br>


<p>Returns the last key <code>Key</code> in the table <code>Tab</code>.  If the table
is empty, <code><em>$end_of_table</em></code> will be returned.</p>


__See also:__ [ets:last/1](ets.md#last-1).
<a name="lookup-2"></a>

### lookup/2 ###


<pre><code>
lookup(Tab::<a href="#type-lets_tab">lets_tab()</a>, Key::<a href="#type-key">key()</a>) -&gt; [<a href="#type-object">object()</a>]
</code></pre>

<br></br>


<p>Returns a list of all objects with the key <code>Key</code> in the table
<code>Tab</code>.</p>


__See also:__ [ets:lookup/2](ets.md#lookup-2).
<a name="lookup_element-3"></a>

### lookup_element/3 ###


<pre><code>
lookup_element(Tab::<a href="#type-lets_tab">lets_tab()</a>, Key::<a href="#type-key">key()</a>, Pos::<a href="#type-pos">pos()</a>) -&gt; term()
</code></pre>

<br></br>


<p>Returns the <code>Pos</code>:th element of the object with the key <code>Key</code>
in the table <code>Tab</code>.</p>


__See also:__ [ets:lookup_element/3](ets.md#lookup_element-3).
<a name="match-1"></a>

### match/1 ###


<pre><code>
match(Cont::<a href="#type-cont">cont()</a> | '$end_of_table') -&gt; {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'
</code></pre>

<br></br>


<p>Continues a match started with <code>match/3</code>.</p>


__See also:__ [ets:match/1](ets.md#match-1).
<a name="match-2"></a>

### match/2 ###


<pre><code>
match(Tab::<a href="#type-lets_tab">lets_tab()</a>, Pattern::<a href="#type-match_pattern">match_pattern()</a>) -&gt; [<a href="#type-match">match()</a>]
</code></pre>

<br></br>


<p>Matches the objects in the table <code>Tab</code> against the pattern
<code>Pattern</code>.</p>


__See also:__ [ets:match/2](ets.md#match-2).
<a name="match-3"></a>

### match/3 ###


<pre><code>
match(Tab::<a href="#type-lets_tab">lets_tab()</a>, Pattern::<a href="#type-match_pattern">match_pattern()</a>, Limit::<a href="#type-limit">limit()</a>) -&gt; {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'
</code></pre>

<br></br>


<p>Matches the objects in the table <code>Tab</code> against the pattern
<code>Pattern</code> and returns a limited (<code>Limit</code>) number of matching
objects.</p>


__See also:__ [ets:match/3](ets.md#match-3).
<a name="match_delete-2"></a>

### match_delete/2 ###


<pre><code>
match_delete(Tab::<a href="#type-lets_tab">lets_tab()</a>, Pattern::<a href="#type-match_pattern">match_pattern()</a>) -&gt; true
</code></pre>

<br></br>


<p>Deletes all objects which match the pattern <code>Pattern</code> from the
table <code>Tab</code>.</p>


__See also:__ [ets:match_delete/2](ets.md#match_delete-2).
<a name="match_object-1"></a>

### match_object/1 ###


<pre><code>
match_object(Cont::<a href="#type-cont">cont()</a> | '$end_of_table') -&gt; {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'
</code></pre>

<br></br>


<p>Continues a match started with <code>match_object/3</code>.</p>


__See also:__ [ets:match_object/1](ets.md#match_object-1).
<a name="match_object-2"></a>

### match_object/2 ###


<pre><code>
match_object(Tab::<a href="#type-lets_tab">lets_tab()</a>, Pattern::<a href="#type-match_pattern">match_pattern()</a>) -&gt; [<a href="#type-match">match()</a>]
</code></pre>

<br></br>


<p>Matches the objects in the table <code>Tab</code> against the pattern
<code>Pattern</code>.</p>


__See also:__ [ets:match_object/2](ets.md#match_object-2).
<a name="match_object-3"></a>

### match_object/3 ###


<pre><code>
match_object(Tab::<a href="#type-lets_tab">lets_tab()</a>, Pattern::<a href="#type-match_pattern">match_pattern()</a>, Limit::<a href="#type-limit">limit()</a>) -&gt; {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'
</code></pre>

<br></br>


<p>Matches the objects in the table <code>Tab</code> against the pattern
<code>Pattern</code> and returns a limited (<code>Limit</code>) number of matching
objects.</p>


__See also:__ [ets:match_object/3](ets.md#match_object-3).
<a name="member-2"></a>

### member/2 ###


<pre><code>
member(Tab::<a href="#type-lets_tab">lets_tab()</a>, Key::<a href="#type-key">key()</a>) -&gt; true | false
</code></pre>

<br></br>


<p>Returns <code>true</code> if one or more elements in the table <code>Tab</code> has
the key <code>Key</code>, <code>false</code> otherwise.</p>


__See also:__ [ets:member/2](ets.md#member-2).
<a name="new-2"></a>

### new/2 ###


<pre><code>
new(Name::<a href="#type-name">name()</a>, Opts::<a href="#type-opts">opts()</a>) -&gt; <a href="#type-lets_tab">lets_tab()</a>
</code></pre>

<br></br>


<p>Creates a new table and returns a table identifier which can
be used in subsequent operations.  The table identifier can be sent
to other processes so that a table can be shared between different
processes within a node.</p>


<pre><code>Valid LETS properties for +Options+ are:</code></pre>

<ul>
<li>
<p>
<code>set</code> The table is a set table - one key, one object, no order
among objects. This is the default table type.
</p>
</li>
<li>
<p>
<code>ordered_set</code> The table is an ordered_set table - one key, one
object, ordered in Erlang term order, which is the order implied
by the <code><</code> and <code>></code> operators.
</p>
</li>
<li>
<p>
<code>named_table</code> If this option is present, the name <code>Name</code> is
associated with the table identifier.
</p>
</li>
<li>
<p>
<code>{keypos,pos_integer()}</code> Specfies which element in the stored
tuples should be used as key. By default, it is the first
element, i.e. <code>Pos=1</code>.
</p>
</li>
<li>
<p>
<code>public</code> Any process may read or write to the table.
</p>
</li>
<li>
<p>
<code>protected</code> The owner process can read and write to the table.
Other processes can only read the table. This is the default
setting for the access rights.
</p>
</li>
<li>
<p>
<code>private</code> Only the owner process can read or write to the table.
</p>
</li>
<li>
<p>
<code>compressed</code> If this option is present, the table data will be
stored in a compressed format.
</p>
</li>
<li>
<p>
<code>async</code> If this option is present, the emulator's async thread
pool will be used when accessing the table data.<em>only the drv
implementation</em>
</p>
</li>
<li>
<p>
<code>drv</code> If this option is present, the table data will be stored
with LevelDB backend via an Erlang Driver.  This is the default
setting for the table implementation.
</p>
</li>
<li>
<p>
<code>nif</code> If this option is present, the table data will be stored
with LevelDB backend via an Erlang NIF.
</p>
</li>
<li>
<p>
<code>ets</code> If this option is present, the table data will be stored
with ETS as the backend.
</p>
</li>
<li>
<p>
<code>{db, [db_opts()]}</code> LevelDB database options.
</p>
</li>
<li>
<p>
<code>{db_read, [db_read_opts()]}</code> LevelDB read options.
</p>
</li>
<li>
<p>
<code>{db_write, [db_write_opts()]}</code> LevelDB write options.
</p>


<pre><code>Valid LevelDB database properties for +db_opts()+ are:</code></pre>

</li>
<li>
<p>
<code>{path, file:filename()}</code> Open the database with the specified
path.  The default is <code>Name</code>.
</p>
</li>
<li>
<p>
<code>create_if_missing | {create_if_missing, boolean()}</code> If <code>true</code>,
the database will be created if it is missing.  The default is
<code>false</code>.
</p>
</li>
<li>
<p>
<code>error_if_exists | {error_if_exists, boolean()}</code> If <code>true</code>, an
error is raised if the database already exists. The default is
<code>false</code>.
</p>
</li>
<li>
<p>
<code>paranoid_checks | {paranoid_checks, boolean()}</code> If <code>true</code>, the
implementation will do aggressive checking of the data it is
processing and will stop early if it detects any errors. The
default is <code>false</code>.
</p>
</li>
<li>
<p>
<code>{write_buffer_size, pos_integer()}</code> The default is 4MB.
</p>
</li>
<li>
<p>
<code>{max_open_files, pos_integer()}</code> The default is 1000.
</p>
</li>
<li>
<p>
<code>{block_cache_size, pos_integer()}</code> The default is 8MB.
</p>
</li>
<li>
<p>
<code>{block_size, pos_integer()}</code> The default is 4K.
</p>
</li>
<li>
<p>
<code>{block_restart_interval, pos_integer()}</code> The default is 16.
</p>
</li>
<li>
<p>
<code>{filter_policy, no | {bloom, pos_integer()}}</code> The default is <code>no</code>.
</p>


<pre><code>Valid LevelDB read properties for +db_read_opts()+ are:</code></pre>

</li>
<li>
<p>
<code>verify_checksums | {verify_checksums, boolean()}</code> If <code>true</code>, all
data read from underlying storage will be verified against
corresponding checksums. The default is <code>false</code>.
</p>
</li>
<li>
<p>
<code>fill_cache | {fill_cache, boolean()}</code> If <code>true</code>, the data read
should be cached in memory. The default is <code>true</code>.
</p>


<pre><code>Valid LevelDB write properties for +db_write_opts()+ are:</code></pre>

</li>
<li>
<p>
<code>sync | {sync, boolean()}</code> If <code>true</code>, the write will be flushed
from the operating system buffer cache before the write is
considered complete. The default is <code>false</code>.
</p>
</li>
</ul>


__See also:__ [ets:new/2](ets.md#new-2).
<a name="next-2"></a>

### next/2 ###


<pre><code>
next(Tab::<a href="#type-lets_tab">lets_tab()</a>, Key::<a href="#type-key">key()</a>) -&gt; <a href="#type-key">key()</a> | '$end_of_table'
</code></pre>

<br></br>


<p>Returns the next key <code>Key2</code>, following the key <code>Key1</code> in the
table <code>Tab</code>.  If there is no next key, <code><em>$end_of_table</em></code> is
returned.</p>


__See also:__ [ets:next/2](ets.md#next-2).
<a name="prev-2"></a>

### prev/2 ###


<pre><code>
prev(Tab::<a href="#type-lets_tab">lets_tab()</a>, Key::<a href="#type-key">key()</a>) -&gt; <a href="#type-key">key()</a> | '$end_of_table'
</code></pre>

<br></br>


<p>Returns the previous key <code>Key2</code>, following the key <code>Key1</code> in
the table <code>Tab</code>.  If there is no previous key, <code><em>$end_of_table</em></code> is
returned.</p>


__See also:__ [ets:prev/2](ets.md#prev-2).
<a name="repair-2"></a>

### repair/2 ###


<pre><code>
repair(Name::<a href="#type-name">name()</a>, Opts::<a href="#type-opts">opts()</a>) -&gt; true
</code></pre>

<br></br>


<p>If a table cannot be opened, you may attempt to call this
method to resurrect as much of the contents of the table as
possible.  Some data may be lost, so be careful when calling this
function on a table that contains important information. This
function only applies to <code>driver</code> and <code>nif</code> implementations.</p>

<a name="select-1"></a>

### select/1 ###


<pre><code>
select(Cont::<a href="#type-cont">cont()</a> | '$end_of_table') -&gt; {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'
</code></pre>

<br></br>


<p>Continues a select started with <code>select/3</code>.</p>


__See also:__ [ets:select/1](ets.md#select-1).
<a name="select-2"></a>

### select/2 ###


<pre><code>
select(Tab::<a href="#type-lets_tab">lets_tab()</a>, Spec::<a href="#type-match_spec">match_spec()</a>) -&gt; [<a href="#type-match">match()</a>]
</code></pre>

<br></br>


<p>Matches the objects in the table <code>Tab</code> against the spec
<code>Spec</code>.</p>


__See also:__ [ets:select/2](ets.md#select-2).
<a name="select-3"></a>

### select/3 ###


<pre><code>
select(Tab::<a href="#type-lets_tab">lets_tab()</a>, Spec::<a href="#type-match_spec">match_spec()</a>, Limit::<a href="#type-limit">limit()</a>) -&gt; {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'
</code></pre>

<br></br>


<p>Matches the objects in the table <code>Tab</code> against the spec <code>Spec</code>
and returns a limited (<code>Limit</code>) number of matching objects.</p>


__See also:__ [ets:select/3](ets.md#select-3).
<a name="select_count-2"></a>

### select_count/2 ###


<pre><code>
select_count(Tab::<a href="#type-lets_tab">lets_tab()</a>, Spec::<a href="#type-match_spec">match_spec()</a>) -&gt; pos_integer()
</code></pre>

<br></br>


<p>Counts all objects which match the spec <code>Spec</code> from the
table <code>Tab</code> and returns the number matched.</p>


__See also:__ [ets:select_count/2](ets.md#select_count-2).
<a name="select_delete-2"></a>

### select_delete/2 ###


<pre><code>
select_delete(Tab::<a href="#type-lets_tab">lets_tab()</a>, Spec::<a href="#type-match_spec">match_spec()</a>) -&gt; pos_integer()
</code></pre>

<br></br>


<p>Deletes all objects which match the spec <code>Spec</code> from the
table <code>Tab</code> and returns the number deleted.</p>


__See also:__ [ets:select_delete/2](ets.md#select_delete-2).
<a name="select_reverse-1"></a>

### select_reverse/1 ###


<pre><code>
select_reverse(Cont::<a href="#type-cont">cont()</a> | '$end_of_table') -&gt; {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'
</code></pre>

<br></br>


<p>Continues a select reverse started with <code>select_reverse/3</code>.</p>


__See also:__ [ets:select_reverse/1](ets.md#select_reverse-1).
<a name="select_reverse-2"></a>

### select_reverse/2 ###


<pre><code>
select_reverse(Tab::<a href="#type-lets_tab">lets_tab()</a>, Spec::<a href="#type-match_spec">match_spec()</a>) -&gt; [<a href="#type-match">match()</a>]
</code></pre>

<br></br>


<p>Matches in reverse the objects in the table <code>Tab</code> against the
spec <code>Spec</code>.</p>


__See also:__ [ets:select_reverse/2](ets.md#select_reverse-2).
<a name="select_reverse-3"></a>

### select_reverse/3 ###


<pre><code>
select_reverse(Tab::<a href="#type-lets_tab">lets_tab()</a>, Spec::<a href="#type-match_spec">match_spec()</a>, Limit::<a href="#type-limit">limit()</a>) -&gt; {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'
</code></pre>

<br></br>


<p>Matches in reverse the objects in the table <code>Tab</code> against the
spec <code>Spec</code> and returns a limited (<code>Limit</code>) number of matching
objects.</p>


__See also:__ [ets:select_reverse/3](ets.md#select_reverse-3).
<a name="tab2list-1"></a>

### tab2list/1 ###


<pre><code>
tab2list(Tab::<a href="#type-lets_tab">lets_tab()</a>) -&gt; [<a href="#type-object">object()</a>]
</code></pre>

<br></br>


<p>Returns a list of all objects in the table <code>Tab</code>. The
operation is <strong>not</strong> guaranteed to be atomic and isolated.</p>


__See also:__ [ets:tab2list/1](ets.md#tab2list-1).
<a name="tid-1"></a>

### tid/1 ###


<pre><code>
tid(Tab::<a href="#type-lets_tab">lets_tab()</a>) -&gt; <a href="#type-lets_tid">lets_tid()</a>
</code></pre>

<br></br>


<p>Returns a table[8217,115,32,105,100,101,110,116,105,102,105,101,114,46]</p>

