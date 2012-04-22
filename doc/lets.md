

#Module lets#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)





<a name="types"></a>

##Data Types##




###<a name="type-cont">cont()</a>##



__abstract datatype__: `cont()`



###<a name="type-db_opts">db_opts()</a>##



<pre>db_opts() = {db, [{path, <a href="file.md#type-filename">file:filename()</a>} | create_if_missing | {create_if_missing, boolean()} | error_if_exists | {error_if_exists, boolean()} | paranoid_checks | {paranoid_checks, boolean()} | {write_buffer_size, pos_integer()} | {max_open_files, pos_integer()} | {block_cache_size, pos_integer()} | {block_size, pos_integer()} | {block_restart_interval, pos_integer()} | {filter_policy, no | {bloom, pos_integer()}}]}</pre>



###<a name="type-db_read_opts">db_read_opts()</a>##



<pre>db_read_opts() = {db_read, [verify_checksums | {verify_checksums, boolean()} | fill_cache | {fill_cache, boolean()}]}</pre>



###<a name="type-db_write_opts">db_write_opts()</a>##



<pre>db_write_opts() = {db_write, [sync | {sync, boolean()}]}</pre>



###<a name="type-ets_opt">ets_opt()</a>##



<pre>ets_opt() = set | ordered_set | named_table | {key_pos, pos_integer()} | public | protected | private | compressed | async</pre>



###<a name="type-impl_opt">impl_opt()</a>##



<pre>impl_opt() = drv | nif | ets</pre>



###<a name="type-item">item()</a>##



<pre>item() = owner | name | named_table | type | keypos | protection | compressed | async | memory | size</pre>



###<a name="type-key">key()</a>##



<pre>key() = binary()</pre>



###<a name="type-limit">limit()</a>##



<pre>limit() = pos_integer()</pre>



###<a name="type-match">match()</a>##



<pre>match() = term()</pre>



###<a name="type-name">name()</a>##



<pre>name() = atom()</pre>



###<a name="type-object">object()</a>##



<pre>object() = term()</pre>



###<a name="type-opts">opts()</a>##



<pre>opts() = [<a href="#type-ets_opt">ets_opt()</a> | <a href="#type-impl_opt">impl_opt()</a> | <a href="#type-db_opts">db_opts()</a> | <a href="#type-db_read_opts">db_read_opts()</a> | <a href="#type-db_write_opts">db_write_opts()</a>]</pre>



###<a name="type-pattern">pattern()</a>##



<pre>pattern() = atom() | tuple()</pre>




<pre><tt>ets:match_pattern() is not exported!</tt></pre>




###<a name="type-pos">pos()</a>##



<pre>pos() = pos_integer()</pre>



###<a name="type-spec">spec()</a>##



<pre>spec() = <a href="ets.md#type-match_spec">ets:match_spec()</a></pre>



###<a name="type-tab">tab()</a>##



__abstract datatype__: `tab()`
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td><p>Deletes the entire table <tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td><p>Deletes all objects with the key <tt>Key</tt> from the table <tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#delete_all_objects-1">delete_all_objects/1</a></td><td><p>Delete all objects in the table <tt>Tab</tt>. The operation is
guaranteed to be atomic and isolated.  This function only applies
to the <tt>ets</tt> implementation.</p>.</td></tr><tr><td valign="top"><a href="#destroy-2">destroy/2</a></td><td><p>Destroy the contents of the specified table.  This function
only applies to <tt>driver</tt> and <tt>nif</tt> implementations.</p>.</td></tr><tr><td valign="top"><a href="#first-1">first/1</a></td><td><p>Returns the first key <tt>Key</tt> in the table <tt>Tab</tt>.  If the table
is empty, <tt><em>$end_of_table</em></tt> will be returned.</p>.</td></tr><tr><td valign="top"><a href="#foldl-3">foldl/3</a></td><td><p>Fold from left to right over the elements of the table.</p>.</td></tr><tr><td valign="top"><a href="#foldr-3">foldr/3</a></td><td><p>Fold from right to left over the elements of the table.</p>.</td></tr><tr><td valign="top"><a href="#info-1">info/1</a></td><td><p>Returns information about the table <tt>Tab</tt> as a list of <tt>{Item,
  Value}</tt> tuples.</p>.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td><p>Returns the information associated with <tt>Item</tt> for the table <tt>Tab</tt>.</p>


<pre><tt>Valid +Item+ options are:</tt></pre>

<ul>
<li>
<p>
<tt>owner</tt>
</p>
</li>
<li>
<p>
<tt>name</tt>
</p>
</li>
<li>
<p>
<tt>named_table</tt> <em>only the ets implementation</em>
</p>
</li>
<li>
<p>
<tt>type</tt>
</p>
</li>
<li>
<p>
<tt>keypos</tt>
</p>
</li>
<li>
<p>
<tt>protection</tt>
</p>
</li>
<li>
<p>
<tt>compressed</tt>
</p>
</li>
<li>
<p>
<tt>async</tt> <em>only the drv implementation</em>
</p>
</li>
<li>
<p>
<tt>memory</tt> <em>only the ets implementation</em>
</p>
</li>
<li>
<p>
<tt>size</tt> <em>only the ets implementation</em>
</p>
</li>
</ul>.</td></tr><tr><td valign="top"><a href="#insert-2">insert/2</a></td><td><p>Inserts the object or all of the objects in the list
<tt>ObjOrObjs</tt> into the table <tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#insert_new-2">insert_new/2</a></td><td><p>This function works exactly like <tt>insert/2</tt>, with the
exception that instead of overwriting objects with the same key, it
simply returns false.  This function only applies to the <tt>ets</tt>
implementation.</p>.</td></tr><tr><td valign="top"><a href="#last-1">last/1</a></td><td><p>Returns the last key <tt>Key</tt> in the table <tt>Tab</tt>.  If the table
is empty, <tt><em>$end_of_table</em></tt> will be returned.</p>.</td></tr><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td><p>Returns a list of all objects with the key <tt>Key</tt> in the table
<tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#lookup_element-3">lookup_element/3</a></td><td><p>Returns the <tt>Pos</tt>:th element of the object with the key <tt>Key</tt>
in the table <tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#match-1">match/1</a></td><td><p>Continues a match started with <tt>match/3</tt>.</p>.</td></tr><tr><td valign="top"><a href="#match-2">match/2</a></td><td><p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt>.</p>.</td></tr><tr><td valign="top"><a href="#match-3">match/3</a></td><td><p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt> and returns a limited (<tt>Limit</tt>) number of matching
objects.</p>.</td></tr><tr><td valign="top"><a href="#match_delete-2">match_delete/2</a></td><td><p>Deletes all objects which match the pattern <tt>Pattern</tt> from the
table <tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#match_object-1">match_object/1</a></td><td><p>Continues a match started with <tt>match_object/3</tt>.</p>.</td></tr><tr><td valign="top"><a href="#match_object-2">match_object/2</a></td><td><p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt>.</p>.</td></tr><tr><td valign="top"><a href="#match_object-3">match_object/3</a></td><td><p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt> and returns a limited (<tt>Limit</tt>) number of matching
objects.</p>.</td></tr><tr><td valign="top"><a href="#member-2">member/2</a></td><td><p>Returns <tt>true</tt> if one or more elements in the table <tt>Tab</tt> has
the key <tt>Key</tt>, <tt>false</tt> otherwise.</p>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td><p>Creates a new table and returns a table identifier which can
be used in subsequent operations.  The table identifier can be sent
to other processes so that a table can be shared between different
processes within a node.</p>


<pre><tt>Valid LETS properties for +Options+ are:</tt></pre>

<ul>
<li>
<p>
<tt>set</tt> The table is a set table - one key, one object, no order
among objects. This is the default table type.
</p>
</li>
<li>
<p>
<tt>ordered_set</tt> The table is an ordered_set table - one key, one
object, ordered in Erlang term order, which is the order implied
by the <tt><</tt> and <tt>></tt> operators.
</p>
</li>
<li>
<p>
<tt>named_table</tt> If this option is present, the name <tt>Name</tt> is
associated with the table identifier.  <em>only the ets
implementation</em>
</p>
</li>
<li>
<p>
<tt>{key_pos,pos_integer()}</tt> Specfies which element in the stored
tuples should be used as key. By default, it is the first
element, i.e. <tt>Pos=1</tt>.
</p>
</li>
<li>
<p>
<tt>public</tt> Any process may read or write to the table.
</p>
</li>
<li>
<p>
<tt>protected</tt> The owner process can read and write to the table.
Other processes can only read the table. This is the default
setting for the access rights.
</p>
</li>
<li>
<p>
<tt>private</tt> Only the owner process can read or write to the table.
</p>
</li>
<li>
<p>
<tt>compressed</tt> If this option is present, the table data will be
stored in a compressed format.
</p>
</li>
<li>
<p>
<tt>async</tt> If this option is present, the emulator's async thread
pool will be used when accessing the table data.  <em>only the drv
implementation</em>
</p>
</li>
<li>
<p>
<tt>drv</tt> If this option is present, the table data will be stored
with LevelDB backend via an Erlang Driver.  This is the default
setting for the table implementation.
</p>
</li>
<li>
<p>
<tt>nif</tt> If this option is present, the table data will be stored
with LevelDB backend via an Erlang NIF.
</p>
</li>
<li>
<p>
<tt>ets</tt> If this option is present, the table data will be stored
with ETS as the backend.
</p>
</li>
<li>
<p>
<tt>{db, [db_opts()]}</tt> LevelDB database options.
</p>
</li>
<li>
<p>
<tt>{db_read, [db_read_opts()]}</tt> LevelDB read options.
</p>
</li>
<li>
<p>
<tt>{db_write, [db_write_opts()]}</tt> LevelDB write options.
</p>


<pre><tt>Valid LevelDB database properties for +db_opts()+ are:</tt></pre>

</li>
<li>
<p>
<tt>{path, file:filename()}</tt> Open the database with the specified
path.  The default is <tt>Name</tt>.
</p>
</li>
<li>
<p>
<tt>create_if_missing | {create_if_missing, boolean()}</tt> If <tt>true</tt>,
the database will be created if it is missing.  The default is
<tt>false</tt>.
</p>
</li>
<li>
<p>
<tt>error_if_exists | {error_if_exists, boolean()}</tt> If <tt>true</tt>, an
error is raised if the database already exists. The default is
<tt>false</tt>.
</p>
</li>
<li>
<p>
<tt>paranoid_checks | {paranoid_checks, boolean()}</tt> If <tt>true</tt>, the
implementation will do aggressive checking of the data it is
processing and will stop early if it detects any errors. The
default is <tt>false</tt>.
</p>
</li>
<li>
<p>
<tt>{write_buffer_size, pos_integer()}</tt> The default is 4MB.
</p>
</li>
<li>
<p>
<tt>{max_open_files, pos_integer()}</tt> The default is 1000.
</p>
</li>
<li>
<p>
<tt>{block_cache_size, pos_integer()}</tt> The default is 8MB.
</p>
</li>
<li>
<p>
<tt>{block_size, pos_integer()}</tt> The default is 4K.
</p>
</li>
<li>
<p>
<tt>{block_restart_interval, pos_integer()}</tt> The default is 16.
</p>
</li>
<li>
<p>
<tt>{filter_policy, no | {bloom, pos_integer()}}</tt> The default is <tt>no</tt>.
</p>


<pre><tt>Valid LevelDB read properties for +db_read_opts()+ are:</tt></pre>

</li>
<li>
<p>
<tt>verify_checksums | {verify_checksums, boolean()}</tt> If <tt>true</tt>, all
data read from underlying storage will be verified against
corresponding checksums. The default is <tt>false</tt>.
</p>
</li>
<li>
<p>
<tt>fill_cache | {fill_cache, boolean()}</tt> If <tt>true</tt>, the data read
should be cached in memory. The default is <tt>true</tt>.
</p>


<pre><tt>Valid LevelDB write properties for +db_write_opts()+ are:</tt></pre>

</li>
<li>
<p>
<tt>sync | {sync, boolean()}</tt> If <tt>true</tt>, the write will be flushed
from the operating system buffer cache before the write is
considered complete. The default is <tt>false</tt>.
</p>
</li>
</ul>.</td></tr><tr><td valign="top"><a href="#next-2">next/2</a></td><td><p>Returns the next key <tt>Key2</tt>, following the key <tt>Key1</tt> in the
table <tt>Tab</tt>.  If there is no next key, <tt><em>$end_of_table</em></tt> is
returned.</p>.</td></tr><tr><td valign="top"><a href="#prev-2">prev/2</a></td><td><p>Returns the previous key <tt>Key2</tt>, following the key <tt>Key1</tt> in
the table <tt>Tab</tt>.  If there is no previous key, <tt><em>$end_of_table</em></tt> is
returned.</p>.</td></tr><tr><td valign="top"><a href="#repair-2">repair/2</a></td><td><p>If a table cannot be opened, you may attempt to call this
method to resurrect as much of the contents of the table as
possible.  Some data may be lost, so be careful when calling this
function on a table that contains important information. This
function only applies to <tt>driver</tt> and <tt>nif</tt> implementations.</p>.</td></tr><tr><td valign="top"><a href="#select-1">select/1</a></td><td><p>Continues a select started with <tt>select/3</tt>.</p>.</td></tr><tr><td valign="top"><a href="#select-2">select/2</a></td><td><p>Matches the objects in the table <tt>Tab</tt> against the spec
<tt>Spec</tt>.</p>.</td></tr><tr><td valign="top"><a href="#select-3">select/3</a></td><td><p>Matches the objects in the table <tt>Tab</tt> against the spec <tt>Spec</tt>
and returns a limited (<tt>Limit</tt>) number of matching objects.</p>.</td></tr><tr><td valign="top"><a href="#select_count-2">select_count/2</a></td><td><p>Counts all objects which match the spec <tt>Spec</tt> from the
table <tt>Tab</tt> and returns the number matched.</p>.</td></tr><tr><td valign="top"><a href="#select_delete-2">select_delete/2</a></td><td><p>Deletes all objects which match the spec <tt>Spec</tt> from the
table <tt>Tab</tt> and returns the number deleted.</p>.</td></tr><tr><td valign="top"><a href="#select_reverse-1">select_reverse/1</a></td><td><p>Continues a select reverse started with <tt>select_reverse/3</tt>.</p>.</td></tr><tr><td valign="top"><a href="#select_reverse-2">select_reverse/2</a></td><td><p>Matches in reverse the objects in the table <tt>Tab</tt> against the
spec <tt>Spec</tt>.</p>.</td></tr><tr><td valign="top"><a href="#select_reverse-3">select_reverse/3</a></td><td><p>Matches in reverse the objects in the table <tt>Tab</tt> against the
spec <tt>Spec</tt> and returns a limited (<tt>Limit</tt>) number of matching
objects.</p>.</td></tr><tr><td valign="top"><a href="#tab2list-1">tab2list/1</a></td><td><p>Returns a list of all objects in the table <tt>Tab</tt>. The
operation is <strong>not</strong> guaranteed to be atomic and isolated.</p>.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="delete-1"></a>

###delete/1##




<pre>delete(Tab::<a href="#type-tab">tab()</a>) -> true</pre>
<br></br>




<p>Deletes the entire table <tt>Tab</tt>.</p>


__See also:__ [ets:delete/1](ets.md#delete-1).<a name="delete-2"></a>

###delete/2##




<pre>delete(Tab::<a href="#type-tab">tab()</a>, Key::<a href="#type-key">key()</a>) -> true</pre>
<br></br>




<p>Deletes all objects with the key <tt>Key</tt> from the table <tt>Tab</tt>.</p>


__See also:__ [ets:delete/2](ets.md#delete-2).<a name="delete_all_objects-1"></a>

###delete_all_objects/1##




<pre>delete_all_objects(Tab::<a href="#type-tab">tab()</a>) -> true</pre>
<br></br>




<p>Delete all objects in the table <tt>Tab</tt>. The operation is
guaranteed to be atomic and isolated.  This function only applies
to the <tt>ets</tt> implementation.</p>


__See also:__ [ets:delete_all_objects/1](ets.md#delete_all_objects-1).<a name="destroy-2"></a>

###destroy/2##




<pre>destroy(Name::<a href="#type-name">name()</a>, Opts::<a href="#type-opts">opts()</a>) -> true</pre>
<br></br>




<p>Destroy the contents of the specified table.  This function
only applies to <tt>driver</tt> and <tt>nif</tt> implementations.</p>
<a name="first-1"></a>

###first/1##




<pre>first(Tab::<a href="#type-tab">tab()</a>) -> <a href="#type-key">key()</a> | '$end_of_table'</pre>
<br></br>




<p>Returns the first key <tt>Key</tt> in the table <tt>Tab</tt>.  If the table
is empty, <tt><em>$end_of_table</em></tt> will be returned.</p>


__See also:__ [ets:first/1](ets.md#first-1).<a name="foldl-3"></a>

###foldl/3##




<pre>foldl(Fun, Acc0::term(), Tab::<a href="#type-tab">tab()</a>) -> Acc1::term()</pre>
<ul class="definitions"><li><pre>Fun = fun((Element::term(), AccIn::term()) -&gt; AccOut::term())</pre></li></ul>



<p>Fold from left to right over the elements of the table.</p>


__See also:__ [ets:foldl/3](ets.md#foldl-3).<a name="foldr-3"></a>

###foldr/3##




<pre>foldr(Fun, Acc0::term(), Tab::<a href="#type-tab">tab()</a>) -> Acc1::term()</pre>
<ul class="definitions"><li><pre>Fun = fun((Element::term(), AccIn::term()) -&gt; AccOut::term())</pre></li></ul>



<p>Fold from right to left over the elements of the table.</p>


__See also:__ [ets:foldr/3](ets.md#foldr-3).<a name="info-1"></a>

###info/1##




<pre>info(Tab::<a href="#type-tab">tab()</a>) -> [{<a href="#type-item">item()</a>, term()}]</pre>
<br></br>




<p>Returns information about the table <tt>Tab</tt> as a list of <tt>{Item,
  Value}</tt> tuples.</p>


__See also:__ [info/2](#info-2).<a name="info-2"></a>

###info/2##




<pre>info(Tab::<a href="#type-tab">tab()</a>, Item::<a href="#type-item">item()</a>) -> term()</pre>
<br></br>




<p>Returns the information associated with <tt>Item</tt> for the table <tt>Tab</tt>.</p>


<pre><tt>Valid +Item+ options are:</tt></pre>

<ul>
<li>
<p>
<tt>owner</tt>
</p>
</li>
<li>
<p>
<tt>name</tt>
</p>
</li>
<li>
<p>
<tt>named_table</tt> <em>only the ets implementation</em>
</p>
</li>
<li>
<p>
<tt>type</tt>
</p>
</li>
<li>
<p>
<tt>keypos</tt>
</p>
</li>
<li>
<p>
<tt>protection</tt>
</p>
</li>
<li>
<p>
<tt>compressed</tt>
</p>
</li>
<li>
<p>
<tt>async</tt> <em>only the drv implementation</em>
</p>
</li>
<li>
<p>
<tt>memory</tt> <em>only the ets implementation</em>
</p>
</li>
<li>
<p>
<tt>size</tt> <em>only the ets implementation</em>
</p>
</li>
</ul>


__See also:__ [ets:info/2](ets.md#info-2).<a name="insert-2"></a>

###insert/2##




<pre>insert(Tab::<a href="#type-tab">tab()</a>, ObjOrObjs::<a href="#type-object">object()</a> | [<a href="#type-object">object()</a>]) -> true</pre>
<br></br>




<p>Inserts the object or all of the objects in the list
<tt>ObjOrObjs</tt> into the table <tt>Tab</tt>.</p>


__See also:__ [ets:insert/2](ets.md#insert-2).<a name="insert_new-2"></a>

###insert_new/2##




<pre>insert_new(Tab::<a href="#type-tab">tab()</a>, ObjOrObjs::<a href="#type-object">object()</a> | [<a href="#type-object">object()</a>]) -> true</pre>
<br></br>




<p>This function works exactly like <tt>insert/2</tt>, with the
exception that instead of overwriting objects with the same key, it
simply returns false.  This function only applies to the <tt>ets</tt>
implementation.</p>


__See also:__ [ets:insert_new/2](ets.md#insert_new-2).<a name="last-1"></a>

###last/1##




<pre>last(Tab::<a href="#type-tab">tab()</a>) -> <a href="#type-key">key()</a> | '$end_of_table'</pre>
<br></br>




<p>Returns the last key <tt>Key</tt> in the table <tt>Tab</tt>.  If the table
is empty, <tt><em>$end_of_table</em></tt> will be returned.</p>


__See also:__ [ets:last/1](ets.md#last-1).<a name="lookup-2"></a>

###lookup/2##




<pre>lookup(Tab::<a href="#type-tab">tab()</a>, Key::<a href="#type-key">key()</a>) -> [<a href="#type-object">object()</a>]</pre>
<br></br>




<p>Returns a list of all objects with the key <tt>Key</tt> in the table
<tt>Tab</tt>.</p>


__See also:__ [ets:lookup/2](ets.md#lookup-2).<a name="lookup_element-3"></a>

###lookup_element/3##




<pre>lookup_element(Tab::<a href="#type-tab">tab()</a>, Key::<a href="#type-key">key()</a>, Pos::<a href="#type-pos">pos()</a>) -> term()</pre>
<br></br>




<p>Returns the <tt>Pos</tt>:th element of the object with the key <tt>Key</tt>
in the table <tt>Tab</tt>.</p>


__See also:__ [ets:lookup_element/3](ets.md#lookup_element-3).<a name="match-1"></a>

###match/1##




<pre>match(X1::<a href="#type-cont">cont()</a> | '$end_of_table') -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>




<p>Continues a match started with <tt>match/3</tt>.</p>


__See also:__ [ets:match/1](ets.md#match-1).<a name="match-2"></a>

###match/2##




<pre>match(Tab::<a href="#type-tab">tab()</a>, Pattern::<a href="#type-pattern">pattern()</a>) -> [<a href="#type-match">match()</a>]</pre>
<br></br>




<p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt>.</p>


__See also:__ [ets:match/2](ets.md#match-2).<a name="match-3"></a>

###match/3##




<pre>match(Tab::<a href="#type-tab">tab()</a>, Pattern::<a href="#type-pattern">pattern()</a>, Limit::<a href="#type-limit">limit()</a>) -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>




<p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt> and returns a limited (<tt>Limit</tt>) number of matching
objects.</p>


__See also:__ [ets:match/3](ets.md#match-3).<a name="match_delete-2"></a>

###match_delete/2##




<pre>match_delete(Tab::<a href="#type-tab">tab()</a>, Pattern::<a href="#type-pattern">pattern()</a>) -> true</pre>
<br></br>




<p>Deletes all objects which match the pattern <tt>Pattern</tt> from the
table <tt>Tab</tt>.</p>


__See also:__ [ets:match_delete/2](ets.md#match_delete-2).<a name="match_object-1"></a>

###match_object/1##




<pre>match_object(X1::<a href="#type-cont">cont()</a> | '$end_of_table') -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>




<p>Continues a match started with <tt>match_object/3</tt>.</p>


__See also:__ [ets:match_object/1](ets.md#match_object-1).<a name="match_object-2"></a>

###match_object/2##




<pre>match_object(Tab::<a href="#type-tab">tab()</a>, Pattern::<a href="#type-pattern">pattern()</a>) -> [<a href="#type-match">match()</a>]</pre>
<br></br>




<p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt>.</p>


__See also:__ [ets:match_object/2](ets.md#match_object-2).<a name="match_object-3"></a>

###match_object/3##




<pre>match_object(Tab::<a href="#type-tab">tab()</a>, Pattern::<a href="#type-pattern">pattern()</a>, Limit::<a href="#type-limit">limit()</a>) -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>




<p>Matches the objects in the table <tt>Tab</tt> against the pattern
<tt>Pattern</tt> and returns a limited (<tt>Limit</tt>) number of matching
objects.</p>


__See also:__ [ets:match_object/3](ets.md#match_object-3).<a name="member-2"></a>

###member/2##




<pre>member(Tab::<a href="#type-tab">tab()</a>, Key::<a href="#type-key">key()</a>) -> true | false</pre>
<br></br>




<p>Returns <tt>true</tt> if one or more elements in the table <tt>Tab</tt> has
the key <tt>Key</tt>, <tt>false</tt> otherwise.</p>


__See also:__ [ets:member/2](ets.md#member-2).<a name="new-2"></a>

###new/2##




<pre>new(Name::<a href="#type-name">name()</a>, Opts::<a href="#type-opts">opts()</a>) -> <a href="#type-tab">tab()</a></pre>
<br></br>




<p>Creates a new table and returns a table identifier which can
be used in subsequent operations.  The table identifier can be sent
to other processes so that a table can be shared between different
processes within a node.</p>


<pre><tt>Valid LETS properties for +Options+ are:</tt></pre>

<ul>
<li>
<p>
<tt>set</tt> The table is a set table - one key, one object, no order
among objects. This is the default table type.
</p>
</li>
<li>
<p>
<tt>ordered_set</tt> The table is an ordered_set table - one key, one
object, ordered in Erlang term order, which is the order implied
by the <tt><</tt> and <tt>></tt> operators.
</p>
</li>
<li>
<p>
<tt>named_table</tt> If this option is present, the name <tt>Name</tt> is
associated with the table identifier.  <em>only the ets
implementation</em>
</p>
</li>
<li>
<p>
<tt>{key_pos,pos_integer()}</tt> Specfies which element in the stored
tuples should be used as key. By default, it is the first
element, i.e. <tt>Pos=1</tt>.
</p>
</li>
<li>
<p>
<tt>public</tt> Any process may read or write to the table.
</p>
</li>
<li>
<p>
<tt>protected</tt> The owner process can read and write to the table.
Other processes can only read the table. This is the default
setting for the access rights.
</p>
</li>
<li>
<p>
<tt>private</tt> Only the owner process can read or write to the table.
</p>
</li>
<li>
<p>
<tt>compressed</tt> If this option is present, the table data will be
stored in a compressed format.
</p>
</li>
<li>
<p>
<tt>async</tt> If this option is present, the emulator's async thread
pool will be used when accessing the table data.  <em>only the drv
implementation</em>
</p>
</li>
<li>
<p>
<tt>drv</tt> If this option is present, the table data will be stored
with LevelDB backend via an Erlang Driver.  This is the default
setting for the table implementation.
</p>
</li>
<li>
<p>
<tt>nif</tt> If this option is present, the table data will be stored
with LevelDB backend via an Erlang NIF.
</p>
</li>
<li>
<p>
<tt>ets</tt> If this option is present, the table data will be stored
with ETS as the backend.
</p>
</li>
<li>
<p>
<tt>{db, [db_opts()]}</tt> LevelDB database options.
</p>
</li>
<li>
<p>
<tt>{db_read, [db_read_opts()]}</tt> LevelDB read options.
</p>
</li>
<li>
<p>
<tt>{db_write, [db_write_opts()]}</tt> LevelDB write options.
</p>


<pre><tt>Valid LevelDB database properties for +db_opts()+ are:</tt></pre>

</li>
<li>
<p>
<tt>{path, file:filename()}</tt> Open the database with the specified
path.  The default is <tt>Name</tt>.
</p>
</li>
<li>
<p>
<tt>create_if_missing | {create_if_missing, boolean()}</tt> If <tt>true</tt>,
the database will be created if it is missing.  The default is
<tt>false</tt>.
</p>
</li>
<li>
<p>
<tt>error_if_exists | {error_if_exists, boolean()}</tt> If <tt>true</tt>, an
error is raised if the database already exists. The default is
<tt>false</tt>.
</p>
</li>
<li>
<p>
<tt>paranoid_checks | {paranoid_checks, boolean()}</tt> If <tt>true</tt>, the
implementation will do aggressive checking of the data it is
processing and will stop early if it detects any errors. The
default is <tt>false</tt>.
</p>
</li>
<li>
<p>
<tt>{write_buffer_size, pos_integer()}</tt> The default is 4MB.
</p>
</li>
<li>
<p>
<tt>{max_open_files, pos_integer()}</tt> The default is 1000.
</p>
</li>
<li>
<p>
<tt>{block_cache_size, pos_integer()}</tt> The default is 8MB.
</p>
</li>
<li>
<p>
<tt>{block_size, pos_integer()}</tt> The default is 4K.
</p>
</li>
<li>
<p>
<tt>{block_restart_interval, pos_integer()}</tt> The default is 16.
</p>
</li>
<li>
<p>
<tt>{filter_policy, no | {bloom, pos_integer()}}</tt> The default is <tt>no</tt>.
</p>


<pre><tt>Valid LevelDB read properties for +db_read_opts()+ are:</tt></pre>

</li>
<li>
<p>
<tt>verify_checksums | {verify_checksums, boolean()}</tt> If <tt>true</tt>, all
data read from underlying storage will be verified against
corresponding checksums. The default is <tt>false</tt>.
</p>
</li>
<li>
<p>
<tt>fill_cache | {fill_cache, boolean()}</tt> If <tt>true</tt>, the data read
should be cached in memory. The default is <tt>true</tt>.
</p>


<pre><tt>Valid LevelDB write properties for +db_write_opts()+ are:</tt></pre>

</li>
<li>
<p>
<tt>sync | {sync, boolean()}</tt> If <tt>true</tt>, the write will be flushed
from the operating system buffer cache before the write is
considered complete. The default is <tt>false</tt>.
</p>
</li>
</ul>


__See also:__ [ets:new/2](ets.md#new-2).<a name="next-2"></a>

###next/2##




<pre>next(Tab::<a href="#type-tab">tab()</a>, Key::<a href="#type-key">key()</a>) -> <a href="#type-key">key()</a> | '$end_of_table'</pre>
<br></br>




<p>Returns the next key <tt>Key2</tt>, following the key <tt>Key1</tt> in the
table <tt>Tab</tt>.  If there is no next key, <tt><em>$end_of_table</em></tt> is
returned.</p>


__See also:__ [ets:next/2](ets.md#next-2).<a name="prev-2"></a>

###prev/2##




<pre>prev(Tab::<a href="#type-tab">tab()</a>, Key::<a href="#type-key">key()</a>) -> <a href="#type-key">key()</a> | '$end_of_table'</pre>
<br></br>




<p>Returns the previous key <tt>Key2</tt>, following the key <tt>Key1</tt> in
the table <tt>Tab</tt>.  If there is no previous key, <tt><em>$end_of_table</em></tt> is
returned.</p>


__See also:__ [ets:prev/2](ets.md#prev-2).<a name="repair-2"></a>

###repair/2##




<pre>repair(Name::<a href="#type-name">name()</a>, Opts::<a href="#type-opts">opts()</a>) -> true</pre>
<br></br>




<p>If a table cannot be opened, you may attempt to call this
method to resurrect as much of the contents of the table as
possible.  Some data may be lost, so be careful when calling this
function on a table that contains important information. This
function only applies to <tt>driver</tt> and <tt>nif</tt> implementations.</p>
<a name="select-1"></a>

###select/1##




<pre>select(X1::<a href="#type-cont">cont()</a> | '$end_of_table') -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>




<p>Continues a select started with <tt>select/3</tt>.</p>


__See also:__ [ets:select/1](ets.md#select-1).<a name="select-2"></a>

###select/2##




<pre>select(Tab::<a href="#type-tab">tab()</a>, Spec::<a href="#type-spec">spec()</a>) -> [<a href="#type-match">match()</a>]</pre>
<br></br>




<p>Matches the objects in the table <tt>Tab</tt> against the spec
<tt>Spec</tt>.</p>


__See also:__ [ets:select/2](ets.md#select-2).<a name="select-3"></a>

###select/3##




<pre>select(Tab::<a href="#type-tab">tab()</a>, Spec::<a href="#type-spec">spec()</a>, Limit::<a href="#type-limit">limit()</a>) -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>




<p>Matches the objects in the table <tt>Tab</tt> against the spec <tt>Spec</tt>
and returns a limited (<tt>Limit</tt>) number of matching objects.</p>


__See also:__ [ets:select/3](ets.md#select-3).<a name="select_count-2"></a>

###select_count/2##




<pre>select_count(Tab::<a href="#type-tab">tab()</a>, Spec::<a href="#type-pattern">pattern()</a>) -> pos_integer()</pre>
<br></br>




<p>Counts all objects which match the spec <tt>Spec</tt> from the
table <tt>Tab</tt> and returns the number matched.</p>


__See also:__ [ets:select_count/2](ets.md#select_count-2).<a name="select_delete-2"></a>

###select_delete/2##




<pre>select_delete(Tab::<a href="#type-tab">tab()</a>, Spec::<a href="#type-pattern">pattern()</a>) -> pos_integer()</pre>
<br></br>




<p>Deletes all objects which match the spec <tt>Spec</tt> from the
table <tt>Tab</tt> and returns the number deleted.</p>


__See also:__ [ets:select_delete/2](ets.md#select_delete-2).<a name="select_reverse-1"></a>

###select_reverse/1##




<pre>select_reverse(X1::<a href="#type-cont">cont()</a> | '$end_of_table') -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>




<p>Continues a select reverse started with <tt>select_reverse/3</tt>.</p>


__See also:__ [ets:select_reverse/1](ets.md#select_reverse-1).<a name="select_reverse-2"></a>

###select_reverse/2##




<pre>select_reverse(Tab::<a href="#type-tab">tab()</a>, Spec::<a href="#type-spec">spec()</a>) -> [<a href="#type-match">match()</a>]</pre>
<br></br>




<p>Matches in reverse the objects in the table <tt>Tab</tt> against the
spec <tt>Spec</tt>.</p>


__See also:__ [ets:select_reverse/2](ets.md#select_reverse-2).<a name="select_reverse-3"></a>

###select_reverse/3##




<pre>select_reverse(Tab::<a href="#type-tab">tab()</a>, Spec::<a href="#type-spec">spec()</a>, Limit::<a href="#type-limit">limit()</a>) -> {[<a href="#type-match">match()</a>], <a href="#type-cont">cont()</a> | '$end_of_table'} | '$end_of_table'</pre>
<br></br>




<p>Matches in reverse the objects in the table <tt>Tab</tt> against the
spec <tt>Spec</tt> and returns a limited (<tt>Limit</tt>) number of matching
objects.</p>


__See also:__ [ets:select_reverse/3](ets.md#select_reverse-3).<a name="tab2list-1"></a>

###tab2list/1##




<pre>tab2list(Tab::<a href="#type-tab">tab()</a>) -> [<a href="#type-object">object()</a>]</pre>
<br></br>




<p>Returns a list of all objects in the table <tt>Tab</tt>. The
operation is <strong>not</strong> guaranteed to be atomic and isolated.</p>


__See also:__ [ets:tab2list/1](ets.md#tab2list-1).