

#Module lets#
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)





<a name="types"></a>

##Data Types##




###<a name="type-db_opts">db_opts()</a>##



<pre>db_opts() = {db, [{path, [file:filename()](file.md#type-filename)} | create_if_missing | {create_if_missing, boolean()} | error_if_exists | {error_if_exists, boolean()} | paranoid_checks | {paranoid_checks, boolean()} | {write_buffer_size, pos_integer()} | {max_open_files, pos_integer()} | {block_cache_size, pos_integer()} | {block_size, pos_integer()} | {block_restart_interval, pos_integer()}]}</pre>



###<a name="type-db_read_opts">db_read_opts()</a>##



<pre>db_read_opts() = {db_read, [verify_checksums | {verify_checksums, boolean()} | fill_cache | {fill_cache, boolean()}]}</pre>



###<a name="type-db_write_opts">db_write_opts()</a>##



<pre>db_write_opts() = {db_write, [sync | {sync, boolean()}]}</pre>



###<a name="type-ets_opt">ets_opt()</a>##



<pre>ets_opt() = set | ordered_set | named_table | {key_pos, pos_integer()} | public | protected | private | compressed | async</pre>



###<a name="type-impl_opt">impl_opt()</a>##



<pre>impl_opt() = drv | nif | ets</pre>



###<a name="type-key">key()</a>##



<pre>key() = binary()</pre>



###<a name="type-object">object()</a>##



<pre>object() = term()</pre>



###<a name="type-opts">opts()</a>##



<pre>opts() = [[ets_opt()](#type-ets_opt) | [impl_opt()](#type-impl_opt) | [db_opts()](#type-db_opts) | [db_read_opts()](#type-db_read_opts) | [db_write_opts()](#type-db_write_opts)]</pre>



###<a name="type-tab">tab()</a>##



__abstract datatype__: `tab()`
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td><p>Deletes the entire table <tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td><p>Deletes all objects with the key <tt>Key</tt> from the table <tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#delete_all_objects-1">delete_all_objects/1</a></td><td><p>Delete all objects in the table <tt>Tab</tt>. The operation is
guaranteed to be atomic and isolated.  This function only applies
to the <tt>ets</tt> implementation.</p>.</td></tr><tr><td valign="top"><a href="#destroy-2">destroy/2</a></td><td><p>Destroy the contents of the specified table.  This function
only applies to <tt>driver</tt> and <tt>nif</tt> implementations.</p>.</td></tr><tr><td valign="top"><a href="#first-1">first/1</a></td><td><p>Returns the first key <tt>Key</tt> in the table <tt>Tab</tt>.  If the table
is empty, <tt><em>$end_of_table</em></tt> will be returned.</p>.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td><p>Returns information about the table <tt>Tab</tt> as a list of <tt>{Item,
  Value}</tt> tuples.</p>


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
<tt>ObjectOrObjects</tt> into the table <tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#insert_new-2">insert_new/2</a></td><td><p>This function works exactly like <tt>insert/2</tt>, with the
exception that instead of overwriting objects with the same key, it
simply returns false.  This function only applies to the <tt>ets</tt>
implementation.</p>.</td></tr><tr><td valign="top"><a href="#lookup-2">lookup/2</a></td><td><p>Returns a list of all objects with the key <tt>Key</tt> in the table
<tt>Tab</tt>.</p>.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td><p>Creates a new table and returns a table identifier which can
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
returned.</p>.</td></tr><tr><td valign="top"><a href="#repair-2">repair/2</a></td><td><p>If a table cannot be opened, you may attempt to call this
method to resurrect as much of the contents of the table as
possible.  Some data may be lost, so be careful when calling this
function on a table that contains important information. This
function only applies to <tt>driver</tt> and <tt>nif</tt> implementations.</p>.</td></tr><tr><td valign="top"><a href="#tab2list-1">tab2list/1</a></td><td><p>Returns a list of all objects in the table <tt>Tab</tt>. The
operation is <strong>not</strong> guaranteed to be atomic and isolated.</p>.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="delete-1"></a>

###delete/1##




<pre>delete(Tab::[tab()](#type-tab)) -&gt; true</pre>
<br></br>




<p>Deletes the entire table <tt>Tab</tt>.</p>


__See also:__ [ets:delete/1](ets.md#delete-1).<a name="delete-2"></a>

###delete/2##




<pre>delete(Tab::[tab()](#type-tab), Key::[key()](#type-key)) -&gt; true</pre>
<br></br>




<p>Deletes all objects with the key <tt>Key</tt> from the table <tt>Tab</tt>.</p>


__See also:__ [ets:delete/2](ets.md#delete-2).<a name="delete_all_objects-1"></a>

###delete_all_objects/1##




<pre>delete_all_objects(Tab::[tab()](#type-tab)) -&gt; true</pre>
<br></br>




<p>Delete all objects in the table <tt>Tab</tt>. The operation is
guaranteed to be atomic and isolated.  This function only applies
to the <tt>ets</tt> implementation.</p>


__See also:__ [ets:delete_all_objects/1](ets.md#delete_all_objects-1).<a name="destroy-2"></a>

###destroy/2##




<pre>destroy(Name::atom(), Options::[opts()](#type-opts)) -&gt; true</pre>
<br></br>




<p>Destroy the contents of the specified table.  This function
only applies to <tt>driver</tt> and <tt>nif</tt> implementations.</p>
<a name="first-1"></a>

###first/1##




<pre>first(Tab::[tab()](#type-tab)) -&gt; [key()](#type-key) | '$end_of_table'</pre>
<br></br>




<p>Returns the first key <tt>Key</tt> in the table <tt>Tab</tt>.  If the table
is empty, <tt><em>$end_of_table</em></tt> will be returned.</p>


__See also:__ [ets:first/1](ets.md#first-1).<a name="info-2"></a>

###info/2##




<pre>info(Tab::[tab()](#type-tab), Item::atom()) -&gt; term()</pre>
<br></br>




<p>Returns information about the table <tt>Tab</tt> as a list of <tt>{Item,
  Value}</tt> tuples.</p>


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




<pre>insert(Tab::[tab()](#type-tab), ObjectOrObjects::[object()](#type-object) | [[object()](#type-object)]) -&gt; true</pre>
<br></br>




<p>Inserts the object or all of the objects in the list
<tt>ObjectOrObjects</tt> into the table <tt>Tab</tt>.</p>


__See also:__ [ets:insert/2](ets.md#insert-2).<a name="insert_new-2"></a>

###insert_new/2##




<pre>insert_new(Tab::[tab()](#type-tab), ObjectOrObjects::[object()](#type-object) | [[object()](#type-object)]) -&gt; true</pre>
<br></br>




<p>This function works exactly like <tt>insert/2</tt>, with the
exception that instead of overwriting objects with the same key, it
simply returns false.  This function only applies to the <tt>ets</tt>
implementation.</p>


__See also:__ [ets:insert_new/2](ets.md#insert_new-2).<a name="lookup-2"></a>

###lookup/2##




<pre>lookup(Tab::[tab()](#type-tab), Key::[key()](#type-key)) -&gt; [[object()](#type-object)]</pre>
<br></br>




<p>Returns a list of all objects with the key <tt>Key</tt> in the table
<tt>Tab</tt>.</p>


__See also:__ [ets:lookup/2](ets.md#lookup-2).<a name="new-2"></a>

###new/2##




<pre>new(Name::atom(), Options::[opts()](#type-opts)) -&gt; [tab()](#type-tab)</pre>
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




<pre>next(Tab::[tab()](#type-tab), Key::[key()](#type-key)) -&gt; [key()](#type-key) | '$end_of_table'</pre>
<br></br>




<p>Returns the next key <tt>Key2</tt>, following the key <tt>Key1</tt> in the
table <tt>Tab</tt>.  If there is no next key, <tt><em>$end_of_table</em></tt> is
returned.</p>


__See also:__ [ets:next/2](ets.md#next-2).<a name="repair-2"></a>

###repair/2##




<pre>repair(Name::atom(), Options::[opts()](#type-opts)) -&gt; true</pre>
<br></br>




<p>If a table cannot be opened, you may attempt to call this
method to resurrect as much of the contents of the table as
possible.  Some data may be lost, so be careful when calling this
function on a table that contains important information. This
function only applies to <tt>driver</tt> and <tt>nif</tt> implementations.</p>
<a name="tab2list-1"></a>

###tab2list/1##




<pre>tab2list(Tab::[tab()](#type-tab)) -&gt; [[object()](#type-object)]</pre>
<br></br>




<p>Returns a list of all objects in the table <tt>Tab</tt>. The
operation is <strong>not</strong> guaranteed to be atomic and isolated.</p>


__See also:__ [ets:tab2list/1](ets.md#tab2list-1).