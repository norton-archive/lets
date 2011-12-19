

#LETS - LevelDB-based Erlang Term Storage#


Copyright (c) 2011 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).<p>LETS is an alternative Erlang Term Storage using LevelDB as the
storage implementation.  LETS tries to address some bad properties of
ETS and DETS.  ETS is limited by physical memory.  DETS is limited by
a 2 GB file size limitation and does not implement ordered sets.  LETS
has neither of these limitations.  Data can also be automatically
compressed using the Snappy compression library.</p>
<p>For testing and comparison purposes, LETS supports three
implementations:</p>
<ul>
<li>
<p>
<tt>drv</tt> C++ Driver with LevelDB backend <em>(default)</em>
</p>
</li>
<li>
<p>
<tt>nif</tt> C++ NIF with LevelDB backend
</p>
</li>
<li>
<p>
<tt>ets</tt> Erlang ETS backend
</p>
</li>
</ul>
<p>LETS is not intended to be an exact clone of ETS.  The currently
supported ETS APIs are:</p>
<ul>
<li>
<p>
<tt>delete/1</tt>
</p>
</li>
<li>
<p>
<tt>delete/2</tt>
</p>
</li>
<li>
<p>
<tt>delete_all_objects/1</tt> <em>only ets implementation</em>
</p>
</li>
<li>
<p>
<tt>first/1</tt>
</p>
</li>
<li>
<p>
<tt>foldl/3</tt>
</p>
</li>
<li>
<p>
<tt>foldr/3</tt>
</p>
</li>
<li>
<p>
<tt>info/1</tt> <em>only a subset of items</em>
</p>
</li>
<li>
<p>
<tt>info/2</tt> <em>only a subset of items</em>
</p>
</li>
<li>
<p>
<tt>insert/2</tt>
</p>
</li>
<li>
<p>
<tt>insert_new/2</tt> <em>only ets implementation</em>
</p>
</li>
<li>
<p>
<tt>last/1</tt>
</p>
</li>
<li>
<p>
<tt>lookup/2</tt>
</p>
</li>
<li>
<p>
<tt>lookup_element/3</tt>
</p>
</li>
<li>
<p>
<tt>match/1</tt>
</p>
</li>
<li>
<p>
<tt>match/2</tt>
</p>
</li>
<li>
<p>
<tt>match/3</tt>
</p>
</li>
<li>
<p>
<tt>match_delete/2</tt>
</p>
</li>
<li>
<p>
<tt>match_object/1</tt>
</p>
</li>
<li>
<p>
<tt>match_object/2</tt>
</p>
</li>
<li>
<p>
<tt>match_object/3</tt>
</p>
</li>
<li>
<p>
<tt>member/2</tt>
</p>
</li>
<li>
<p>
<tt>new/2</tt>
</p>
</li>
<li>
<p>
<tt>next/2</tt>
</p>
</li>
<li>
<p>
<tt>prev/2</tt>
</p>
</li>
<li>
<p>
<tt>select/1</tt>
</p>
</li>
<li>
<p>
<tt>select/2</tt>
</p>
</li>
<li>
<p>
<tt>select/3</tt>
</p>
</li>
<li>
<p>
<tt>select_count/2</tt>
</p>
</li>
<li>
<p>
<tt>select_delete/2</tt>
</p>
</li>
<li>
<p>
<tt>select_reverse/1</tt>
</p>
</li>
<li>
<p>
<tt>select_reverse/2</tt>
</p>
</li>
<li>
<p>
<tt>select_reverse/3</tt>
</p>
</li>
<li>
<p>
<tt>tab2list/1</tt>
</p>
</li>
</ul>
<p><em>This repository is experimental in nature - use at your own risk and
please contribute if you find LETS useful.</em></p>

<h2 id="_quick_start_recipe">Quick Start Recipe</h2>

<p>To download and build the lets application in one shot, please follow
this recipe:</p>


<pre><tt>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone git://github.com/norton/lets.git lets
$ cd lets
$ ./rebar get-deps
$ ./rebar clean
$ ./rebar compile</tt></pre>

<p>For an alternative recipe with other "features" albeit more complex,
please read further.</p>



<h2 id="_documentation">Documentation</h2>


<h3 id="_where_should_i_start">Where should I start?</h3>
<p>This README is the only bit of documentation right now.</p>
<p>The QC (a.k.a. QuickCheck, PropEr, etc.) tests underneath the
"tests/qc" directory should be helpful for understanding the
specification and behavior of ETS and LETS.  These QC tests also
illustrate several strategies for testing Erlang Driver-based and
NIF-based implementations.</p>


<h3 id="_what_is_ets_and_dets">What is ETS and DETS?</h3>
<p>ETS and DETS are Erlang/OTP's standard library modules for Erlang
term storage.  ETS is a memory-based implementation.  DETS is a
disk-based implementation.</p>
<p>See <a href="http://www.erlang.org/doc/man/ets.html">http://www.erlang.org/doc/man/ets.html</a> and
<a href="http://www.erlang.org/doc/man/dets.html">http://www.erlang.org/doc/man/dets.html</a> for further details.</p>


<h3 id="_what_is_leveldb">What is LevelDB?</h3>
<p>LevelDB is a fast key-value storage library written at Google that
provides an ordered mapping from string keys to string values.</p>
<p>See <a href="http://code.google.com/p/leveldb/">http://code.google.com/p/leveldb/</a> for further details.</p>


<h3 id="_what_is_snappy">What is Snappy?</h3>
<p>Snappy is a fast compression/decompression library written at Google.</p>
<p>See <a href="http://code.google.com/p/snappy/">http://code.google.com/p/snappy/</a> for further details.</p>




<h2 id="_to_download">To download</h2>

<ol class="arabic">
<li>
<p>
Configure your e-mail and name for Git
</p>


<pre><tt>$ git config \--global user.email "you@example.com"
$ git config \--global user.name "Your Name"</tt></pre>

</li>
<li>
<p>
Install Repo
</p>


<pre><tt>$ mkdir -p ~/bin
$ wget -O - https://dl-ssl.google.com/dl/googlesource/git-repo/repo > ~/bin/repo
$ chmod a+x ~/bin/repo</tt></pre>

</li>
<li>
<p>
Create working directory
</p>


<pre><tt>$ mkdir working-directory-name
$ cd working-directory-name
$ repo init -u git://github.com/norton/manifests.git -m lets-default.xml</tt></pre>


<table><tr>
<td class="icon">
Note
</td>
<td class="content">Your "Git" identity is needed during the init step.  Please
enter the name and email of your GitHub account if you have one.  Team
members having read-write access are recommended to use "repo init -u
<a href="mailto:git@github.com">git@github.com</a>:norton/manifests.git -m lets-default-rw.xml".</td>
</tr></table>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">If you want to checkout the latest development version, please
append " -b dev" to the repo init command.</td>
</tr></table>

</li>
<li>
<p>
Download Git repositories
</p>


<pre><tt>$ cd working-directory-name
$ repo sync</tt></pre>

</li>
</ol>
<p>For futher information and help for related tools, please refer to the
following links:</p>
<ul>
<li>
<p>
Erlang - <a href="http://www.erlang.org/">http://www.erlang.org/</a>
</p>
<ul>
<li>
<p>
<strong>R13B04 or newer, R15B has been tested most recently</strong>
</p>
</li>
</ul>
</li>
<li>
<p>
Git - <a href="http://git-scm.com/">http://git-scm.com/</a>
</p>
<ul>
<li>
<p>
<strong>Git 1.5.4 or newer, Git 1.7.8 has been tested recently</strong>
</p>
</li>
<li>
<p>
<em>required for Repo and GitHub</em>
</p>
</li>
</ul>
</li>
<li>
<p>
GitHub - <a href="https://github.com">https://github.com</a>
</p>
</li>
<li>
<p>
Python - <a href="http://www.python.org">http://www.python.org</a>
</p>
<ul>
<li>
<p>
<strong>Python 2.4 or newer, Python 2.7.1 has been tested most recently
    (CAUTION: Python 3.x might be too new)</strong>
</p>
</li>
<li>
<p>
<em>required for Repo</em>
</p>
</li>
</ul>
</li>
<li>
<p>
Rebar - <a href="https://github.com/basho/rebar/wiki">https://github.com/basho/rebar/wiki</a>
</p>
</li>
<li>
<p>
Repo - <a href="http://source.android.com/source/git-repo.md">http://source.android.com/source/git-repo.html</a>
</p>
</li>
</ul>



<h2 id="_to_build_basic_recipe">To build - basic recipe</h2>

<ol class="arabic">
<li>
<p>
Get and install an erlang system <a href="http://www.erlang.org">http://www.erlang.org</a>
</p>
</li>
<li>
<p>
Build
</p>


<pre><tt>$ cd working-directory-name/src
$ make compile</tt></pre>

</li>
</ol>



<h2 id="_to_build_optional_features">To build - optional features</h2>

<ol class="upperalpha">
<li>
<p>
Dialyzer Testing <em>basic recipe</em>
</p>
<ol class="arabic">
<li>
<p>
Build Dialyzer's PLT <em>(required once)</em>
</p>


<pre><tt>$ cd working-directory-name/src
$ make build-plt</tt></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">Check Makefile and dialyzer's documentation for further
information.</td>
</tr></table>

</li>
<li>
<p>
Dialyze with specs
</p>


<pre><tt>$ cd working-directory-name/src
$ make dialyze</tt></pre>


<table><tr>
<td class="icon">
Caution
</td>
<td class="content">If you manually run dialyzer with the "-r" option, execute
"make clean compile" first to avoid finding duplicate beam files
underneath rebar's .eunit directory.  Check Makefile for further
information.</td>
</tr></table>

</li>
<li>
<p>
Dialyze without specs
</p>


<pre><tt>$ cd working-directory-name/src
$ make dialyze-nospec</tt></pre>

</li>
</ol>
</li>
</ol>



<h2 id="_to_test_quickcheck">To test - QuickCheck</h2>

<ol class="arabic">
<li>
<p>
Make sure QuickCheck is in your Erlang code path.  One simple way
   to accomplish this is by adding the code path to your <tt>~/.erlang</tt>
   resource file.
</p>


<pre><tt>true = code:add_pathz(os:getenv("HOME")++"/.erlang.d/lib/quviq/eqc-X.Y.Z/ebin").</tt></pre>

</li>
<li>
<p>
Compile for QuickCheck
</p>


<pre><tt>$ cd working-directory-name/src
$ make clean
$ make compile-eqc eqc-compile</tt></pre>

</li>
<li>
<p>
Run 5,000 QuickCheck tests
</p>


<pre><tt>$ cd working-directory-name/src/lib/lets/.eunit
$ erl -smp +A 5 -pz ../../sext/ebin -pz ../../qc/ebin

1> qc_statem_lets:qc_run(5000).
....
OK, passed 5000 tests

9.022% {delete,ok}
7.800% {new,ok}
4.535% {match_delete,ok}
4.491% {lookup,ok}
4.399% {select,ok}
4.352% {select_delete,ok}
4.348% {tab2list,ok}
4.341% {member,ok}
4.334% {last,ok}
4.315% {foldl,ok}
4.308% {select_reverse,ok}
4.301% {select_count,ok}
4.293% {select31,ok}
4.264% {first,ok}
4.216% {foldr,ok}
4.202% {match_object,ok}
4.184% {match,ok}
4.056% {insert,ok}
3.997% {prev,ok}
3.774% {next,ok}
3.416% {lookup_element,{error,badarg}}
1.298% {insert_new,ok}
0.757% {lookup_element,ok}
0.516% {next,{error,badarg}}
0.483% {prev,{error,badarg}}
true
.......</tt></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">For testing LevelDB directly using the C bindings, try
     <tt>qc_statemc_lets:qc_run(5000)</tt>.</td>
</tr></table>

</li>
</ol>



<h2 id="_to_test_proper">To test - PropEr</h2>

<ol class="arabic">
<li>
<p>
Make sure PropEr is in your Erlang code path.  One simple way to
   accomplish this is by adding the code path to your <tt>~/.erlang</tt>
   resource file.
</p>


<pre><tt>true = code:add_pathz(os:getenv("HOME")++"/.erlang.d/lib/proper/ebin").</tt></pre>

</li>
<li>
<p>
Compile for PropEr
</p>


<pre><tt>$ cd working-directory-name/src
$ make clean
$ make compile-proper proper-compile</tt></pre>

</li>
<li>
<p>
Run 5,000 PropEr tests
</p>


<pre><tt>$ cd working-directory-name/src/lib/lets/.eunit
$ erl -smp +A 5 -pz ../../sext/ebin -pz ../../qc/ebin

1> qc_statem_lets:qc_run(5000).
....
OK: Passed 5000 test(s).

11% {new,ok}
8% {delete,ok}
4% {member,ok}
4% {select,ok}
4% {select_count,ok}
4% {select_reverse,ok}
4% {lookup,ok}
4% {match_object,ok}
4% {tab2list,ok}
4% {last,ok}
4% {match,ok}
4% {foldl,ok}
4% {match_delete,ok}
3% {prev,ok}
3% {select31,ok}
3% {select_delete,ok}
3% {foldr,ok}
3% {insert,ok}
3% {first,ok}
3% {next,ok}
3% {lookup_element,{error,badarg}}
1% {insert_new,ok}
0% {prev,{error,badarg}}
0% {lookup_element,ok}
0% {next,{error,badarg}}
true
.......</tt></pre>

</li>
</ol>



<h2 id="_roadmap">Roadmap</h2>

<ul>
<li>
<p>
Documentation
</p>
<ul>
<li>
<p>
Explain how to run QuickCheck/PropEr tests using a new rebar
    plugin.
</p>
</li>
<li>
<p>
Explain how to build and to run lets with valgrind enabled
    OTP/Erlang virtual machine
</p>
</li>
</ul>
</li>
<li>
<p>
Testing - Black Box
</p>
<ul>
<li>
<p>
Functional
</p>
<ul>
<li>
<p>
Update test model to include LevelDB's database, read, and
       write options.  These options have not been tested.
</p>
</li>
<li>
<p>
Update test model to include LevelDB's destroy and repair
       operations.  These operations have not been tested.
</p>
</li>
</ul>
</li>
<li>
<p>
Performance (TBD)
</p>
</li>
<li>
<p>
Stability (TBD)
</p>
</li>
</ul>
</li>
<li>
<p>
Testing - White (or more like "Grey") Box
</p>
<ul>
<li>
<p>
Goals
</p>
<ul>
<li>
<p>
Test normal, abnormal, and corner test cases without having to
       actually use "big data" or invoke lots of operations.  Invoke
       operations using small inputs but with varying sizes, ranges,
       and patterns.
</p>
</li>
<li>
<p>
Learn about what special parameters exist, their default values
       and ranges, and the difficulty to control these parameters on a
       request-by-request basis (at best case).
</p>
</li>
</ul>
</li>
<li>
<p>
Functional (TBD)
</p>
<ul>
<li>
<p>
Enable/disable background compaction
</p>
</li>
<li>
<p>
Invoke/suspend manual compaction
</p>
</li>
<li>
<p>
Force new memtable creation
</p>
</li>
<li>
<p>
Force new level creation
</p>
</li>
<li>
<p>
Database Recovery (i.e. closing/reopening the db)
</p>
</li>
<li>
<p>
Large keys (e.g. 1KB)
</p>
</li>
<li>
<p>
Adjacent keys that share a long prefix (e.g ~1KB); useful
       since file format has prefix compression
</p>
</li>
<li>
<p>
Snapshots that are live across compactions and are read from
       after compaction
</p>
</li>
<li>
<p>
Iterators that are live across compactions and are read from
       after compaction
</p>
</li>
<li>
<p>
File system writes return errors (e.g., disk-full)
</p>
</li>
</ul>
</li>
</ul>
</li>
<li>
<p>
New APIs (TBD)
</p>
<ul>
<li>
<p>
<tt>insert_new/2</tt>
    (<a href="http://code.google.com/p/leveldb/issues/detail?id=42">http://code.google.com/p/leveldb/issues/detail?id=42</a>)
</p>
</li>
<li>
<p>
<tt>delete_all_objects/1</tt>
    (<a href="http://code.google.com/p/leveldb/issues/detail?id=43">http://code.google.com/p/leveldb/issues/detail?id=43</a>)
</p>
</li>
<li>
<p>
Add custom (i.e. not supported by native ETS) APIs for providing
    access to LevelDB's iterators for <tt>drv</tt> and <tt>nif</tt> backend
    implementations.
</p>
</li>
</ul>
</li>
<li>
<p>
Existing APIs (TBD)
</p>
<ul>
<li>
<p>
Performance improvement for non-existing keys
</p>
<ul>
<li>
<p>
<a href="http://erlang.org/pipermail/erlang-questions/2011-November/062755.md">http://erlang.org/pipermail/erlang-questions/2011-November/062755.html</a>
</p>
</li>
<li>
<p>
<a href="http://comments.gmane.org/gmane.comp.db.leveldb/249">http://comments.gmane.org/gmane.comp.db.leveldb/249</a>
</p>
</li>
</ul>
</li>
<li>
<p>
<tt>delete/1</tt>
    (<a href="http://code.google.com/p/leveldb/issues/detail?id=48">http://code.google.com/p/leveldb/issues/detail?id=48</a>)
</p>
</li>
<li>
<p>
<tt>new/2</tt> -
    (<a href="http://code.google.com/p/leveldb/issues/detail?id=49">http://code.google.com/p/leveldb/issues/detail?id=49</a>)
</p>
</li>
<li>
<p>
<tt>new/2</tt> - investigate if LevelDB's snapshot feature is useful (or
    not) for LETS
</p>
</li>
<li>
<p>
<tt>info/2</tt> - investigate if LevelDB's implementation can (easily)
    support size and memory info items
</p>
</li>
<li>
<p>
consider adding explicit read_options and write_options for LET's
    operations (rather than just <tt>new/2</tt>, <tt>destroy/2</tt>, and <tt>repair/2</tt>
    operations).
</p>
</li>
</ul>
</li>
</ul>




##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/norton/lets/blob/master/doc/lets.md" class="module">lets</a></td></tr>
<tr><td><a href="https://github.com/norton/lets/blob/master/doc/lets_drv.md" class="module">lets_drv</a></td></tr>
<tr><td><a href="https://github.com/norton/lets/blob/master/doc/lets_ets.md" class="module">lets_ets</a></td></tr>
<tr><td><a href="https://github.com/norton/lets/blob/master/doc/lets_nif.md" class="module">lets_nif</a></td></tr></table>

