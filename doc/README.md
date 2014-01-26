

# LETS - LevelDB-based Erlang Term Storage #

Copyright (c) 2011-2013 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).
<p>LETS is an alternative Erlang Term Storage using LevelDB as the
storage implementation.  LETS tries to address some bad properties of
ETS and DETS.  ETS is limited by physical memory.  DETS is limited by
a 2 GB file size limitation and does not implement ordered sets.  LETS
has neither of these limitations.  Data can also be automatically
compressed using the Snappy compression library.  In addition, the
name of a LETS table can be any Erlang term.</p>
<p>LETS is not intended to be an exact clone of ETS.  The currently
supported ETS APIs are:</p>
<ul>
<li>
<p>
<code>all/0</code>
</p>
</li>
<li>
<p>
<code>delete/1</code>
</p>
</li>
<li>
<p>
<code>delete/2</code>
</p>
</li>
<li>
<p>
<code>delete_all_objects/1</code> <em>only ets implementation</em>
</p>
</li>
<li>
<p>
<code>first/1</code>
</p>
</li>
<li>
<p>
<code>foldl/3</code>
</p>
</li>
<li>
<p>
<code>foldr/3</code>
</p>
</li>
<li>
<p>
<code>info/1</code> <em>only a subset of items</em>
</p>
</li>
<li>
<p>
<code>info/2</code> <em>only a subset of items</em>
</p>
</li>
<li>
<p>
<code>insert/2</code>
</p>
</li>
<li>
<p>
<code>insert_new/2</code> <em>only ets implementation</em>
</p>
</li>
<li>
<p>
<code>last/1</code>
</p>
</li>
<li>
<p>
<code>lookup/2</code>
</p>
</li>
<li>
<p>
<code>lookup_element/3</code>
</p>
</li>
<li>
<p>
<code>match/1</code>
</p>
</li>
<li>
<p>
<code>match/2</code>
</p>
</li>
<li>
<p>
<code>match/3</code>
</p>
</li>
<li>
<p>
<code>match_delete/2</code>
</p>
</li>
<li>
<p>
<code>match_object/1</code>
</p>
</li>
<li>
<p>
<code>match_object/2</code>
</p>
</li>
<li>
<p>
<code>match_object/3</code>
</p>
</li>
<li>
<p>
<code>member/2</code>
</p>
</li>
<li>
<p>
<code>new/2</code>
</p>
</li>
<li>
<p>
<code>next/2</code>
</p>
</li>
<li>
<p>
<code>prev/2</code>
</p>
</li>
<li>
<p>
<code>select/1</code>
</p>
</li>
<li>
<p>
<code>select/2</code>
</p>
</li>
<li>
<p>
<code>select/3</code>
</p>
</li>
<li>
<p>
<code>select_count/2</code>
</p>
</li>
<li>
<p>
<code>select_delete/2</code>
</p>
</li>
<li>
<p>
<code>select_reverse/1</code>
</p>
</li>
<li>
<p>
<code>select_reverse/2</code>
</p>
</li>
<li>
<p>
<code>select_reverse/3</code>
</p>
</li>
<li>
<p>
<code>tab2list/1</code>
</p>
</li>
</ul>
<p>For testing and comparison purposes, LETS supports three backend
implementations:</p>
<ul>
<li>
<p>
<code>drv</code> C++ Driver with LevelDB backend <em>(default)</em>
</p>
</li>
<li>
<p>
<code>nif</code> C++ NIF with LevelDB backend
</p>
</li>
<li>
<p>
<code>ets</code> Erlang ETS backend
</p>
</li>
</ul>
<p><em>This repository is experimental in nature - use at your own risk and
please contribute if you find LETS useful.</em></p>

<h2 id="_quick_start_recipe">Quick Start Recipe</h2>

<p>To download and build the lets application in one shot, please follow
this recipe:</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/norton/lets.git lets
$ cd lets
$ make deps clean compile</code></pre>

<p><em>OR</em> if QuickCheck is available then follow this recipe:</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/norton/lets.git lets
$ cd lets
$ make deps clean compile-for-eqc
$ (cd .qc; erl -smp +A 5 -pz ../deps/sext/ebin -pz ../deps/gen_ets/ebin -pz ../deps/qc/ebin)

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
.......</code></pre>

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


<pre><code>$ git config \--global user.email "you@example.com"
$ git config \--global user.name "Your Name"</code></pre>

</li>
<li>
<p>
Install Repo
</p>


<pre><code>$ mkdir -p ~/bin
$ wget -O - https://dl-ssl.google.com/dl/googlesource/git-repo/repo > ~/bin/repo
$ chmod a+x ~/bin/repo</code></pre>

</li>
<li>
<p>
Create working directory
</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ repo init -u https://github.com/norton/manifests.git -m lets-default.xml</code></pre>


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


<pre><code>$ cd working-directory-name
$ repo sync</code></pre>

</li>
</ol>
<p>For further information and help for related tools, please refer to the
following links:</p>
<ul>
<li>
<p>
Erlang - <a href="http://www.erlang.org/">http://www.erlang.org/</a>
</p>
<ul>
<li>
<p>
<strong>R16B or newer, R16B has been tested most recently</strong>
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
<strong>Git 1.5.4 or newer, Git 1.8.2 has been tested most recently</strong>
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
<strong>Python 2.4 or newer, Python 2.7.3 has been tested most recently
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
Rebar - <a href="https://github.com/rebar/rebar/wiki">https://github.com/rebar/rebar/wiki</a>
</p>
</li>
<li>
<p>
Repo - <a href="http://source.android.com/source/git-repo.html">http://source.android.com/source/git-repo.html</a>
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


<pre><code>$ cd working-directory-name
$ make compile</code></pre>

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


<pre><code>$ cd working-directory-name
$ make build-plt</code></pre>


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


<pre><code>$ cd working-directory-name
$ make dialyze</code></pre>


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


<pre><code>$ cd working-directory-name
$ make dialyze-nospec</code></pre>

</li>
</ol>
</li>
</ol>



<h2 id="_to_test_quickcheck">To test - QuickCheck</h2>

<ol class="arabic">
<li>
<p>
Make sure QuickCheck is in your Erlang code path.  One simple way
   to accomplish this is by adding the code path to your <code>~/.erlang</code>
   resource file.
</p>


<pre><code>true = code:add_pathz(os:getenv("HOME")++"/.erlang.d/deps/quviq/eqc-X.Y.Z/ebin").</code></pre>

</li>
<li>
<p>
Compile for QuickCheck
</p>


<pre><code>$ cd working-directory-name
$ make clean
$ make compile-for-eqc</code></pre>

</li>
<li>
<p>
Run 5,000 QuickCheck tests
</p>


<pre><code>$ cd working-directory-name/deps/lets/.qc
$ erl -smp +A 5 -pz ../../sext/ebin -pz ../../gen_ets/ebin -pz ../../qc/ebin

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
.......</code></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">For testing LevelDB directly using the C bindings, try<code>qc_statemc_lets:qc_run(5000)</code>.</td>
</tr></table>

</li>
</ol>



<h2 id="_to_test_proper">To test - PropEr</h2>

<ol class="arabic">
<li>
<p>
Make sure PropEr is in your Erlang code path.  One simple way to
   accomplish this is by adding the code path to your <code>~/.erlang</code>
   resource file.
</p>


<pre><code>true = code:add_pathz(os:getenv("HOME")++"/.erlang.d/deps/proper/ebin").</code></pre>

</li>
<li>
<p>
Compile for PropEr
</p>


<pre><code>$ cd working-directory-name
$ make clean
$ make compile-for-proper</code></pre>

</li>
<li>
<p>
Run 5,000 PropEr tests
</p>


<pre><code>$ cd working-directory-name/deps/lets/.qc
$ erl -smp +A 5 -pz ../../sext/ebin -pz ../../gen_ets/ebin -pz ../../qc/ebin

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
.......</code></pre>

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
<code>insert_new/2</code>
    (<a href="http://code.google.com/p/leveldb/issues/detail?id=42">http://code.google.com/p/leveldb/issues/detail?id=42</a>)
</p>
</li>
<li>
<p>
<code>delete_all_objects/1</code>
    (<a href="http://code.google.com/p/leveldb/issues/detail?id=43">http://code.google.com/p/leveldb/issues/detail?id=43</a>)
</p>
</li>
<li>
<p>
Add custom (i.e. not supported by native ETS) APIs for providing
    access to LevelDB's iterators for <code>drv</code> and <code>nif</code> backend
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
<code>delete/1</code>
    (<a href="http://code.google.com/p/leveldb/issues/detail?id=48">http://code.google.com/p/leveldb/issues/detail?id=48</a>)
</p>
</li>
<li>
<p>
<code>new/2</code> -
    (<a href="http://code.google.com/p/leveldb/issues/detail?id=49">http://code.google.com/p/leveldb/issues/detail?id=49</a>)
</p>
</li>
<li>
<p>
<code>new/2</code> - investigate if LevelDB's snapshot feature is useful (or
    not) for LETS
</p>
</li>
<li>
<p>
<code>info/2</code> - investigate if LevelDB's implementation can (easily)
    support size and memory info items
</p>
</li>
<li>
<p>
consider adding explicit read_options and write_options for LET's
    operations (rather than just <code>new/2</code>, <code>destroy/2</code>, and <code>repair/2</code>
    operations).
</p>
</li>
</ul>
</li>
</ul>




## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="lets.md" class="module">lets</a></td></tr>
<tr><td><a href="lets_impl_drv.md" class="module">lets_impl_drv</a></td></tr>
<tr><td><a href="lets_impl_nif.md" class="module">lets_impl_nif</a></td></tr></table>

