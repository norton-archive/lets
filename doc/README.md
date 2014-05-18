

# LETS - LevelDB-based Erlang Term Storage #

Copyright (c) 2011-2014 by Joseph Wayne Norton

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
<p>For testing and comparison purposes, LETS supports five backend
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
<code>hyper drv</code> C++ Driver with HyperLevelDB backend
</p>
</li>
<li>
<p>
<code>hyper nif</code> C++ NIF with HyperLevelDB backend
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
$ (cd .qc; erl -smp +A 5 -pz -pz ../deps/{sext,gen_ets,qc}/ebin)

1> qc_statem_lets:qc_run(500).
.......
OK, passed 500 tests

100.0% {1,attempts}

3.79% {{undefined,[]},{new,2},ok}
1.29% {{drv,[]},{match_delete,2},ok}
1.25% {{ets,[]},{select_delete,2},ok}
1.23% {{drv,[]},{select_count,2},ok}
1.22% {{drv,[]},{match,2},ok}
1.22% {{drv,[]},{match_object,2},ok}
1.22% {{drv,[]},{lookup,2},ok}
1.21% {{drv,[]},{select_delete,2},ok}
1.21% {{drv,[]},{delete,1},ok}
1.19% {{drv,[]},{last,1},ok}
1.18% {{drv,[]},{insert,2},ok}
1.18% {{drv,[]},{delete,2},ok}
1.18% {{ets,[]},{select,2},ok}
1.18% {{drv,[]},{foldr,3},ok}
1.17% {{ets,[]},{tab2list,1},ok}
1.17% {{drv,[]},{new,3},ok}
1.17% {{drv,[]},{foldl,3},ok}
1.16% {{ets,[]},{select_reverse31,3},ok}
1.14% {{drv,[]},{match31,3},ok}
1.13% {{ets,[]},{insert,2},ok}
1.13% {{drv,[]},{all,1},ok}
1.12% {{ets,[]},{select_count,2},ok}
1.12% {{ets,[]},{match_object,2},ok}
1.12% {{ets,[]},{match_delete,2},ok}
1.11% {{drv,[]},{tab2list,1},ok}
1.11% {{drv,[]},{select,2},ok}
1.11% {{drv,[]},{member,2},ok}
1.10% {{ets,[]},{match31,3},ok}
1.10% {{ets,[]},{first,1},ok}
1.10% {{drv,[]},{select_reverse31,3},ok}
1.10% {{drv,[]},{next,2},ok}
1.10% {{drv,[]},{match_object31,3},ok}
1.09% {{nif,[]},{match_object,2},ok}
1.09% {{drv,[]},{select31,3},ok}
1.09% {{ets,[]},{match_object31,3},ok}
1.09% {{ets,[]},{lookup,2},ok}
1.08% {{ets,[]},{select31,3},ok}
1.08% {{drv,[]},{select_reverse,2},ok}
1.07% {{ets,[]},{delete,2},ok}
1.05% {{ets,[]},{select_reverse,2},ok}
1.05% {{ets,[]},{foldr,3},ok}
1.05% {{ets,[]},{foldl,3},ok}
1.03% {{nif,[]},{match,2},ok}
1.03% {{ets,[]},{all,1},ok}
1.03% {{drv,[]},{prev,2},ok}
1.01% {{ets,[]},{member,2},ok}
1.01% {{ets,[]},{last,1},ok}
1.01% {{ets,[]},{insert_new,2},ok}
1.01% {{drv,[]},{first,1},ok}
0.96% {{nif,[]},{prev,2},ok}
0.96% {{nif,[]},{match31,3},ok}
0.96% {{nif,[]},{all,1},ok}
0.95% {{nif,[]},{tab2list,1},ok}
0.94% {{drv,[]},{lookup_element,3},{error,badarg}}
0.93% {{nif,[]},{first,1},ok}
0.93% {{ets,[]},{delete,1},ok}
0.92% {{nif,[]},{select31,3},ok}
0.92% {{nif,[]},{lookup_element,3},{error,badarg}}
0.92% {{ets,[]},{match,2},ok}
0.91% {{nif,[]},{select,2},ok}
0.90% {{nif,[]},{lookup,2},ok}
0.89% {{nif,[]},{insert,2},ok}
0.89% {{nif,[]},{foldl,3},ok}
0.88% {{nif,[]},{match_object31,3},ok}
0.88% {{ets,[]},{new,3},ok}
0.88% {{ets,[]},{lookup_element,3},{error,badarg}}
0.86% {{nif,[]},{delete,2},ok}
0.84% {{nif,[]},{select_reverse31,3},ok}
0.84% {{nif,[]},{last,1},ok}
0.84% {{nif,[]},{select_delete,2},ok}
0.82% {{nif,[]},{select_reverse,2},ok}
0.82% {{nif,[]},{foldr,3},ok}
0.81% {{nif,[]},{next,2},ok}
0.80% {{nif,[]},{match_delete,2},ok}
0.77% {{nif,[]},{select_count,2},ok}
0.77% {{nif,[]},{delete,1},ok}
0.76% {{nif,[]},{member,2},ok}
0.73% {{nif,[]},{new,3},ok}
0.67% {{ets,[]},{next,2},ok}
0.55% {{ets,[]},{prev,2},{error,badarg}}
0.52% {{ets,[]},{next,2},{error,badarg}}
0.50% {{ets,[]},{prev,2},ok}
0.39% {{drv,[hyper]},{tab2list,1},ok}
0.38% {{drv,[hyper]},{select_reverse31,3},ok}
0.38% {{drv,[hyper]},{select_reverse,2},ok}
0.36% {{drv,[hyper]},{match_object,2},ok}
0.35% {{drv,[hyper]},{insert,2},ok}
0.35% {{drv,[hyper]},{foldr,3},ok}
0.35% {{drv,[hyper]},{delete,1},ok}
0.34% {{drv,[hyper]},{new,3},ok}
0.34% {{drv,[hyper]},{lookup,2},ok}
0.34% {{drv,[hyper]},{delete,2},ok}
0.33% {{drv,[hyper]},{next,2},ok}
0.33% {{drv,[hyper]},{match_object31,3},ok}
0.33% {{drv,[hyper]},{all,1},ok}
0.31% {{drv,[hyper]},{prev,2},ok}
0.31% {{drv,[hyper]},{last,1},ok}
0.31% {{drv,[hyper]},{first,1},ok}
0.30% {{nif,[hyper]},{insert,2},ok}
0.30% {{drv,[hyper]},{select_count,2},ok}
0.30% {{drv,[hyper]},{select,2},ok}
0.29% {{nif,[hyper]},{select,2},ok}
0.29% {{nif,[hyper]},{delete,1},ok}
0.29% {{drv,[hyper]},{foldl,3},ok}
0.28% {{drv,[hyper]},{match31,3},ok}
0.27% {{nif,[hyper]},{select_reverse31,3},ok}
0.27% {{nif,[hyper]},{select31,3},ok}
0.27% {{nif,[hyper]},{select_reverse,2},ok}
0.27% {{drv,[hyper]},{select31,3},ok}
0.26% {{nif,[hyper]},{tab2list,1},ok}
0.26% {{nif,[hyper]},{select_count,2},ok}
0.26% {{nif,[hyper]},{lookup,2},ok}
0.26% {{nif,[hyper]},{all,1},ok}
0.26% {{drv,[hyper]},{member,2},ok}
0.25% {{nif,[hyper]},{new,3},ok}
0.25% {{nif,[hyper]},{match_object31,3},ok}
0.24% {{nif,[hyper]},{select_delete,2},ok}
0.24% {{drv,[hyper]},{select_delete,2},ok}
0.24% {{drv,[hyper]},{match_delete,2},ok}
0.24% {{drv,[hyper]},{match,2},ok}
0.24% {{drv,[hyper]},{lookup_element,3},{error,badarg}}
0.23% {{nif,[hyper]},{foldr,3},ok}
0.23% {{nif,[hyper]},{prev,2},ok}
0.23% {{nif,[hyper]},{member,2},ok}
0.23% {{nif,[hyper]},{match,2},ok}
0.22% {{nif,[hyper]},{first,1},ok}
0.21% {{nif,[hyper]},{match_object,2},ok}
0.21% {{nif,[hyper]},{last,1},ok}
0.21% {{nif,[hyper]},{delete,2},ok}
0.19% {{drv,[]},{lookup_element,3},ok}
0.18% {{nif,[hyper]},{match31,3},ok}
0.18% {{nif,[hyper]},{lookup_element,3},{error,badarg}}
0.18% {{nif,[]},{lookup_element,3},ok}
0.18% {{ets,[]},{lookup_element,3},ok}
0.18% {{nif,[hyper]},{next,2},ok}
0.18% {{nif,[hyper]},{match_delete,2},ok}
0.17% {{nif,[hyper]},{foldl,3},ok}
0.03% {{nif,[hyper]},{lookup_element,3},ok}
0.02% {{drv,[hyper]},{lookup_element,3},ok}
true
.......</code></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">For testing LevelDB directly using the C bindings, try<code>qc_statemc_lets:qc_run(500)</code>.</td>
</tr></table>




<h2 id="_documentation">Documentation</h2>


<h3 id="_where_should_i_start">Where should I start?</h3>
<p>This README is the only bit of documentation.</p>
<p>The QC (a.k.a. QuickCheck) tests underneath the "tests/qc" directory
should be helpful for understanding the specification and behavior of
ETS and LETS.  These QC tests also illustrate several strategies for
testing Erlang Driver-based and NIF-based implementations.</p>


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


<h3 id="_what_is_hyperleveldb">What is HyperLevelDB?</h3>
<p>HyperLevelDB is the data storage engine that powers HyperDex.
HyperLevelDB was forked from Google's LevelDB and adapted to more
closely meet the needs of HyperDex.</p>
<p>See <a href="https://github.com/rescrv/HyperLevelDB">https://github.com/rescrv/HyperLevelDB</a> for further details.</p>


<h3 id="_what_is_snappy">What is Snappy?</h3>
<p>Snappy is a fast compression/decompression library written at Google.</p>
<p>See <a href="http://code.google.com/p/snappy/">http://code.google.com/p/snappy/</a> for further details.</p>




<h2 id="_tools">Tools</h2>

<p>For further information and help for related tools, please refer to
the following links:</p>
<ul>
<li>
<p>
Erlang - <a href="http://www.erlang.org/">http://www.erlang.org/</a>
</p>
<ul>
<li>
<p>
<strong>R16B or newer, 17.0 has been tested most recently</strong>
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
<strong>Git 1.5.4 or newer, Git 1.9.3 has been tested most recently</strong>
</p>
</li>
</ul>
</li>
<li>
<p>
GitHub - <a href="https://github.com">https://github.com</a>
</p>
</li>
</ul>



<h2 id="_roadmap">Roadmap</h2>

<ul>
<li>
<p>
Documentation
</p>
<ul>
<li>
<p>
Explain how to run QuickCheck tests using a new rebar plugin.
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
<tr><td><a href="hets_impl_drv.md" class="module">hets_impl_drv</a></td></tr>
<tr><td><a href="hets_impl_nif.md" class="module">hets_impl_nif</a></td></tr>
<tr><td><a href="lets.md" class="module">lets</a></td></tr>
<tr><td><a href="lets_impl_drv.md" class="module">lets_impl_drv</a></td></tr>
<tr><td><a href="lets_impl_nif.md" class="module">lets_impl_nif</a></td></tr></table>

