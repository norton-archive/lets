

#LETS - LevelDB-based Erlang Term Storage#


Copyright (c) 2011 by Joseph Wayne Norton

__Authors:__ Joseph Wayne Norton ([`norton@alum.mit.edu`](mailto:norton@alum.mit.edu)).<p>LETS is an alternative Erlang Term Storage using LevelDB as the
storage implementation.  LETS tries to address some bad properties of
ETS and DETS.  ETS is limited by physical memory.  DETS is limited by
a 2 GB file size limitation and does not implement ordered sets.  LETS
has neither of these limitations.</p>
<p>For testing and comparison purposes, LETS supports three
implementations:</p>
<ul>
<li>
<p>
<tt>drv</tt> C+\+ Driver with LevelDB backend <em>(default)</em>
</p>
</li>
<li>
<p>
<tt>nif</tt> C+\+ NIF with LevelDB backend
</p>
</li>
<li>
<p>
<tt>ets</tt> Erlang ETS backend
</p>
</li>
</ul>
<p>LETS is not intended to be an exact clone of ETS.  The currently
supported APIs are:</p>
<ul>
<li>
<p>
<tt>new/2</tt>
</p>
</li>
<li>
<p>
<tt>destroy/2</tt> <em>only driver and nif implementations</em>
</p>
</li>
<li>
<p>
<tt>repair/2</tt>  <em>only driver and nif implementations</em>
</p>
</li>
<li>
<p>
<tt>insert/2</tt>
</p>
</li>
<li>
<p>
<tt>insert_new/2</tt> <em>only the ets implementation</em>
</p>
</li>
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
<tt>delete_all_objects/1</tt> <em>only the ets implementation</em>
</p>
</li>
<li>
<p>
<tt>lookup/2</tt>
</p>
</li>
<li>
<p>
<tt>first/1</tt>
</p>
</li>
<li>
<p>
<tt>next/2</tt>
</p>
</li>
<li>
<p>
<tt>info/2</tt> <em>only a subset of items</em>
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
$ git clone git://github.com/norton/snappy.git snappy
$ git clone git://github.com/norton/leveldb.git leveldb
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
<p>The QC (a.k.a. QuickCheck, Proper, etc.) tests underneath the
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
$ wget -O - https://github.com/android/tools_repo/raw/master/repo > ~/bin/repo
$ perl -i.bak -pe 's!git://android.git.kernel.org/tools/repo.git!git://github.com/android/tools_repo.git!;' ~/bin/repo
$ chmod a+x ~/bin/repo</tt></pre>


<table><tr>
<td class="icon">
Caution
</td>
<td class="content">Since access to kernel.org has been shutdown due to hackers,
fetch and replace repo tool with android's GitHub repository mirror.</td>
</tr></table>

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
<strong>R14B03 or newer, R14B04 has been tested most recently</strong>
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
<strong>Git 1.5.4 or newer, Git 1.7.7 has been tested recently</strong>
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

1> qc_statem_lets:run(5000).
.......</tt></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">For testing LevelDB directly using the C bindings, try
     <tt>qc_statemc_lets:run(5000)</tt>.</td>
</tr></table>

</li>
</ol>



<h2 id="_to_test_proper">To test - Proper</h2>

<ol class="arabic">
<li>
<p>
Make sure Proper is in your Erlang code path.  One simple way to
   accomplish this is by adding the code path to your <tt>~/.erlang</tt>
   resource file.
</p>


<pre><tt>true = code:add_pathz(os:getenv("HOME")++"/.erlang.d/lib/proper/ebin").</tt></pre>

</li>
<li>
<p>
Compile for Proper
</p>


<pre><tt>$ cd working-directory-name/src
$ make clean
$ make compile-proper proper-compile</tt></pre>

</li>
<li>
<p>
Run 5,000 Proper tests
</p>


<pre><tt>$ cd working-directory-name/src/lib/lets/.eunit
$ erl -smp +A 5 -pz ../../sext/ebin -pz ../../qc/ebin

1> qc_statem_lets:run(5000).
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
Explain how to run QuickCheck/Proper tests using a new rebar
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
Testing
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
</ul>
</li>
<li>
<p>
Existing APIs (TBD)
</p>
<ul>
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
<tr><td><a href="lets.md" class="module">lets</a></td></tr>
<tr><td><a href="lets_drv.md" class="module">lets_drv</a></td></tr>
<tr><td><a href="lets_ets.md" class="module">lets_ets</a></td></tr>
<tr><td><a href="lets_nif.md" class="module">lets_nif</a></td></tr></table>

