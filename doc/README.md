

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
<code>drv</code> CPP Driver with LevelDB backend <em>(default)</em>
</p>
</li>
<li>
<p>
<code>nif</code> CPP NIF with LevelDB backend
</p>
</li>
<li>
<p>
<code>hyper drv</code> CPP Driver with HyperLevelDB backend
</p>
</li>
<li>
<p>
<code>hyper nif</code> CPP NIF with HyperLevelDB backend
</p>
</li>
<li>
<p>
<code>rocks drv</code> CPP Driver with RocksDB backend
</p>
</li>
<li>
<p>
<code>rocks nif</code> CPP NIF with RocksDB backend
</p>
</li>
<li>
<p>
<code>ets</code> Erlang ETS backend
</p>
</li>
</ul>
<p>If desired, all of the five backends can be called directly using the
table identifier returned by <code>new/2</code>, <code>tid/1</code>, and/or <code>tid/2</code>.  The
<code>tid/1</code> and <code>tid/2</code> approach must be used for named tables.  The five
backends implement the <code>gen_ets_ns</code> behaviour to provide batch
insertion, batch iteration by key, batch iteration by object, and
other basic table operations.  See the following modules for the
corresponding backend:</p>
<ul>
<li>
<p>
<code>drv</code> - src/lets_impl_drv.erl
</p>
</li>
<li>
<p>
<code>nif</code> - src/lets_impl_nif.erl
</p>
</li>
<li>
<p>
<code>hyper drv</code> - src/hets_impl_drv.erl
</p>
</li>
<li>
<p>
<code>hyper nif</code> - src/hets_impl_nif.erl
</p>
</li>
<li>
<p>
<code>rocks drv</code> - src/rets_impl_drv.erl
</p>
</li>
<li>
<p>
<code>rocks nif</code> - src/rets_impl_nif.erl
</p>
</li>
<li>
<p>
<code>ets</code> - deps/gen_ets/src/gen_ets_impl_ets.erl
</p>
</li>
</ul>
<p>The Driver backends are recommended over the NIF backends for two
reasons:</p>
<ul>
<li>
<p>
The NIF backends when deleting the table rely on garbage collection
  of the NIF resource to delete the corresponding CPP database object.
  The calling application must manage this carefully to ensure that a
  call to re-open an existing database is not performed until the CPP
  database object is deleted.  The Driver backends do not have this
  limitation.
</p>
</li>
<li>
<p>
The NIF backends are not built as "dirty" NIFs and can easily block
  the emulator's schedulers.  The Driver backends support the <code>async</code>
  option for such purpose.  It is recommended to use the Driver
  backends until the "dirty" NIF functionality is a non-experimental
  Erlang/OTP feature and the LETS NIF backends are built as "dirty"
  NIFs.
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

3.44% {{undefined,[]},{new,2},ok}
1.41% {{ets,[]},{select_reverse,2},ok}
1.39% {{ets,[]},{select_reverse31,3},ok}
1.37% {{ets,[]},{select,2},ok}
1.33% {{ets,[]},{select_delete,2},ok}
1.33% {{ets,[]},{member,2},ok}
1.33% {{ets,[]},{last,1},ok}
1.31% {{ets,[]},{delete_all_objects,1},ok}
1.30% {{ets,[]},{delete,2},ok}
1.29% {{ets,[]},{insert_new,2},ok}
1.29% {{ets,[]},{delete,1},ok}
1.28% {{ets,[]},{insert,2},ok}
1.27% {{ets,[]},{tid,2},ok}
1.27% {{ets,[]},{match_object,2},ok}
1.25% {{ets,[]},{select_count,2},ok}
1.24% {{ets,[]},{tab2list,1},ok}
1.24% {{ets,[]},{first,1},ok}
1.24% {{ets,[]},{new,3},ok}
1.24% {{ets,[]},{match_object31,3},ok}
1.24% {{ets,[]},{match,2},ok}
1.23% {{ets,[]},{tid,1},ok}
1.23% {{ets,[]},{select31,3},ok}
1.21% {{ets,[]},{lookup,2},ok}
1.19% {{ets,[]},{all,1},ok}
1.18% {{ets,[]},{match31,3},ok}
1.17% {{ets,[]},{foldr,3},ok}
1.09% {{ets,[]},{foldl,3},ok}
1.08% {{ets,[]},{lookup_element,3},{error,badarg}}
1.05% {{ets,[]},{match_delete,2},ok}
0.92% {{nif,[]},{select_reverse31,3},ok}
0.88% {{drv,[]},{match,2},ok}
0.88% {{drv,[]},{all,1},ok}
0.87% {{drv,[]},{match_delete,2},ok}
0.85% {{nif,[]},{member,2},ok}
0.85% {{nif,[]},{last,1},ok}
0.85% {{drv,[]},{match31,3},ok}
0.82% {{nif,[]},{first,1},ok}
0.82% {{nif,[]},{select31,3},ok}
0.82% {{drv,[]},{select_delete,2},ok}
0.81% {{nif,[]},{select_reverse,2},ok}
0.80% {{nif,[]},{prev,2},ok}
0.79% {{nif,[]},{tid,1},ok}
0.79% {{nif,[]},{match,2},ok}
0.79% {{nif,[]},{all,1},ok}
0.77% {{nif,[]},{match_object31,3},ok}
0.77% {{drv,[]},{member,2},ok}
0.77% {{drv,[]},{delete,1},ok}
0.76% {{nif,[]},{foldr,3},ok}
0.76% {{drv,[]},{select31,3},ok}
0.76% {{drv,[]},{first,1},ok}
0.75% {{drv,[]},{prev,2},ok}
0.75% {{drv,[]},{new,3},ok}
0.75% {{drv,[]},{last,1},ok}
0.75% {{nif,[]},{select_delete,2},ok}
0.75% {{drv,[]},{select_reverse31,3},ok}
0.74% {{nif,[]},{next,2},ok}
0.74% {{nif,[]},{delete,2},ok}
0.74% {{drv,[]},{next,2},ok}
0.74% {{drv,[]},{insert,2},ok}
0.73% {{nif,[]},{tid,2},ok}
0.73% {{nif,[]},{insert,2},ok}
0.73% {{drv,[]},{tid,1},ok}
0.73% {{drv,[]},{match_object31,3},ok}
0.73% {{drv,[]},{match_object,2},ok}
0.73% {{drv,[]},{foldl,3},ok}
0.73% {{drv,[]},{delete,2},ok}
0.72% {{nif,[]},{select,2},ok}
0.72% {{nif,[]},{match_object,2},ok}
0.72% {{nif,[]},{match31,3},ok}
0.71% {{nif,[]},{lookup,2},ok}
0.71% {{nif,[]},{foldl,3},ok}
0.71% {{drv,[]},{select,2},ok}
0.70% {{nif,[]},{tab2list,1},ok}
0.68% {{nif,[]},{match_delete,2},ok}
0.68% {{nif,[]},{new,3},ok}
0.68% {{nif,[]},{delete,1},ok}
0.68% {{drv,[]},{lookup_element,3},{error,badarg}}
0.67% {{drv,[]},{tab2list,1},ok}
0.67% {{drv,[]},{foldr,3},ok}
0.66% {{drv,[]},{select_reverse,2},ok}
0.66% {{drv,[]},{select_count,2},ok}
0.65% {{nif,[]},{select_count,2},ok}
0.65% {{drv,[]},{lookup,2},ok}
0.64% {{nif,[]},{lookup_element,3},{error,badarg}}
0.62% {{drv,[]},{tid,2},ok}
0.61% {{ets,[]},{next,2},ok}
0.58% {{ets,[]},{next,2},{error,badarg}}
0.56% {{ets,[]},{prev,2},ok}
0.51% {{ets,[]},{prev,2},{error,badarg}}
0.43% {{nif,[hyper]},{foldr,3},ok}
0.41% {{nif,[hyper]},{select_reverse31,3},ok}
0.41% {{nif,[hyper]},{prev,2},ok}
0.40% {{nif,[hyper]},{tid,1},ok}
0.40% {{nif,[hyper]},{match,2},ok}
0.39% {{nif,[hyper]},{delete,2},ok}
0.38% {{nif,[hyper]},{tab2list,1},ok}
0.38% {{nif,[hyper]},{match_delete,2},ok}
0.37% {{nif,[hyper]},{select_reverse,2},ok}
0.36% {{nif,[hyper]},{select,2},ok}
0.36% {{nif,[hyper]},{last,1},ok}
0.35% {{nif,[hyper]},{insert,2},ok}
0.35% {{nif,[hyper]},{foldl,3},ok}
0.35% {{nif,[hyper]},{match_object31,3},ok}
0.35% {{nif,[hyper]},{first,1},ok}
0.35% {{drv,[hyper]},{select_count,2},ok}
0.34% {{nif,[hyper]},{lookup_element,3},{error,badarg}}
0.33% {{nif,[hyper]},{tid,2},ok}
0.33% {{nif,[hyper]},{member,2},ok}
0.31% {{nif,[hyper]},{next,2},ok}
0.31% {{nif,[hyper]},{lookup,2},ok}
0.31% {{drv,[hyper]},{select_delete,2},ok}
0.31% {{drv,[hyper]},{lookup,2},ok}
0.31% {{drv,[hyper]},{foldr,3},ok}
0.31% {{drv,[hyper]},{foldl,3},ok}
0.31% {{nif,[hyper]},{select_delete,2},ok}
0.31% {{drv,[hyper]},{tid,2},ok}
0.31% {{drv,[hyper]},{select_reverse31,3},ok}
0.30% {{nif,[hyper]},{select_count,2},ok}
0.29% {{nif,[hyper]},{match31,3},ok}
0.28% {{nif,[hyper]},{select31,3},ok}
0.28% {{nif,[hyper]},{delete,1},ok}
0.28% {{drv,[hyper]},{match_delete,2},ok}
0.28% {{nif,[hyper]},{new,3},ok}
0.28% {{nif,[hyper]},{match_object,2},ok}
0.28% {{nif,[hyper]},{all,1},ok}
0.28% {{drv,[hyper]},{match_object,2},ok}
0.28% {{drv,[hyper]},{match,2},ok}
0.27% {{drv,[hyper]},{select_reverse,2},ok}
0.27% {{drv,[hyper]},{select31,3},ok}
0.27% {{drv,[hyper]},{last,1},ok}
0.27% {{drv,[hyper]},{first,1},ok}
0.26% {{drv,[hyper]},{delete,2},ok}
0.26% {{drv,[hyper]},{prev,2},ok}
0.24% {{drv,[hyper]},{lookup_element,3},{error,badarg}}
0.24% {{drv,[hyper]},{all,1},ok}
0.24% {{drv,[hyper]},{tid,1},ok}
0.24% {{drv,[hyper]},{select,2},ok}
0.24% {{drv,[hyper]},{member,2},ok}
0.23% {{drv,[hyper]},{delete,1},ok}
0.22% {{drv,[hyper]},{new,3},ok}
0.21% {{drv,[hyper]},{next,2},ok}
0.20% {{drv,[hyper]},{insert,2},ok}
0.18% {{ets,[]},{lookup_element,3},ok}
0.18% {{drv,[hyper]},{tab2list,1},ok}
0.18% {{drv,[hyper]},{match_object31,3},ok}
0.17% {{nif,[]},{lookup_element,3},ok}
0.14% {{drv,[hyper]},{match31,3},ok}
0.13% {{drv,[]},{lookup_element,3},ok}
0.04% {{nif,[hyper]},{lookup_element,3},ok}
0.01% {{drv,[hyper]},{lookup_element,3},ok}
true
.......</code></pre>


<table><tr>
<td class="icon">
Tip
</td>
<td class="content">For testing LevelDB directly using the C bindings, try<code>qc_statemc_lets:qc_run(500)</code>.</td>
</tr></table>

<p><em>OR</em> if PropEr is available then follow this recipe:</p>


<pre><code>$ mkdir working-directory-name
$ cd working-directory-name
$ git clone https://github.com/norton/lets.git lets
$ cd lets
$ make deps clean compile-for-proper
$ (cd .qc; erl -smp +A 5 -pz -pz ../deps/{sext,gen_ets,qc}/ebin)

1> qc_statem_lets:qc_run(500).
.......
OK: Passed 500 test(s).

8% {{undefined,[]},{new,2},ok}
1% {{ets,[]},{insert,2},ok}
1% {{ets,[]},{match31,3},ok}
1% {{ets,[]},{foldl,3},ok}
1% {{ets,[]},{first,1},ok}
1% {{ets,[]},{delete,2},ok}
1% {{ets,[]},{match,2},ok}
1% {{ets,[]},{match_object,2},ok}
1% {{ets,[]},{select31,3},ok}
1% {{ets,[]},{all,1},ok}
1% {{ets,[]},{match_delete,2},ok}
1% {{ets,[]},{tid,1},ok}
1% {{nif,[]},{match_delete,2},ok}
0% {{ets,[]},{select_reverse,2},ok}
0% {{ets,[]},{tab2list,1},ok}
0% {{ets,[]},{delete,1},ok}
0% {{ets,[]},{insert_new,2},ok}
0% {{ets,[]},{member,2},ok}
0% {{ets,[]},{foldr,3},ok}
0% {{ets,[]},{last,1},ok}
0% {{ets,[]},{new,3},ok}
0% {{ets,[]},{match_object31,3},ok}
0% {{nif,[]},{all,1},ok}
0% {{ets,[]},{select_count,2},ok}
0% {{drv,[]},{first,1},ok}
0% {{drv,[]},{lookup,2},ok}
0% {{ets,[]},{select_delete,2},ok}
0% {{nif,[]},{lookup,2},ok}
0% {{nif,[]},{tid,2},ok}
0% {{ets,[]},{delete_all_objects,1},ok}
0% {{ets,[]},{lookup,2},ok}
0% {{drv,[]},{prev,2},ok}
0% {{drv,[]},{select_count,2},ok}
0% {{drv,[]},{all,1},ok}
0% {{nif,[]},{delete,2},ok}
0% {{nif,[]},{match31,3},ok}
0% {{drv,[]},{member,2},ok}
0% {{ets,[]},{select,2},ok}
0% {{nif,[]},{foldr,3},ok}
0% {{nif,[]},{select_count,2},ok}
0% {{drv,[]},{match,2},ok}
0% {{drv,[]},{next,2},ok}
0% {{ets,[]},{tid,2},ok}
0% {{nif,[]},{tid,1},ok}
0% {{drv,[]},{delete,2},ok}
0% {{drv,[]},{foldl,3},ok}
0% {{drv,[]},{tid,1},ok}
0% {{ets,[]},{select_reverse31,3},ok}
0% {{nif,[]},{match,2},ok}
0% {{nif,[]},{member,2},ok}
0% {{drv,[]},{select,2},ok}
0% {{drv,[]},{select_reverse31,3},ok}
0% {{drv,[]},{tab2list,1},ok}
0% {{ets,[]},{lookup_element,3},{error,badarg}}
0% {{nif,[]},{first,1},ok}
0% {{nif,[]},{match_object,2},ok}
0% {{nif,[]},{select,2},ok}
0% {{nif,[]},{select_reverse,2},ok}
0% {{drv,[]},{match31,3},ok}
0% {{drv,[]},{tid,2},ok}
0% {{nif,[]},{foldl,3},ok}
0% {{nif,[]},{match_object31,3},ok}
0% {{nif,[]},{select31,3},ok}
0% {{nif,[]},{next,2},ok}
0% {{drv,[]},{select_delete,2},ok}
0% {{nif,[]},{select_delete,2},ok}
0% {{drv,[]},{delete,1},ok}
0% {{drv,[]},{match_object,2},ok}
0% {{drv,[]},{select31,3},ok}
0% {{nif,[]},{select_reverse31,3},ok}
0% {{nif,[]},{prev,2},ok}
0% {{drv,[]},{last,1},ok}
0% {{nif,[]},{delete,1},ok}
0% {{drv,[]},{insert,2},ok}
0% {{drv,[]},{new,3},ok}
0% {{nif,[]},{last,1},ok}
0% {{drv,[]},{foldr,3},ok}
0% {{ets,[]},{next,2},ok}
0% {{ets,[]},{prev,2},ok}
0% {{nif,[]},{insert,2},ok}
0% {{nif,[]},{lookup_element,3},{error,badarg}}
0% {{nif,[]},{tab2list,1},ok}
0% {{nif,[]},{new,3},ok}
0% {{drv,[]},{match_object31,3},ok}
0% {{drv,[]},{select_reverse,2},ok}
0% {{drv,[]},{lookup_element,3},{error,badarg}}
0% {{drv,[]},{match_delete,2},ok}
0% {{nif,[hyper]},{match_object31,3},ok}
0% {{nif,[hyper]},{select31,3},ok}
0% {{drv,[hyper]},{match_delete,2},ok}
0% {{drv,[hyper]},{select,2},ok}
0% {{drv,[hyper]},{tab2list,1},ok}
0% {{drv,[hyper]},{delete,2},ok}
0% {{drv,[hyper]},{select_delete,2},ok}
0% {{drv,[hyper]},{delete,1},ok}
0% {{drv,[hyper]},{foldl,3},ok}
0% {{drv,[hyper]},{insert,2},ok}
0% {{drv,[hyper]},{last,1},ok}
0% {{drv,[hyper]},{match,2},ok}
0% {{nif,[hyper]},{select,2},ok}
0% {{drv,[hyper]},{lookup_element,3},{error,badarg}}
0% {{drv,[hyper]},{match_object,2},ok}
0% {{drv,[hyper]},{prev,2},ok}
0% {{nif,[hyper]},{all,1},ok}
0% {{nif,[hyper]},{delete,1},ok}
0% {{nif,[hyper]},{insert,2},ok}
0% {{nif,[hyper]},{match_object,2},ok}
0% {{nif,[hyper]},{select_count,2},ok}
0% {{nif,[hyper]},{select_reverse,2},ok}
0% {{drv,[hyper]},{first,1},ok}
0% {{drv,[hyper]},{foldr,3},ok}
0% {{drv,[hyper]},{match_object31,3},ok}
0% {{drv,[hyper]},{new,3},ok}
0% {{drv,[hyper]},{select31,3},ok}
0% {{drv,[hyper]},{select_reverse,2},ok}
0% {{nif,[hyper]},{last,1},ok}
0% {{nif,[hyper]},{match,2},ok}
0% {{nif,[hyper]},{next,2},ok}
0% {{nif,[hyper]},{select_reverse31,3},ok}
0% {{drv,[hyper]},{lookup,2},ok}
0% {{drv,[hyper]},{select_count,2},ok}
0% {{nif,[hyper]},{member,2},ok}
0% {{nif,[hyper]},{new,3},ok}
0% {{nif,[hyper]},{select_delete,2},ok}
0% {{drv,[hyper]},{member,2},ok}
0% {{drv,[hyper]},{next,2},ok}
0% {{drv,[hyper]},{select_reverse31,3},ok}
0% {{nif,[hyper]},{first,1},ok}
0% {{nif,[hyper]},{foldl,3},ok}
0% {{nif,[hyper]},{lookup,2},ok}
0% {{drv,[hyper]},{all,1},ok}
0% {{drv,[hyper]},{tid,1},ok}
0% {{ets,[]},{next,2},{error,badarg}}
0% {{nif,[hyper]},{match31,3},ok}
0% {{drv,[hyper]},{match31,3},ok}
0% {{nif,[hyper]},{delete,2},ok}
0% {{nif,[hyper]},{lookup_element,3},{error,badarg}}
0% {{nif,[hyper]},{match_delete,2},ok}
0% {{nif,[hyper]},{tab2list,1},ok}
0% {{drv,[hyper]},{tid,2},ok}
0% {{nif,[hyper]},{prev,2},ok}
0% {{ets,[]},{prev,2},{error,badarg}}
0% {{nif,[hyper]},{foldr,3},ok}
0% {{nif,[hyper]},{tid,1},ok}
0% {{nif,[hyper]},{tid,2},ok}
0% {{drv,[]},{lookup_element,3},ok}
0% {{ets,[]},{lookup_element,3},ok}
0% {{nif,[hyper]},{lookup_element,3},ok}
0% {{nif,[]},{lookup_element,3},ok}
0% {{drv,[hyper]},{lookup_element,3},ok}
true
.......</code></pre>




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


<h3 id="_what_is_rocksdb">What is RocksDB?</h3>
<p>RocksDB is a persistent Key-Value Store for Flash and RAM Storage.
RocksDB is built on earlier work from Google's LevelDB.</p>
<p>See <a href="https://github.com/facebook/rocksdb">https://github.com/facebook/rocksdb</a> for further details.</p>


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
Backends (TBD)
</p>
<ul>
<li>
<p>
Implement the NIF backends as "dirty" NIFs.
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
</ul>




## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="hets_impl_drv.md" class="module">hets_impl_drv</a></td></tr>
<tr><td><a href="hets_impl_nif.md" class="module">hets_impl_nif</a></td></tr>
<tr><td><a href="lets.md" class="module">lets</a></td></tr>
<tr><td><a href="lets_impl_drv.md" class="module">lets_impl_drv</a></td></tr>
<tr><td><a href="lets_impl_nif.md" class="module">lets_impl_nif</a></td></tr>
<tr><td><a href="rets_impl_drv.md" class="module">rets_impl_drv</a></td></tr>
<tr><td><a href="rets_impl_nif.md" class="module">rets_impl_nif</a></td></tr></table>

