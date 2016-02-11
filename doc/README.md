

# LETS - LevelDB-based Erlang Term Storage #

Copyright (c) 2011-2016 by Joseph Wayne Norton

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
<p><em>Development and testing of LETS is done solely on Mac OS X.  Feedback
and/or bug reports on other operating systems is welcome.</em></p>
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

3.46% {{undefined,[]},{new,2},ok}
1.21% {{ets,[]},{select_delete,2},ok}
1.19% {{ets,[]},{tid,1},ok}
1.11% {{ets,[]},{match_object,2},ok}
1.11% {{ets,[]},{insert,2},ok}
1.11% {{ets,[]},{all,1},ok}
1.11% {{ets,[]},{select_reverse,2},ok}
1.11% {{ets,[]},{match_delete,2},ok}
1.08% {{ets,[]},{member,2},ok}
1.08% {{ets,[]},{insert_new,2},ok}
1.03% {{ets,[]},{foldl,3},ok}
1.02% {{ets,[]},{select,2},ok}
1.01% {{ets,[]},{foldr,3},ok}
1.01% {{ets,[]},{delete,2},ok}
1.01% {{ets,[]},{select31,3},ok}
1.00% {{ets,[]},{match_object31,3},ok}
0.98% {{ets,[]},{tid,2},ok}
0.97% {{ets,[]},{select_reverse31,3},ok}
0.97% {{ets,[]},{select_count,2},ok}
0.97% {{ets,[]},{delete,1},ok}
0.95% {{ets,[]},{delete_all_objects,1},ok}
0.94% {{ets,[]},{last,1},ok}
0.93% {{ets,[]},{lookup_element,3},{error,badarg}}
0.93% {{ets,[]},{first,1},ok}
0.92% {{ets,[]},{tab2list,1},ok}
0.90% {{ets,[]},{match,2},ok}
0.90% {{ets,[]},{lookup,2},ok}
0.90% {{ets,[]},{match31,3},ok}
0.89% {{ets,[]},{new,3},ok}
0.67% {{drv,[hyper]},{tid,1},ok}
0.66% {{drv,[hyper]},{select,2},ok}
0.66% {{drv,[hyper]},{all,1},ok}
0.65% {{drv,[hyper]},{match_object31,3},ok}
0.64% {{nif,[]},{match_object,2},ok}
0.64% {{drv,[hyper]},{select_reverse31,3},ok}
0.64% {{nif,[]},{select_reverse31,3},ok}
0.64% {{drv,[hyper]},{tid,2},ok}
0.63% {{nif,[]},{tab2list,1},ok}
0.62% {{nif,[]},{select31,3},ok}
0.62% {{drv,[hyper]},{foldr,3},ok}
0.61% {{drv,[hyper]},{next,2},ok}
0.61% {{nif,[]},{select_delete,2},ok}
0.61% {{drv,[hyper]},{first,1},ok}
0.60% {{nif,[]},{match_object31,3},ok}
0.60% {{drv,[hyper]},{match_object,2},ok}
0.60% {{drv,[hyper]},{match_delete,2},ok}
0.60% {{drv,[hyper]},{delete,2},ok}
0.59% {{drv,[hyper]},{foldl,3},ok}
0.58% {{nif,[]},{select,2},ok}
0.58% {{drv,[hyper]},{tab2list,1},ok}
0.58% {{drv,[hyper]},{select_count,2},ok}
0.57% {{drv,[hyper]},{member,2},ok}
0.56% {{drv,[hyper]},{match31,3},ok}
0.56% {{drv,[hyper]},{insert,2},ok}
0.56% {{nif,[]},{prev,2},ok}
0.56% {{nif,[]},{next,2},ok}
0.55% {{nif,[]},{select_reverse,2},ok}
0.55% {{nif,[]},{lookup,2},ok}
0.55% {{drv,[hyper]},{select_delete,2},ok}
0.54% {{nif,[]},{all,1},ok}
0.54% {{ets,[]},{next,2},{error,badarg}}
0.53% {{nif,[]},{match31,3},ok}
0.53% {{nif,[]},{delete,1},ok}
0.53% {{nif,[]},{new,3},ok}
0.51% {{nif,[]},{foldr,3},ok}
0.51% {{drv,[hyper]},{lookup,2},ok}
0.51% {{nif,[]},{last,1},ok}
0.51% {{ets,[]},{next,2},ok}
0.51% {{drv,[hyper]},{select_reverse,2},ok}
0.51% {{drv,[hyper]},{prev,2},ok}
0.50% {{drv,[hyper]},{last,1},ok}
0.48% {{nif,[]},{member,2},ok}
0.48% {{ets,[]},{prev,2},ok}
0.48% {{drv,[hyper]},{select31,3},ok}
0.48% {{nif,[]},{match_delete,2},ok}
0.48% {{nif,[]},{delete,2},ok}
0.47% {{drv,[hyper]},{match,2},ok}
0.47% {{drv,[hyper]},{delete,1},ok}
0.46% {{drv,[]},{delete,2},ok}
0.45% {{nif,[]},{foldl,3},ok}
0.44% {{nif,[hyper]},{match_object31,3},ok}
0.44% {{nif,[]},{tid,2},ok}
0.44% {{nif,[]},{insert,2},ok}
0.43% {{nif,[rocks]},{member,2},ok}
0.43% {{drv,[hyper]},{new,3},ok}
0.43% {{drv,[hyper]},{lookup_element,3},{error,badarg}}
0.43% {{nif,[rocks]},{all,1},ok}
0.43% {{ets,[]},{prev,2},{error,badarg}}
0.42% {{nif,[hyper]},{match_object,2},ok}
0.42% {{drv,[]},{last,1},ok}
0.41% {{nif,[]},{tid,1},ok}
0.40% {{drv,[]},{tid,1},ok}
0.40% {{drv,[]},{select_reverse,2},ok}
0.39% {{nif,[hyper]},{tid,1},ok}
0.39% {{nif,[hyper]},{select31,3},ok}
0.39% {{drv,[]},{insert,2},ok}
0.38% {{nif,[rocks]},{select,2},ok}
0.38% {{nif,[rocks]},{insert,2},ok}
0.38% {{nif,[rocks]},{foldr,3},ok}
0.38% {{nif,[hyper]},{lookup,2},ok}
0.38% {{nif,[hyper]},{last,1},ok}
0.38% {{nif,[hyper]},{first,1},ok}
0.38% {{nif,[]},{match,2},ok}
0.38% {{nif,[]},{first,1},ok}
0.38% {{drv,[]},{match_object31,3},ok}
0.38% {{nif,[rocks]},{select31,3},ok}
0.38% {{nif,[hyper]},{insert,2},ok}
0.38% {{nif,[]},{select_count,2},ok}
0.38% {{drv,[]},{tid,2},ok}
0.37% {{nif,[rocks]},{select_reverse31,3},ok}
0.37% {{nif,[rocks]},{prev,2},ok}
0.37% {{nif,[rocks]},{match,2},ok}
0.37% {{drv,[rocks]},{select_delete,2},ok}
0.37% {{drv,[]},{first,1},ok}
0.36% {{drv,[]},{tab2list,1},ok}
0.35% {{nif,[rocks]},{tab2list,1},ok}
0.35% {{nif,[hyper]},{select_reverse31,3},ok}
0.35% {{nif,[hyper]},{next,2},ok}
0.35% {{drv,[rocks]},{tid,2},ok}
0.35% {{drv,[]},{delete,1},ok}
0.35% {{nif,[rocks]},{match_object,2},ok}
0.35% {{nif,[hyper]},{select,2},ok}
0.35% {{nif,[hyper]},{match_delete,2},ok}
0.35% {{nif,[hyper]},{delete,2},ok}
0.35% {{drv,[]},{select_reverse31,3},ok}
0.35% {{drv,[]},{foldr,3},ok}
0.34% {{nif,[rocks]},{select_delete,2},ok}
0.34% {{nif,[hyper]},{select_reverse,2},ok}
0.34% {{nif,[hyper]},{foldr,3},ok}
0.33% {{nif,[hyper]},{tab2list,1},ok}
0.33% {{nif,[]},{lookup_element,3},{error,badarg}}
0.33% {{drv,[rocks]},{next,2},ok}
0.33% {{drv,[]},{match_object,2},ok}
0.33% {{nif,[rocks]},{select_count,2},ok}
0.33% {{nif,[rocks]},{match_object31,3},ok}
0.33% {{nif,[rocks]},{match_delete,2},ok}
0.33% {{nif,[hyper]},{prev,2},ok}
0.33% {{nif,[hyper]},{match31,3},ok}
0.33% {{drv,[rocks]},{prev,2},ok}
0.32% {{nif,[rocks]},{foldl,3},ok}
0.32% {{nif,[hyper]},{match,2},ok}
0.32% {{nif,[hyper]},{lookup_element,3},{error,badarg}}
0.32% {{drv,[rocks]},{last,1},ok}
0.32% {{drv,[]},{new,3},ok}
0.32% {{drv,[]},{match,2},ok}
0.31% {{nif,[rocks]},{tid,2},ok}
0.31% {{nif,[rocks]},{match31,3},ok}
0.31% {{nif,[rocks]},{delete,2},ok}
0.31% {{nif,[rocks]},{delete,1},ok}
0.31% {{drv,[rocks]},{tid,1},ok}
0.31% {{drv,[]},{lookup,2},ok}
0.31% {{drv,[]},{all,1},ok}
0.30% {{nif,[rocks]},{select_reverse,2},ok}
0.30% {{nif,[rocks]},{new,3},ok}
0.30% {{nif,[hyper]},{all,1},ok}
0.30% {{drv,[rocks]},{first,1},ok}
0.30% {{drv,[]},{prev,2},ok}
0.30% {{drv,[]},{next,2},ok}
0.30% {{drv,[]},{lookup_element,3},{error,badarg}}
0.30% {{nif,[hyper]},{tid,2},ok}
0.30% {{drv,[rocks]},{tab2list,1},ok}
0.30% {{drv,[rocks]},{select_reverse,2},ok}
0.30% {{drv,[]},{match_delete,2},ok}
0.29% {{nif,[rocks]},{next,2},ok}
0.29% {{nif,[hyper]},{select_count,2},ok}
0.29% {{drv,[rocks]},{delete,2},ok}
0.28% {{drv,[rocks]},{insert,2},ok}
0.28% {{drv,[]},{foldl,3},ok}
0.28% {{nif,[rocks]},{tid,1},ok}
0.28% {{nif,[hyper]},{select_delete,2},ok}
0.28% {{nif,[hyper]},{member,2},ok}
0.28% {{nif,[hyper]},{foldl,3},ok}
0.28% {{nif,[hyper]},{delete,1},ok}
0.28% {{drv,[rocks]},{delete,1},ok}
0.28% {{drv,[]},{select,2},ok}
0.27% {{drv,[rocks]},{new,3},ok}
0.27% {{drv,[rocks]},{lookup,2},ok}
0.27% {{drv,[]},{select_delete,2},ok}
0.27% {{drv,[]},{select_count,2},ok}
0.27% {{drv,[]},{match31,3},ok}
0.26% {{nif,[rocks]},{lookup_element,3},{error,badarg}}
0.26% {{nif,[hyper]},{new,3},ok}
0.26% {{drv,[rocks]},{select31,3},ok}
0.26% {{drv,[rocks]},{match_object,2},ok}
0.26% {{drv,[rocks]},{match_delete,2},ok}
0.26% {{drv,[]},{member,2},ok}
0.25% {{nif,[rocks]},{lookup,2},ok}
0.25% {{drv,[rocks]},{match,2},ok}
0.25% {{nif,[rocks]},{last,1},ok}
0.25% {{nif,[rocks]},{first,1},ok}
0.25% {{drv,[rocks]},{match31,3},ok}
0.25% {{drv,[rocks]},{foldr,3},ok}
0.24% {{drv,[rocks]},{select_count,2},ok}
0.23% {{drv,[rocks]},{member,2},ok}
0.23% {{drv,[rocks]},{all,1},ok}
0.22% {{drv,[rocks]},{select,2},ok}
0.22% {{drv,[]},{select31,3},ok}
0.21% {{drv,[rocks]},{match_object31,3},ok}
0.21% {{drv,[rocks]},{foldl,3},ok}
0.20% {{drv,[rocks]},{lookup_element,3},{error,badarg}}
0.17% {{drv,[rocks]},{select_reverse31,3},ok}
0.13% {{ets,[]},{lookup_element,3},ok}
0.11% {{nif,[]},{lookup_element,3},ok}
0.08% {{drv,[hyper]},{lookup_element,3},ok}
0.07% {{nif,[rocks]},{lookup_element,3},ok}
0.04% {{drv,[]},{lookup_element,3},ok}
0.03% {{nif,[hyper]},{lookup_element,3},ok}
0.03% {{drv,[rocks]},{lookup_element,3},ok}
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

9% {{undefined,[]},{new,2},ok}
1% {{ets,[]},{foldr,3},ok}
1% {{ets,[]},{tid,2},ok}
1% {{ets,[]},{delete,1},ok}
1% {{ets,[]},{tid,1},ok}
1% {{ets,[]},{delete_all_objects,1},ok}
1% {{ets,[]},{new,3},ok}
1% {{ets,[]},{select,2},ok}
1% {{ets,[]},{all,1},ok}
1% {{ets,[]},{last,1},ok}
1% {{ets,[]},{select_delete,2},ok}
1% {{ets,[]},{select_reverse31,3},ok}
1% {{ets,[]},{match_delete,2},ok}
1% {{ets,[]},{select31,3},ok}
1% {{ets,[]},{insert_new,2},ok}
1% {{ets,[]},{match_object,2},ok}
0% {{ets,[]},{delete,2},ok}
0% {{ets,[]},{tab2list,1},ok}
0% {{ets,[]},{insert,2},ok}
0% {{ets,[]},{match_object31,3},ok}
0% {{ets,[]},{foldl,3},ok}
0% {{ets,[]},{lookup,2},ok}
0% {{ets,[]},{lookup_element,3},{error,badarg}}
0% {{ets,[]},{first,1},ok}
0% {{ets,[]},{member,2},ok}
0% {{ets,[]},{select_count,2},ok}
0% {{ets,[]},{match,2},ok}
0% {{ets,[]},{select_reverse,2},ok}
0% {{ets,[]},{match31,3},ok}
0% {{ets,[]},{prev,2},ok}
0% {{nif,[]},{select,2},ok}
0% {{ets,[]},{next,2},{error,badarg}}
0% {{nif,[]},{tid,1},ok}
0% {{drv,[]},{match,2},ok}
0% {{drv,[]},{select_delete,2},ok}
0% {{nif,[]},{delete,2},ok}
0% {{nif,[]},{first,1},ok}
0% {{nif,[]},{insert,2},ok}
0% {{nif,[]},{select31,3},ok}
0% {{drv,[]},{delete,1},ok}
0% {{nif,[]},{match_object,2},ok}
0% {{drv,[hyper]},{foldr,3},ok}
0% {{drv,[rocks]},{prev,2},ok}
0% {{ets,[]},{next,2},ok}
0% {{ets,[]},{prev,2},{error,badarg}}
0% {{nif,[]},{delete,1},ok}
0% {{nif,[]},{match_delete,2},ok}
0% {{nif,[rocks]},{tid,1},ok}
0% {{drv,[hyper]},{match31,3},ok}
0% {{nif,[]},{match,2},ok}
0% {{nif,[]},{new,3},ok}
0% {{nif,[]},{select_reverse31,3},ok}
0% {{drv,[]},{match31,3},ok}
0% {{drv,[]},{tab2list,1},ok}
0% {{nif,[]},{foldr,3},ok}
0% {{nif,[rocks]},{last,1},ok}
0% {{nif,[rocks]},{lookup_element,3},{error,badarg}}
0% {{nif,[rocks]},{match,2},ok}
0% {{drv,[]},{new,3},ok}
0% {{drv,[hyper]},{select31,3},ok}
0% {{drv,[hyper]},{select_reverse,2},ok}
0% {{drv,[hyper]},{select_reverse31,3},ok}
0% {{nif,[]},{next,2},ok}
0% {{nif,[rocks]},{foldl,3},ok}
0% {{nif,[rocks]},{select_delete,2},ok}
0% {{nif,[rocks]},{tab2list,1},ok}
0% {{drv,[]},{all,1},ok}
0% {{drv,[]},{delete,2},ok}
0% {{drv,[]},{first,1},ok}
0% {{drv,[]},{foldr,3},ok}
0% {{drv,[]},{match_object31,3},ok}
0% {{drv,[]},{next,2},ok}
0% {{drv,[hyper]},{match_object31,3},ok}
0% {{drv,[hyper]},{tab2list,1},ok}
0% {{nif,[]},{select_delete,2},ok}
0% {{nif,[rocks]},{prev,2},ok}
0% {{drv,[hyper]},{insert,2},ok}
0% {{drv,[hyper]},{select,2},ok}
0% {{drv,[rocks]},{tid,1},ok}
0% {{nif,[]},{last,1},ok}
0% {{nif,[]},{lookup_element,3},{error,badarg}}
0% {{nif,[]},{match_object31,3},ok}
0% {{nif,[]},{member,2},ok}
0% {{drv,[]},{insert,2},ok}
0% {{drv,[hyper]},{tid,2},ok}
0% {{drv,[rocks]},{match31,3},ok}
0% {{nif,[]},{lookup,2},ok}
0% {{nif,[]},{match31,3},ok}
0% {{nif,[]},{select_count,2},ok}
0% {{drv,[]},{last,1},ok}
0% {{drv,[]},{lookup,2},ok}
0% {{drv,[]},{select_count,2},ok}
0% {{drv,[hyper]},{lookup_element,3},{error,badarg}}
0% {{drv,[hyper]},{match,2},ok}
0% {{drv,[hyper]},{select_delete,2},ok}
0% {{nif,[]},{prev,2},ok}
0% {{nif,[]},{tab2list,1},ok}
0% {{nif,[rocks]},{next,2},ok}
0% {{drv,[]},{foldl,3},ok}
0% {{drv,[]},{match_object,2},ok}
0% {{drv,[]},{member,2},ok}
0% {{drv,[]},{select,2},ok}
0% {{drv,[]},{select31,3},ok}
0% {{drv,[]},{tid,2},ok}
0% {{drv,[hyper]},{foldl,3},ok}
0% {{drv,[rocks]},{all,1},ok}
0% {{drv,[rocks]},{match_object,2},ok}
0% {{nif,[rocks]},{foldr,3},ok}
0% {{nif,[rocks]},{lookup,2},ok}
0% {{nif,[rocks]},{member,2},ok}
0% {{drv,[hyper]},{all,1},ok}
0% {{drv,[rocks]},{tab2list,1},ok}
0% {{nif,[]},{all,1},ok}
0% {{nif,[]},{select_reverse,2},ok}
0% {{nif,[hyper]},{tab2list,1},ok}
0% {{nif,[rocks]},{first,1},ok}
0% {{nif,[rocks]},{insert,2},ok}
0% {{nif,[rocks]},{select31,3},ok}
0% {{nif,[rocks]},{select_reverse31,3},ok}
0% {{drv,[]},{lookup_element,3},{error,badarg}}
0% {{drv,[]},{select_reverse,2},ok}
0% {{drv,[hyper]},{first,1},ok}
0% {{drv,[hyper]},{member,2},ok}
0% {{drv,[rocks]},{delete,1},ok}
0% {{drv,[rocks]},{last,1},ok}
0% {{drv,[rocks]},{lookup,2},ok}
0% {{drv,[rocks]},{match,2},ok}
0% {{drv,[rocks]},{new,3},ok}
0% {{drv,[rocks]},{select_reverse,2},ok}
0% {{nif,[]},{foldl,3},ok}
0% {{nif,[hyper]},{delete,2},ok}
0% {{nif,[hyper]},{last,1},ok}
0% {{nif,[hyper]},{select_count,2},ok}
0% {{nif,[rocks]},{delete,1},ok}
0% {{nif,[rocks]},{match_delete,2},ok}
0% {{nif,[rocks]},{match_object31,3},ok}
0% {{nif,[rocks]},{select_count,2},ok}
0% {{drv,[]},{match_delete,2},ok}
0% {{drv,[]},{prev,2},ok}
0% {{drv,[hyper]},{delete,1},ok}
0% {{drv,[hyper]},{next,2},ok}
0% {{drv,[hyper]},{prev,2},ok}
0% {{drv,[hyper]},{tid,1},ok}
0% {{drv,[rocks]},{foldl,3},ok}
0% {{drv,[rocks]},{match_delete,2},ok}
0% {{nif,[]},{tid,2},ok}
0% {{nif,[hyper]},{foldr,3},ok}
0% {{nif,[hyper]},{lookup,2},ok}
0% {{nif,[rocks]},{delete,2},ok}
0% {{nif,[rocks]},{select,2},ok}
0% {{nif,[rocks]},{tid,2},ok}
0% {{drv,[hyper]},{last,1},ok}
0% {{drv,[hyper]},{lookup,2},ok}
0% {{drv,[hyper]},{match_delete,2},ok}
0% {{drv,[hyper]},{select_count,2},ok}
0% {{drv,[rocks]},{match_object31,3},ok}
0% {{ets,[]},{lookup_element,3},ok}
0% {{nif,[hyper]},{match31,3},ok}
0% {{nif,[hyper]},{prev,2},ok}
0% {{nif,[hyper]},{select,2},ok}
0% {{nif,[rocks]},{new,3},ok}
0% {{drv,[]},{select_reverse31,3},ok}
0% {{drv,[hyper]},{match_object,2},ok}
0% {{drv,[hyper]},{new,3},ok}
0% {{drv,[rocks]},{first,1},ok}
0% {{drv,[rocks]},{foldr,3},ok}
0% {{drv,[rocks]},{insert,2},ok}
0% {{nif,[hyper]},{first,1},ok}
0% {{nif,[hyper]},{insert,2},ok}
0% {{nif,[hyper]},{select_reverse31,3},ok}
0% {{nif,[rocks]},{all,1},ok}
0% {{nif,[rocks]},{match31,3},ok}
0% {{drv,[]},{tid,1},ok}
0% {{drv,[rocks]},{select_delete,2},ok}
0% {{nif,[hyper]},{match_object31,3},ok}
0% {{nif,[hyper]},{select_delete,2},ok}
0% {{nif,[hyper]},{tid,1},ok}
0% {{nif,[rocks]},{select_reverse,2},ok}
0% {{drv,[hyper]},{delete,2},ok}
0% {{nif,[hyper]},{all,1},ok}
0% {{nif,[hyper]},{match,2},ok}
0% {{nif,[hyper]},{match_delete,2},ok}
0% {{nif,[hyper]},{member,2},ok}
0% {{drv,[rocks]},{delete,2},ok}
0% {{drv,[rocks]},{select,2},ok}
0% {{drv,[rocks]},{select31,3},ok}
0% {{nif,[hyper]},{delete,1},ok}
0% {{nif,[hyper]},{lookup_element,3},{error,badarg}}
0% {{nif,[hyper]},{next,2},ok}
0% {{nif,[hyper]},{select_reverse,2},ok}
0% {{nif,[hyper]},{tid,2},ok}
0% {{nif,[rocks]},{match_object,2},ok}
0% {{drv,[rocks]},{member,2},ok}
0% {{drv,[rocks]},{select_reverse31,3},ok}
0% {{drv,[rocks]},{tid,2},ok}
0% {{nif,[hyper]},{foldl,3},ok}
0% {{nif,[hyper]},{new,3},ok}
0% {{drv,[rocks]},{next,2},ok}
0% {{drv,[rocks]},{select_count,2},ok}
0% {{nif,[]},{lookup_element,3},ok}
0% {{nif,[hyper]},{match_object,2},ok}
0% {{drv,[rocks]},{lookup_element,3},{error,badarg}}
0% {{nif,[hyper]},{select31,3},ok}
0% {{drv,[rocks]},{lookup_element,3},ok}
0% {{nif,[hyper]},{lookup_element,3},ok}
0% {{drv,[hyper]},{lookup_element,3},ok}
0% {{nif,[rocks]},{lookup_element,3},ok}
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
<p>See <a href="https://github.com/google/leveldb">https://github.com/google/leveldb</a> for further details.</p>


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
<p>See <a href="https://github.com/google/snappy">https://github.com/google/snappy</a> for further details.</p>




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
<strong>R16B or newer, 17.5 has been tested most recently</strong>
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
<strong>Git 1.5.4 or newer, Git 2.3.5 has been tested most recently</strong>
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

