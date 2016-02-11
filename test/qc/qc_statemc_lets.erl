%%% The MIT License
%%%
%%% Copyright (C) 2011-2016 by Joseph Wayne Norton <norton@alum.mit.edu>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.

-module(qc_statemc_lets).

-ifdef(QC).
-ifdef(QC_EQC).

-include_lib("eqc/include/eqc_c.hrl").
-include_lib("qc/include/qc_statem.hrl").

-ifdef(QC_STATEM).

%% API
-export([qc_run/0, qc_run/1, qc_run/2]).
-export([qc_sample/0, qc_sample/1]).
-export([qc_prop/1]).
-export([qc_check/0, qc_check/1, qc_check/2]).
-export([qc_check_file/2]).

%% qc_statem Callbacks
-behaviour(qc_statem).
-export([command/1]).
-export([initial_state/0, initial_state/1, next_state/3, invariant/1, precondition/2, postcondition/3]).
-export([init/0, init/1, stop/2, aggregate/1]).

%% DEBUG -compile(export_all).


%%%----------------------------------------------------------------------
%%% defines, types, records
%%%----------------------------------------------------------------------

-define(IMPL, qc_leveldb).

-record(obj, {key :: binary(), val :: binary()}).

-type obj() :: #obj{}.
-type proplist() :: proplists:proplist().

-record(state, {
          parallel=false :: boolean(),
          exists=false   :: boolean(),
          options=[]     :: proplists:proplist(),
          db=undefined   :: undefined | term(),
          objs=[]        :: [obj()]
         }).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

qc_run() ->
    qc_run(100).

qc_run(NumTests) ->
    qc_run(NumTests, []).

qc_run(NumTests, Options) ->
    qc_statem:qc_run(?MODULE, NumTests, Options).

qc_sample() ->
    qc_sample([]).

qc_sample(Options) ->
    qc_statem:qc_sample(?MODULE, Options).

qc_prop(Options) ->
    qc_statem:qc_prop(?MODULE, Options).

qc_check() ->
    qc_check([]).

qc_check(Options) ->
    qc_check(Options, ?QC:counterexample()).

qc_check(Options, CounterExample) ->
    qc_statem:qc_check(?MODULE, Options, CounterExample).

qc_check_file(Options, FileName) ->
    qc_statem:qc_check_file(?MODULE, Options, FileName).


%%%----------------------------------------------------------------------
%%% qc_statem Callbacks
%%%----------------------------------------------------------------------
command(#state{parallel=false}=S) ->
    serial_command_gen(S);
command(#state{parallel=true}=S) ->
    parallel_command_gen(S).

serial_command_gen(#state{db=undefined, exists=false}) ->
    {call,?IMPL,open,[ulist(gen_db_options())]};
serial_command_gen(#state{db=undefined, exists=true}) ->
    oneof([{call,?IMPL,reopen,[ulist(gen_db_options())]}
           %% @TODO {call,?IMPL,destroy,[ulist(gen_db_options())]}
           %% @TODO {call,?IMPL,repair[ulist(gen_db_options())]}
          ]);
serial_command_gen(#state{db=Db}=S) ->
    oneof([{call,?IMPL,close,[Db]},
           {call,?IMPL,put,[Db,gen_obj(S),ulist(gen_db_write_options())]},
           {call,?IMPL,delete,[Db,gen_key(S),ulist(gen_db_write_options())]},
           {call,?IMPL,get,[Db,gen_key(S),ulist(gen_db_read_options())]},
           {call,?IMPL,first,[Db,ulist(gen_db_read_options())]},
           {call,?IMPL,last,[Db,ulist(gen_db_read_options())]},
           {call,?IMPL,next,[Db,gen_key(S),ulist(gen_db_read_options())]},
           {call,?IMPL,prev,[Db,gen_key(S),ulist(gen_db_read_options())]}
          ]).

parallel_command_gen(#state{db=undefined, exists=false}) ->
    {call,?IMPL,open,[ulist(gen_db_options())]};
parallel_command_gen(#state{db=Db}=S) ->
    oneof([{call,?IMPL,put,[Db,gen_obj(S),ulist(gen_db_write_options())]},
           {call,?IMPL,delete,[Db,gen_key(S),ulist(gen_db_write_options())]},
           {call,?IMPL,get,[Db,gen_key(S),ulist(gen_db_read_options())]}
          ]).

-spec initial_state() -> #state{}.
initial_state() ->
    initial_state([]).

-spec initial_state(proplist()) -> #state{}.
initial_state(Opts) ->
    #state{parallel=proplists:get_value(parallel, Opts, false)}.

-spec next_state(#state{}, term(), tuple()) -> #state{}.
next_state(#state{db=undefined, exists=false}=S, V, {call,_,open,[Opts]}) ->
    S#state{options=Opts, db=V, exists=true};
next_state(#state{db=undefined, exists=true}=S, V, {call,_,reopen,[Opts]}) ->
    S#state{options=Opts, db=V, exists=true};
next_state(#state{db=undefined, exists=true}=S, V, {call,_,destroy,[Opts]}) ->
    S#state{options=Opts, db=V, exists=false, objs=[]};
next_state(#state{db=Db}=S, _V, {call,_,close,[Db]}) when Db /= undefined ->
    S#state{db=undefined};
next_state(S, _V, {call,_,put,[_Db,Obj,_Opts]}) ->
    insert_obj(S, Obj);
next_state(S, _V, {call,_,delete,[_Db,Key,_Opts]}) ->
    delete_obj(S, Key);
next_state(S, _V, {call,_,_,_}) ->
    S.

-spec invariant(#state{}) -> boolean().
invariant(_S) ->
    true.

-spec precondition(#state{}, tuple()) -> boolean().
precondition(#state{exists=true}, {call,_,open,[__Opts]}) ->
    false;
precondition(#state{exists=false}, {call,_,reopen,[__Opts]}) ->
    false;
precondition(#state{exists=false}, {call,_,destroy,[__Opts]}) ->
    false;
precondition(#state{exists=false}, {call,_,repair,[__Opts]}) ->
    false;
precondition(#state{db=Db}, {call,_,open,[__Opts]}) when Db /= undefined->
    false;
precondition(#state{db=Db}, {call,_,reopen,[__Opts]}) when Db /= undefined->
    false;
precondition(#state{db=Db}, {call,_,destroy,[__Opts]}) when Db /= undefined->
    false;
precondition(#state{db=Db}, {call,_,repair,[__Opts]}) when Db /= undefined->
    false;
precondition(_S, {call,_,_,_}) ->
    true.

-spec postcondition(#state{}, tuple(), term()) -> boolean().
postcondition(#state{exists=false}, {call,_,open,[__Opts]}, Res) ->
    ?IMPL:is_db(Res);
postcondition(#state{exists=true}, {call,_,reopen,[_Opts]}, Res) ->
    ?IMPL:is_db(Res);
postcondition(#state{exists=true}, {call,_,destroy,[_Opts]}, Res) ->
    Res;
postcondition(#state{exists=true}, {call,_,repair,[_Opts]}, Res) ->
    Res;
postcondition(#state{db=Db}, {call,_,close,[_Db]}, Res) ->
    Res andalso Db /= undefined;
postcondition(_S, {call,_,put,[_Db,_,_Opts]}, Res) ->
    Res;
postcondition(_S, {call,_,delete,[_Db,_,_Opts]}, Res) ->
    Res;
postcondition(S, {call,_,get,[_Db,Key,_Opts]}, Res) ->
    Res =:= get_val(S, Key);
postcondition(#state{objs=[]}, {call,_,first,[_Db,_Opts]}, Res) ->
    Res;
postcondition(S, {call,_,first,[_Db,_Opts]}, Res) ->
    #obj{key=K} = hd(sort_objs(S)),
    Res =:= K;
postcondition(#state{objs=[]}, {call,_,last,[_Db,_Opts]}, Res) ->
    Res;
postcondition(S, {call,_,last,[_Db,_Opts]}, Res) ->
    #obj{key=K} = hd(lists:reverse(sort_objs(S))),
    Res =:= K;
postcondition(S, {call,_,next,[_Db,Key,_Opts]}, Res) ->
    case lists:dropwhile(fun(#obj{key=X}) -> X =< Key end, sort_objs(S)) of
        [] ->
            Res;
        [#obj{key=K}|_] ->
            Res =:= K
    end;
postcondition(S, {call,_,prev,[_Db,Key,_Opts]}, Res) ->
    case lists:dropwhile(fun(#obj{key=X}) -> X >= Key end, rsort_objs(S)) of
        [] ->
            Res;
        [#obj{key=K}|_] ->
            Res =:= K
    end;
postcondition(_S, {call,_,_,_}, _Res) ->
    false.

-spec init() -> ok.
init() ->
    ok.

-spec init(#state{}) -> ok.
init(_State) ->
    ok.

-spec stop(#state{}, #state{}) -> ok.
stop(_State0, _State) ->
    ok.

-spec aggregate([{integer(), term(), term(), #state{}}])
               -> [{{atom(), integer()}, term()}].
aggregate(L) ->
    [ {{Cmd,length(Args)},filter_reply(Reply)} || {_N,{set,_,{call,_,Cmd,Args}},Reply,_State} <- L ].

filter_reply({'EXIT',{Err,_}}) ->
    {error,Err};
filter_reply(_) ->
    ok.


%%%----------------------------------------------------------------------
%%% Internal - Generators
%%%----------------------------------------------------------------------

gen_db_options() ->
    oneof([paranoid_checks, {paranoid_checks,gen_boolean()}, {write_buffer_size,gen_pos_integer()}, {max_open_files,gen_pos_integer()}, {block_cache_size,gen_pos_integer()}, {block_size,gen_pos_integer()}, {block_restart_interval,gen_pos_integer()}, compression, {compression, oneof([no, snappy])}, {filter_policy, oneof([no, {bloom,gen_pos_integer()}])}]).

gen_db_read_options() ->
    oneof([verify_checksums, {verify_checksums,gen_boolean()}, fill_cache, {fill_cache,gen_boolean()}]).

gen_db_write_options() ->
    oneof([sync, {sync,gen_boolean()}]).

gen_boolean() ->
    oneof([true, false]).

gen_pos_integer() ->
    ?LET(N, nat(), N+1).

gen_bytes() ->
    ?LET(B, list(choose(0,127)), list_to_binary(B)).

gen_key() ->
    gen_bytes().

gen_val() ->
    gen_bytes().

gen_obj() ->
    #obj{key=gen_key(), val=gen_val()}.

gen_key(#state{objs=[]}) ->
    gen_key();
gen_key(#state{objs=Objs}) ->
    oneof([?LET(Obj, oneof(Objs), Obj#obj.key), gen_key()]).

gen_obj(#state{objs=[]}) ->
    gen_obj();
gen_obj(#state{objs=Objs}) ->
    oneof([oneof(Objs), gen_obj()]).


%%%----------------------------------------------------------------------
%%% Internal - Model
%%%----------------------------------------------------------------------

insert_obj(S, #obj{key=K}=Obj) ->
    case keymember(K, S) of
        false ->
            S#state{objs=[Obj|S#state.objs]};
        true ->
            S#state{objs=keyreplace(K, Obj, S)}
    end.

delete_obj(S, K) ->
    S#state{objs=keydelete(K, S)}.

get_val(S, K) ->
    case keyfind(K, S) of
        [] ->
            true;
        [#obj{val=Val}] ->
            Val
    end.

sort_objs(#state{objs=Objs}) ->
    lists:sort(Objs).

rsort_objs(S) ->
    lists:reverse(sort_objs(S)).

keydelete(X, #state{objs=L}) ->
    lists:filter(fun(#obj{key=K}) -> K =/= X end, L).

keyreplace(X, Y, #state{objs=L}) ->
    lists:map(fun(Z=#obj{key=K}) -> case K =:= X of true -> Y; false -> Z end end, L).

keyfind(X, #state{objs=L}) ->
    lists:filter(fun(#obj{key=K}) -> K =:= X end, L).

keymember(X, S) ->
    [] /= keyfind(X, S).


%%%----------------------------------------------------------------------
%%% Internal - Implementation
%%%----------------------------------------------------------------------

-endif. %% -ifdef(QC_STATEM).

-endif. %% -ifdef(QC_EQC).
-endif. %% -ifdef(QC).
