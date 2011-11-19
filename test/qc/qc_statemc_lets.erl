%%% The MIT License
%%%
%%% Copyright (C) 2011 by Joseph Wayne Norton <norton@alum.mit.edu>
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
-ifdef(EQC).

%% qc_statem Callbacks
-behaviour(qc_statem).
-export([command_gen/2]).
-export([initial_state/0, state_is_sane/1, next_state/3, precondition/2, postcondition/3]).
-export([setup/1, teardown/1, teardown/2, aggregate/1]).

%% @NOTE For boilerplate exports, see "qc_statem.hrl"
-include_lib("eqc/include/eqc_c.hrl").
-include_lib("qc/include/qc_statem.hrl").


%%%----------------------------------------------------------------------
%%% defines, types, records
%%%----------------------------------------------------------------------

-define(IMPL, qc_leveldb).

-record(obj, {key :: binary(), val :: binary()}).

-type obj() :: #obj{}.

-record(state, {
          parallel=false :: boolean(),
          exists=false   :: boolean(),
          db=undefined   :: undefined | term(),
          objs=[]        :: [obj()]
         }).


%%%----------------------------------------------------------------------
%%% qc_statem Callbacks
%%%----------------------------------------------------------------------

command_gen(Mod,#state{parallel=false}=S) ->
    serial_command_gen(Mod,S);
command_gen(Mod,#state{parallel=true}=S) ->
    parallel_command_gen(Mod,S).

serial_command_gen(_Mod,#state{db=undefined, exists=false}) ->
    {call,?IMPL,open,[]};
serial_command_gen(_Mod,#state{db=undefined, exists=true}) ->
    oneof([{call,?IMPL,reopen,[]}
           %% @TODO {call,?IMPL,destroy,[]},
           %% @TODO {call,?IMPL,repair,[]}
          ]);
serial_command_gen(_Mod,#state{db=Db}=S) ->
    oneof([{call,?IMPL,close,[Db]},
           {call,?IMPL,put,[Db,gen_obj(S)]},
           {call,?IMPL,delete,[Db,gen_key(S)]},
           {call,?IMPL,get,[Db,gen_key(S)]},
           {call,?IMPL,first,[Db]},
           {call,?IMPL,last,[Db]},
           {call,?IMPL,next,[Db,gen_key(S)]},
           {call,?IMPL,prev,[Db,gen_key(S)]}
          ]).

parallel_command_gen(_Mod,#state{db=undefined}) ->
    {call,?IMPL,open,[]};
parallel_command_gen(_Mod,#state{db=Db}=S) ->
    oneof([{call,?IMPL,put,[Db,gen_obj(S)]},
           {call,?IMPL,delete,[Db,gen_key(S)]},
           {call,?IMPL,get,[Db,gen_key(S)]}
          ]).

-spec initial_state() -> #state{}.
initial_state() ->
    ?LET(Parallel,parameter(parallel,false),
         #state{parallel=Parallel}).

-spec state_is_sane(#state{}) -> boolean().
state_is_sane(_S) ->
    %% @TODO
    true.

-spec next_state(#state{}, term(), tuple()) -> #state{}.
next_state(#state{db=undefined, exists=false}=S, V, {call,_,open,[]}) ->
    S#state{db=V, exists=true};
next_state(#state{db=undefined, exists=true}=S, V, {call,_,reopen,[]}) ->
    S#state{db=V, exists=true};
next_state(#state{db=undefined, exists=true}=S, V, {call,_,destroy,[]}) ->
    S#state{db=V, exists=false, objs=[]};
next_state(#state{db=Db}=S, _V, {call,_,close,[Db]}) when Db /= undefined ->
    S#state{db=undefined};
next_state(S, _V, {call,_,put,[_Db,Obj]}) ->
    insert_obj(S, Obj);
next_state(S, _V, {call,_,delete,[_Db,Key]}) ->
    delete_obj(S, Key);
next_state(S, _V, {call,_,_,_}) ->
    S.

-spec precondition(#state{}, tuple()) -> boolean().
precondition(#state{exists=true}, {call,_,open,[]}) ->
    false;
precondition(#state{exists=false}, {call,_,reopen,[]}) ->
    false;
precondition(#state{exists=false}, {call,_,destroy,[]}) ->
    false;
precondition(#state{exists=false}, {call,_,repair,[]}) ->
    false;
precondition(#state{db=Db}, {call,_,open,[]}) when Db /= undefined->
    false;
precondition(#state{db=Db}, {call,_,reopen,[]}) when Db /= undefined->
    false;
precondition(#state{db=Db}, {call,_,destroy,[]}) when Db /= undefined->
    false;
precondition(#state{db=Db}, {call,_,repair,[]}) when Db /= undefined->
    false;
precondition(_S, {call,_,_,_}) ->
    true.

-spec postcondition(#state{}, tuple(), term()) -> boolean().
postcondition(#state{exists=false}, {call,_,open,[]}, Res) ->
    ?IMPL:is_db(Res);
postcondition(#state{exists=true}, {call,_,reopen,[]}, Res) ->
    ?IMPL:is_db(Res);
postcondition(#state{exists=true}, {call,_,destroy,[]}, Res) ->
    Res;
postcondition(#state{exists=true}, {call,_,repair,[]}, Res) ->
    Res;
postcondition(#state{db=Db}, {call,_,close,[_Db]}, Res) ->
    Res andalso Db /= undefined;
postcondition(_S, {call,_,put,[_Db,_]}, Res) ->
    Res;
postcondition(_S, {call,_,delete,[_Db,_]}, Res) ->
    Res;
postcondition(S, {call,_,get,[_Db,Key]}, Res) ->
    Res =:= get_val(S, Key);
postcondition(#state{objs=[]}, {call,_,first,[_Db]}, Res) ->
    Res;
postcondition(S, {call,_,first,[_Db]}, Res) ->
    #obj{key=K} = hd(sort_objs(S)),
    Res =:= K;
postcondition(#state{objs=[]}, {call,_,last,[_Db]}, Res) ->
    Res;
postcondition(S, {call,_,last,[_Db]}, Res) ->
    #obj{key=K} = hd(lists:reverse(sort_objs(S))),
    Res =:= K;
postcondition(S, {call,_,next,[_Db,Key]}, Res) ->
    case lists:dropwhile(fun(#obj{key=X}) -> X =< Key end, sort_objs(S)) of
        [] ->
            Res;
        [#obj{key=K}|_] ->
            Res =:= K
    end;
postcondition(S, {call,_,prev,[_Db,Key]}, Res) ->
    case lists:dropwhile(fun(#obj{key=X}) -> X >= Key end, rsort_objs(S)) of
        [] ->
            Res;
        [#obj{key=K}|_] ->
            Res =:= K
    end;
postcondition(_S, {call,_,_,_}, _Res) ->
    false.

-spec setup(boolean()) -> {ok, term()}.
setup(_Hard) ->
    ?IMPL:setup(),
    teardown(),
    {ok, unused}.

-spec teardown(term()) -> ok.
teardown(unused) ->
    teardown(),
    ok.

-spec teardown(term(), #state{}) -> ok.
teardown(Ref, _State) ->
    teardown(Ref).

-spec aggregate([{integer(), term(), term(), #state{}}])
               -> [{atom(), atom(), integer() | term()}].
aggregate(L) ->
    [ {Cmd,filter_reply(Reply)} || {_N,{set,_,{call,_,Cmd,_}},Reply,_State} <- L ].

filter_reply({'EXIT',{Err,_}}) ->
    {error,Err};
filter_reply(_) ->
    ok.


%%%----------------------------------------------------------------------
%%% Internal - Generators
%%%----------------------------------------------------------------------

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

teardown() ->
    ?IMPL:teardown().


-endif. %% -ifdef(EQC).
-endif. %% -ifdef(QC).
