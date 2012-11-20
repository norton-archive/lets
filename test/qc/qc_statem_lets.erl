%%% The MIT License
%%%
%%% Copyright (C) 2011-2012 by Joseph Wayne Norton <norton@alum.mit.edu>
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

-module(qc_statem_lets).

-ifdef(QC).

%% API
-export([qc_run/1, qc_run/2]).
-export([qc_sample/1]).
-export([qc_prop/1]).
-export([qc_counterexample/2]).
-export([qc_counterexample_read/2]).
-export([qc_counterexample_write/2]).

%% qc_statem Callbacks
-behaviour(qc_statem).
-export([scenario_gen/0, command_gen/1]).
-export([initial_state/1, state_is_sane/1, next_state/3, precondition/2, postcondition/3]).
-export([setup/0, setup/1, teardown/2, aggregate/1]).

%% DEBUG
-compile(export_all).

%% Implementation
-export([fold_helper/2, match31/3, match_object31/3, select31/3, select_reverse31/3]).

%% @NOTE For boilerplate exports, see "qc_statem.hrl"
-include_lib("qc/include/qc_statem.hrl").


%%%----------------------------------------------------------------------
%%% defines, types, records
%%%----------------------------------------------------------------------

%%-define(IMPL, qc_lets_raw).
%%-define(IMPL, qc_lets_proxy).
-define(IMPL, qc_lets_slave_proxy).

-define(TAB, ?MODULE).
-define(INT_KEYS, lists:seq(0,10)).
-define(FLOAT_KEYS, [float(Key) || Key <- ?INT_KEYS]).
-define(BINARY_KEYS, [term_to_binary(Key) || Key <- ?INT_KEYS]).

-type key() :: integer() | float() | binary() | atom().
-type val() :: integer() | float() | binary() | atom().

-record(obj, {
          key :: key(),
          val :: val()
         }).

-type obj() :: #obj{}.
-type ets_type() :: set | ordered_set.  %% default is set
-type ets_impl() :: drv | nif | ets.    %% default is drv

-record(state, {
          parallel=false :: boolean(),
          type           :: ets_type(),
          impl           :: ets_impl(),
          exists=false   :: boolean(),
          options=[]     :: proplists:proplist(),
          tab            :: tuple() | pid(),
          objs=[]        :: [obj()]
         }).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

qc_run(NumTests) ->
    qc_run(NumTests, []).

qc_run(NumTests, Options) ->
    qc_statem:qc_run(?MODULE, NumTests, Options).

qc_sample(Options) ->
    qc_statem:qc_sample(?MODULE, Options).

qc_prop(Options) ->
    qc_statem:qc_prop(?MODULE, Options).

qc_counterexample(Options, CounterExample) ->
    qc_statem:qc_counterexample(?MODULE, Options, CounterExample).

qc_counterexample_read(Options, FileName) ->
    qc_statem:qc_counterexample_read(?MODULE, Options, FileName).

qc_counterexample_write(FileName, CounterExample) ->
    qc_statem:qc_counterexample_write(FileName, CounterExample).


%%%----------------------------------------------------------------------
%%% qc_statem Callbacks
%%%----------------------------------------------------------------------
scenario_gen() ->
    undefined.

command_gen(#state{parallel=false}=S) ->
    serial_command_gen(S);
command_gen(#state{parallel=true}=S) ->
    parallel_command_gen(S).

serial_command_gen(#state{tab=undefined, type=undefined, impl=undefined}=S) ->
    {call,?IMPL,new,[?TAB,gen_options(new,S)]};
serial_command_gen(#state{tab=undefined}=S) ->
    oneof([{call,?IMPL,new,[undefined,?TAB,gen_options(new,S)]}]
          %% @TODO ++ [{call,?IMPL,destroy,[undefined,?TAB,gen_options(destroy,S)]}]
          %% @TODO ++ [{call,?IMPL,repair,[undefined,?TAB,gen_options(repair,S)]}]
         );
serial_command_gen(#state{tab=Tab, type=Type, impl=Impl}=S) ->
    %% @TODO gen_db_write_options/2
    %% @TODO gen_db_read_options/2
    %% @TODO info/1, info/2
    oneof([{call,?IMPL,all,[Tab]}]
          ++ [{call,?IMPL,insert,[Tab,oneof([gen_obj(S),gen_objs(S)])]}]
          ++ [{call,?IMPL,insert_new,[Tab,oneof([gen_obj(S),gen_objs(S)])]} || Impl =:= ets]
          ++ [{call,?IMPL,delete,[Tab]}]
          ++ [{call,?IMPL,delete,[Tab,gen_key(S)]}]
          ++ [{call,?IMPL,delete_all_objects,[Tab]} || Type =:= ets]
          ++ [{call,?IMPL,member,[Tab,gen_key(S)]}]
          ++ [{call,?IMPL,lookup,[Tab,gen_key(S)]}]
          ++ [{call,?IMPL,lookup_element,[Tab,gen_key(S),choose(1,record_info(size,obj))]}]
          ++ [{call,?IMPL,first,[Tab]}]
          ++ [{call,?IMPL,last,[Tab]}]
          ++ [{call,?IMPL,next,[Tab,gen_key(S)]}]
          ++ [{call,?IMPL,prev,[Tab,gen_key(S)]}]
          ++ [{call,?IMPL,foldl,[fun ?MODULE:fold_helper/2, [], Tab]}]
          ++ [{call,?IMPL,foldr,[fun ?MODULE:fold_helper/2, [], Tab]}]
          ++ [{call,?IMPL,tab2list,[Tab]}]
          ++ [{call,?IMPL,match,[Tab, gen_pattern(S)]}]
          ++ [{call,?MODULE,match31,[Tab, gen_pattern(S), gen_pos_integer()]}]
          ++ [{call,?IMPL,match_delete,[Tab, gen_pattern(S)]}]
          ++ [{call,?IMPL,match_object,[Tab, gen_pattern(S)]}]
          ++ [{call,?MODULE,match_object31,[Tab, gen_pattern(S), gen_pos_integer()]}]
          ++ [{call,?IMPL,select,[Tab, gen_spec(S)]}]
          ++ [{call,?MODULE,select31,[Tab, gen_spec(S), gen_pos_integer()]}]
          ++ [{call,?IMPL,select_count,[Tab, gen_spec_true(S)]}]
          ++ [{call,?IMPL,select_delete,[Tab, gen_spec_true(S)]}]
          ++ [{call,?IMPL,select_reverse,[Tab, gen_spec(S)]}]
          ++ [{call,?MODULE,select_reverse31,[Tab, gen_spec(S), gen_pos_integer()]}]
         ).

parallel_command_gen(#state{tab=undefined, type=undefined, impl=undefined}=S) ->
    {call,?IMPL,new,[?TAB,gen_options(new,S)]};
parallel_command_gen(#state{tab=Tab, impl=Impl}=S) ->
    %% @TODO gen_db_write_options/2
    %% @TODO gen_db_read_options/2
    oneof([{call,?IMPL,all,[Tab]}]
          ++ [{call,?IMPL,insert,[Tab,oneof([gen_obj(S),gen_objs(S)])]}]
          ++ [{call,?IMPL,insert_new,[Tab,oneof([gen_obj(S),gen_objs(S)])]} || Impl =:= ets]
          ++ [{call,?IMPL,delete,[Tab,gen_key(S)]}]
          ++ [{call,?IMPL,member,[Tab,gen_key(S)]}]
          ++ [{call,?IMPL,lookup,[Tab,gen_key(S)]}]
          ++ [{call,?IMPL,lookup_element,[Tab,gen_key(S),choose(1,record_info(size,obj))]}]
          ++ [{call,?IMPL,first,[Tab]}]
          ++ [{call,?IMPL,last,[Tab]}]
          ++ [{call,?IMPL,next,[Tab,gen_key(S)]}]
          ++ [{call,?IMPL,prev,[Tab,gen_key(S)]}]
         ).

fold_helper(X, Acc) ->
    [X|Acc].

-spec initial_state(term()) -> #state{}.
initial_state(_Scenario) ->
    ?LET(Parallel,parameter(parallel,false),
         #state{parallel=Parallel}).

-spec state_is_sane(#state{}) -> boolean().
state_is_sane(_S) ->
    %% @TODO
    true.

-spec next_state(#state{}, term(), tuple()) -> #state{}.
next_state(#state{tab=undefined, type=undefined, impl=undefined}=S, V, {call,_,new,[?TAB,Options]}) ->
    case [proplists:get_bool(X, Options) || X <- [set, ordered_set]] of
        [_, false] ->
            Type = set;
        [false, true] ->
            Type = ordered_set
    end,
    case [proplists:get_bool(X, Options) || X <- [drv, nif, ets]] of
        [_, false, false] ->
            Impl = drv;
        [false, true, _] ->
            Impl = nif;
        [false, false, true] ->
            Impl = ets
    end,
    S#state{type=Type, impl=Impl, exists=true, options=Options, tab=V};
next_state(#state{type=Type, impl=Impl, tab=undefined}=S, V, {call,_,new,[_Tab,?TAB,Options]}) ->
    case [proplists:get_bool(X, Options) || X <- [set, ordered_set]] of
        [_, false] ->
            Type = set;
        [false, true] ->
            Type = ordered_set
    end,
    case [proplists:get_bool(X, Options) || X <- [drv, nif, ets]] of
        [_, false, false] ->
            Impl = drv;
        [false, true, _] ->
            Impl = nif;
        [false, false, true] ->
            Impl = ets
    end,
    S#state{type=Type, impl=Impl, exists=true, options=Options, tab=V};
next_state(#state{impl=Impl}=S, _V, {call,_,destroy,[_Tab,?TAB,_Options]})
  when Impl =/= ets ->
    S#state{tab=undefined, exists=false, objs=[]};
next_state(S, _V, {call,_,insert,[_Tab,Objs]}) when is_list(Objs) ->
    insert_objs(S, Objs);
next_state(S, _V, {call,_,insert,[_Tab,Obj]}) ->
    insert_objs(S, [Obj]);
next_state(#state{impl=ets}=S, _V, {call,_,insert_new,[_Tab,Objs]}) when is_list(Objs) ->
    insert_new_objs(S, Objs);
next_state(#state{impl=ets}=S, _V, {call,_,insert_new,[_Tab,Obj]}) ->
    insert_new_objs(S, [Obj]);
next_state(S, _V, {call,_,insert_new,[_Tab,_ObjOrObjs]}) ->
    S;
next_state(#state{impl=ets}=S, _V, {call,_,delete,[_Tab]}) ->
    S#state{tab=undefined, exists=false, objs=[]};
next_state(#state{exists=Exists}=S, _V, {call,_,delete,[_Tab]}) ->
    S#state{tab=undefined, exists=Exists};
next_state(S, _V, {call,_,delete,[_Tab,Key]}) ->
    S#state{objs=keydelete(Key, S)};
next_state(#state{impl=ets}=S, _V, {call,_,delete_all_objects,[_Tab]}) ->
    S#state{objs=[]};
next_state(S, _V, {call,_,delete_all_objects,[_Tab]}) ->
    S;
next_state(S, _V, {call,_,match_delete,[_Tab,Pattern]}) ->
    match_delete(S, Pattern);
next_state(S, _V, {call,_,select_delete,[_Tab,Spec]}) ->
    select_delete(S, Spec);
next_state(S, _V, {call,_,_,_}) ->
    S.

-spec precondition(#state{}, tuple()) -> boolean().
precondition(#state{tab=undefined, type=undefined, impl=undefined}, {call,_,new,[?TAB,Options]}) ->
    Drv = proplists:get_bool(drv, Options),
    Nif = proplists:get_bool(nif, Options),
    if Drv orelse Nif ->
            L = proplists:get_value(db, Options, []),
            proplists:get_bool(create_if_missing, L) andalso proplists:get_bool(error_if_exists, L);
       true ->
            true
    end;
precondition(#state{tab=_Tab}, {call,_,new,[?TAB,_Options]}) ->
    false;
precondition(#state{tab=undefined, type=undefined, impl=undefined}, {call,_,new,[_Tab,?TAB,_Options]}) ->
    false;
precondition(#state{tab=Tab}, {call,_,new,[_Tab,?TAB,_Options]}) ->
    Tab =:= undefined;
precondition(#state{tab=undefined, type=undefined, impl=undefined}, {call,_,destroy,[_Tab,?TAB,_Options]}) ->
    false;
precondition(#state{tab=Tab}, {call,_,destroy,[_Tab,?TAB,_Options]}) ->
    Tab =:= undefined;
precondition(#state{tab=undefined, type=undefined, impl=undefined}, {call,_,repair,[_Tab,?TAB,_Options]}) ->
    false;
precondition(#state{tab=Tab}, {call,_,repair,[_Tab,?TAB,_Options]}) ->
    Tab =:= undefined;
precondition(_S, {call,_,_,_}) ->
    true.

-spec postcondition(#state{}, tuple(), term()) -> boolean().
postcondition(#state{tab=Tab}, {call,_,all,[_Tab]}, Res) ->
    if is_pid(Tab) ->
            true;
       true ->
            Res =:= [Tab]
    end;
postcondition(#state{tab=undefined}, {call,_,new,[?TAB,Options]}, Res) ->
    if is_pid(Res) ->
            true;
       true ->
            NamedTable = proplists:get_bool(named_table, Options),
            if NamedTable ->
                    Res =:= ?TAB;
               true ->
                    ?IMPL:is_table(Res)
            end
    end;
postcondition(_S, {call,_,new,[?TAB,_Options]}, Res) ->
    case Res of
        {'EXIT', {badarg, _}} ->
            true;
        _ ->
            false
    end;
postcondition(#state{tab=undefined}, {call,_,new,[_Tab,?TAB,Options]}, Res) ->
    if is_pid(Res) ->
            true;
       true ->
            NamedTable = proplists:get_bool(named_table, Options),
            if NamedTable ->
                    Res =:= ?TAB;
               true ->
                    ?IMPL:is_table(Res)
            end
    end;
postcondition(_S, {call,_,destroy,[_Tab,?TAB,_Options]}, Res) ->
    Res =:= true;
postcondition(_S, {call,_,repair,[_Tab,?TAB,_Options]}, Res) ->
    Res =:= true;
postcondition(_S, {call,_,insert,[_Tab,_ObjOrObjs]}, Res) ->
    Res =:= true;
postcondition(#state{impl=ets}=S, {call,_,insert_new,[_Tab,Objs]}, Res) when is_list(Objs) ->
    Res =:= has_insert_new_objs(S, Objs);
postcondition(#state{impl=ets}=S, {call,_,insert_new,[_Tab,Obj]}, Res) ->
    Res =:= has_insert_new_objs(S, [Obj]);
postcondition(_S, {call,_,insert_new,[_Tab,_ObjOrObjs]}, {'EXIT',{badarg,_}}) ->
    true;
postcondition(_S, {call,_,delete,[_Tab]}, Res) ->
    Res =:= true;
postcondition(_S, {call,_,delete,[_Tab,_Key]}, Res) ->
    Res =:= true;
postcondition(#state{impl=ets}=_S, {call,_,delete_all_objects,[_Tab]}, Res) ->
    Res =:= true;
postcondition(_S, {call,_,delete_all_objects,[_Tab]}, {'EXIT',{badarg,_}}) ->
    true;
postcondition(S, {call,_,member,[_Tab,Key]}, Res) ->
    Res =:= keymember(Key, S);
postcondition(S, {call,_,lookup,[_Tab,Key]}, Res) ->
    Res =:= keyfind(Key, S);
postcondition(S, {call,_,lookup_element,[_Tab,Key,_Pos]}, {'EXIT',{badarg,_}}) ->
    not keymember(Key, S);
postcondition(S, {call,_,lookup_element,[_Tab,Key,Pos]}, Res) ->
    [Res] =:= keyposfind(Key, Pos, S);
postcondition(#state{objs=[]}, {call,_,first,[_Tab]}, Res) ->
    Res =:= '$end_of_table';
postcondition(#state{type=set}=S, {call,_,first,[_Tab]}, Res) ->
    keymember(Res, S);
postcondition(#state{type=ordered_set}=S, {call,_,first,[_Tab]}, Res) ->
    #obj{key=K} = hd(sort(S)),
    Res =:= K;
postcondition(#state{objs=[]}, {call,_,last,[_Tab]}, Res) ->
    Res =:= '$end_of_table';
postcondition(#state{type=set}=S, {call,_,last,[_Tab]}, Res) ->
    keymember(Res, S);
postcondition(#state{type=ordered_set}=S, {call,_,last,[_Tab]}, Res) ->
    #obj{key=K} = hd(rsort(S)),
    Res =:= K;
postcondition(#state{impl=ets, type=set}=S, {call,_,next,[_Tab, Key]}, {'EXIT',{badarg,_}}) ->
    not keymember(Key, S);
postcondition(#state{impl=ets, type=set}=S, {call,_,next,[_Tab, Key]}, '$end_of_table') ->
    keymember(Key, S);
postcondition(#state{impl=ets, type=set}=S, {call,_,next,[_Tab, Key]}, Res) ->
    keymember(Key, S) andalso keymember(Res, S);
postcondition(#state{type=set}, {call,_,next,[_Tab, _Key]}, '$end_of_table') ->
    true;
postcondition(#state{type=set}=S, {call,_,next,[_Tab, _Key]}, Res) ->
    keymember(Res, S);
postcondition(#state{type=ordered_set}=S, {call,_,next,[_Tab, Key]}, Res) ->
    case lists:dropwhile(fun(#obj{key=X}) -> lteq(X, Key, S) end, sort(S)) of
        [] ->
            Res =:= '$end_of_table';
        [#obj{key=K}|_] ->
            Res =:= K
    end;
postcondition(#state{impl=ets, type=set}=S, {call,_,prev,[_Tab, Key]}, {'EXIT',{badarg,_}}) ->
    not keymember(Key, S);
postcondition(#state{impl=ets, type=set}=S, {call,_,prev,[_Tab, Key]}, '$end_of_table') ->
    keymember(Key, S);
postcondition(#state{impl=ets, type=set}=S, {call,_,prev,[_Tab, Key]}, Res) ->
    keymember(Key, S) andalso keymember(Res, S);
postcondition(#state{type=set}, {call,_,prev,[_Tab, _Key]}, '$end_of_table') ->
    true;
postcondition(#state{type=set}=S, {call,_,prev,[_Tab, _Key]}, Res) ->
    keymember(Res, S);
postcondition(#state{type=ordered_set}=S, {call,_,prev,[_Tab, Key]}, Res) ->
    case lists:dropwhile(fun(#obj{key=X}) -> gteq(X, Key, S) end, rsort(S)) of
        [] ->
            Res =:= '$end_of_table';
        [#obj{key=K}|_] ->
            Res =:= K
    end;
postcondition(#state{type=set}=S, {call,_,foldl,[_Function,_Acc0,_Tab]}, Res) ->
    [] =:= (S#state.objs -- Res);
postcondition(#state{type=ordered_set}=S, {call,_,foldl,[_Function,_Acc0,_Tab]}, Res) ->
    rsort(S) =:= Res;
postcondition(#state{type=set}=S, {call,_,foldr,[_Function,_Acc0,_Tab]}, Res) ->
    [] =:= (S#state.objs -- Res);
postcondition(#state{type=ordered_set}=S, {call,_,foldr,[_Function,_Acc0,_Tab]}, Res) ->
    sort(S) =:= Res;
postcondition(#state{type=set}=S, {call,_,tab2list,[_Tab]}, Res) ->
    [] =:= (S#state.objs -- Res);
postcondition(#state{type=ordered_set}=S, {call,_,tab2list,[_Tab]}, Res) ->
    sort(S) =:= Res;
postcondition(#state{type=set}=S, {call,_,match,[_Tab,Pattern]}, Res) ->
    [] =:= (match(S, Pattern) -- Res);
postcondition(#state{type=ordered_set}=S, {call,_,match,[_Tab,Pattern]}, Res) ->
    match(S, Pattern) =:= Res;
postcondition(#state{type=set}=S, {call,_,match31,[_Tab,Pattern,_Limit]}, Res) ->
    [] =:= (match(S, Pattern) -- Res);
postcondition(#state{type=ordered_set}=S, {call,_,match31,[_Tab,Pattern,_Limit]}, Res) ->
    match(S, Pattern) =:= Res;
postcondition(_S, {call,_,match_delete,[_Tab,_Pattern]}, Res) ->
    Res;
postcondition(#state{type=set}=S, {call,_,match_object,[_Tab,Pattern]}, Res) ->
    [] =:= (match_object(S, Pattern) -- Res);
postcondition(#state{type=ordered_set}=S, {call,_,match_object,[_Tab,Pattern]}, Res) ->
    match_object(S, Pattern) =:= Res;
postcondition(#state{type=set}=S, {call,_,match_object31,[_Tab,Pattern,_Limit]}, Res) ->
    [] =:= (match_object(S, Pattern) -- Res);
postcondition(#state{type=ordered_set}=S, {call,_,match_object31,[_Tab,Pattern,_Limit]}, Res) ->
    match_object(S, Pattern) =:= Res;
postcondition(#state{type=set}=S, {call,_,select,[_Tab,Spec]}, Res) ->
    [] =:= (select(S, Spec) -- Res);
postcondition(#state{type=ordered_set}=S, {call,_,select,[_Tab,Spec]}, Res) ->
    select(S, Spec) =:= Res;
postcondition(#state{type=set}=S, {call,_,select31,[_Tab,Spec,_Limit]}, Res) ->
    [] =:= (select(S, Spec) -- Res);
postcondition(#state{type=ordered_set}=S, {call,_,select31,[_Tab,Spec,_Limit]}, Res) ->
    select(S, Spec) =:= Res;
postcondition(S, {call,_,select_count,[_Tab,Spec]}, Res) ->
    select_count(S, Spec) =:= Res;
postcondition(S, {call,_,select_delete,[_Tab,Spec]}, Res) ->
    select_count(S, Spec) =:= Res;
postcondition(#state{type=set}=S, {call,_,select_reverse,[_Tab,Spec]}, Res) ->
    [] =:= (select_reverse(S, Spec) -- Res);
postcondition(#state{type=ordered_set}=S, {call,_,select_reverse,[_Tab,Spec]}, Res) ->
    select_reverse(S, Spec) =:= Res;
postcondition(#state{type=set}=S, {call,_,select_reverse31,[_Tab,Spec,_Limit]}, Res) ->
    [] =:= (select_reverse(S, Spec) -- Res);
postcondition(#state{type=ordered_set}=S, {call,_,select_reverse31,[_Tab,Spec,_Limit]}, Res) ->
    select_reverse(S, Spec) =:= Res;
postcondition(_S, {call,_,_,_}, _Res) ->
    false.

-spec setup() -> ok.
setup() ->
    ok.

-spec setup(term()) -> {ok, term()}.
setup(_Scenario) ->
    _ = ?IMPL:teardown(?TAB),
    {ok, undefined}.

-spec teardown(term(), #state{}) -> ok.
teardown(_Ref, _State) ->
    ok.

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

gen_options(Op,#state{type=undefined, impl=undefined, tab=undefined}=S) ->
    ?LET({Type,Impl}, {lets_type(), lets_impl()},
         gen_options(Op,S#state{type=Type, impl=Impl}));
gen_options(Op,#state{type=Type, impl=drv=Impl}=S) ->
    Defaults = [Type, Impl]
        ++ [public, {keypos,#obj.key}, {compressed, gen_boolean()}, {async, gen_boolean()}]
        ++ gen_leveldb_options(Op,S),
    oneof([Defaults, [named_table|Defaults]]);
gen_options(Op,#state{type=Type, impl=nif=Impl}=S) ->
    Defaults = [Type, Impl]
        ++ [public, {keypos,#obj.key}, {compressed, gen_boolean()}]
        ++ gen_leveldb_options(Op,S),
    oneof([Defaults, [named_table|Defaults]]);
gen_options(_Op,#state{type=Type, impl=ets=Impl}) ->
    Defaults = [Type, Impl]
        ++ [public, {keypos,#obj.key}, {compressed, gen_boolean()}],
    oneof([Defaults, [named_table|Defaults]]).

gen_leveldb_options(Op,S) ->
    [gen_db_options(Op,S), gen_db_read_options(Op,S), gen_db_write_options(Op,S)].

gen_db_options(new,#state{exists=Exists}) ->
    ExistsOptions = if Exists -> []; true -> [create_if_missing, error_if_exists] end,
    ?LET(Options,ulist(gen_db_options()), {db, ExistsOptions ++ Options});
gen_db_options(_,_) ->
    ?LET(Options,ulist(gen_db_options()), {db, Options}).

gen_db_read_options(_Op,_S) ->
    ?LET(Options, ulist(gen_db_read_options()), {db_read, Options}).

gen_db_write_options(_Op,_S) ->
    ?LET(Options, ulist(gen_db_write_options()), {db_write, Options}).

gen_db_options() ->
    oneof([paranoid_checks, {paranoid_checks,gen_boolean()}, {write_buffer_size,gen_pos_integer()}, {max_open_files,gen_pos_integer()}, {block_cache_size,gen_pos_integer()}, {block_size,gen_pos_integer()}, {block_restart_interval,gen_pos_integer()}, {filter_policy, oneof([no, {bloom,gen_pos_integer()}])}]).

gen_db_read_options() ->
    oneof([verify_checksums, {verify_checksums,gen_boolean()}, fill_cache, {fill_cache,gen_boolean()}]).

gen_db_write_options() ->
    oneof([sync, {sync,gen_boolean()}]).

gen_boolean() ->
    oneof([true, false]).

gen_pos_integer() ->
    ?LET(N, nat(), N+1).

lets_type() ->
    noshrink(oneof([set, ordered_set])).

lets_impl() ->
    %% @NOTE Remove one or two of these to restrict to a particular
    %% implementation.
    noshrink(oneof([drv,nif,ets])).

gen_integer_key() ->
    oneof(?INT_KEYS).

gen_float_key() ->
    oneof(?FLOAT_KEYS).

gen_binary_key() ->
    oneof(?BINARY_KEYS).

gen_key() ->
    frequency([{5, gen_integer_key()}, {1, gen_float_key()}, {1, gen_binary_key()}]).

gen_key(#state{objs=[]}) ->
    gen_key();
gen_key(#state{objs=Objs}) ->
    oneof([?LET(Obj, oneof(Objs), Obj#obj.key), gen_key()]).

gen_int_or_float_or_bin() ->
    frequency([{5, int()}, {1, real()}, {1, binary()}]).

gen_val() ->
    gen_int_or_float_or_bin().

gen_obj() ->
    #obj{key=gen_key(), val=gen_val()}.

gen_obj(#state{objs=[]}) ->
    gen_obj();
gen_obj(#state{objs=Objs}) ->
    oneof([oneof(Objs), gen_obj()]).

gen_objs(S) ->
    frequency([{9, non_empty(list(gen_obj(S)))}, {1, list(gen_obj(S))}]).

gen_pattern(S) ->
    oneof([{'$1', '$2', '$3'}
           , #obj{key=oneof(['_',gen_key(S)]), val='$1'}
           , #obj{key='$1', val=oneof(['_',gen_val()])}
          ]).

gen_spec(S) ->
    [{gen_pattern(S), [], [oneof(['$$', '$_'])]}].

gen_spec_true(S) ->
    [{gen_pattern(S), [], [true]}].


%%%----------------------------------------------------------------------
%%% Internal - Model
%%%----------------------------------------------------------------------

insert_objs(S, []) ->
    S;
insert_objs(S, [#obj{key=K}=Obj|T]) ->
    case keymember(K, S) of
        false ->
            insert_objs(S#state{objs=[Obj|S#state.objs]}, T);
        true ->
            insert_objs(S#state{objs=keyreplace(K, Obj, S)}, T)
    end.

insert_new_objs(S, L) ->
    insert_new_objs(S, lists:reverse(L), S).

insert_new_objs(S, [], _S0) ->
    S;
insert_new_objs(S, [#obj{key=K}=Obj|T], S0) ->
    case keymember(K, S) of
        false ->
            NewT = keydelete(K, T, S),
            insert_new_objs(S#state{objs=[Obj|S#state.objs]}, NewT, S0);
        true ->
            S0
    end.

has_insert_new_objs(S, L) ->
    has_insert_new_objs(S, lists:reverse(L), true).

has_insert_new_objs(_S, [], Bool) ->
    Bool;
has_insert_new_objs(S, [#obj{key=K}=Obj|T], _Bool) ->
    case keymember(K, S) of
        false ->
            NewT = keydelete(K, T, S),
            has_insert_new_objs(S#state{objs=[Obj|S#state.objs]}, NewT, true);
        true ->
            false
    end.

keydelete(X, #state{objs=L}=S) ->
    keydelete(X, L, S).

keydelete(X, L, S) ->
    lists:filter(fun(#obj{key=K}) -> neq(X, K, S) end, L).

keyreplace(X, Y, #state{objs=L}=S) ->
    lists:map(fun(Z=#obj{key=K}) -> case eq(X, K, S) of true -> Y; false -> Z end end, L).

keymember(X, S) ->
    [] =/= keyfind(X, S).

keyfind(X, #state{objs=L}=S) ->
    lists:filter(fun(#obj{key=K}) -> eq(X, K, S) end, L).

keyposfind(X, Pos, S) ->
    [ element(Pos, Obj) || Obj <- keyfind(X, S) ].

match(S, Pattern) ->
    select(S, [{Pattern, [], ['$$']}]).

match(S, Pattern, Limit) ->
    select(S, [{Pattern, [], ['$$']}], Limit).

match_cont(S, Pattern, Limit, StartKey) ->
    ContObjs = lists:dropwhile(fun(#obj{key=X}) -> lteq(X, StartKey, S) end, sort(S)),
    match(S#state{objs=ContObjs}, Pattern, Limit).

match_delete(#state{objs=L}=S, Pattern) ->
    S#state{objs=L -- match_object(S, Pattern)}.

match_object(S, Pattern) ->
    match_object(S, Pattern, undefined).

match_object(S, Pattern, Limit) ->
    select(S, [{Pattern, [], ['$_']}], Limit).

match_object_cont(S, Pattern, Limit, StartKey) ->
    ContObjs = lists:dropwhile(fun(#obj{key=X}) -> lteq(X, StartKey, S) end, sort(S)),
    match_object(S#state{objs=ContObjs}, Pattern, Limit).

match_object_reverse(S, Pattern) ->
    match_object_reverse(S, Pattern, undefined).

match_object_reverse(S, Pattern, Limit) ->
    select_reverse(S, [{Pattern, [], ['$_']}], Limit).

match_object_reverse_cont(S, Pattern, Limit, StartKey) ->
    ContObjs = lists:dropwhile(fun(#obj{key=X}) -> gteq(X, StartKey, S) end, rsort(S)),
    match_object_reverse(S#state{objs=ContObjs}, Pattern, Limit).

select(S, Spec) ->
    select(S, Spec, undefined).

select(S, Spec, Limit) ->
    case select1(S, sort(S), Spec) of
        [] ->
            [];
        Match when Limit =:= undefined ->
            Match;
        Match ->
            lists:sublist(Match, Limit)
    end.

select_cont(S, Spec, Limit, StartKey) ->
    ContObjs = lists:dropwhile(fun(#obj{key=X}) -> lteq(X, StartKey, S) end, sort(S)),
    select(S#state{objs=ContObjs}, Spec, Limit).

select_count(S, Spec) ->
    select1(S, sort(S), Spec).

select_delete(#state{objs=L}=S, [{Pattern, [], [true]}]) ->
    S#state{objs=L -- match_object(S, Pattern)}.

select_reverse(S, Spec) ->
    select_reverse(S, Spec, undefined).

select_reverse(S, Spec, Limit) ->
    case select1(S, rsort(S), Spec) of
        [] ->
            [];
        Match when Limit =:= undefined ->
            Match;
        Match ->
            lists:sublist(Match, Limit)
    end.

select_reverse_cont(S, Spec, Limit, StartKey) ->
    ContObjs = lists:dropwhile(fun(#obj{key=X}) -> gteq(X, StartKey, S) end, rsort(S)),
    select_reverse(S#state{objs=ContObjs}, Spec, Limit).

select1(#state{impl=ets}=S, L, Spec) ->
    select2(S#state{type=set}, L, Spec);
select1(S, L, Spec) ->
    select2(S, L, Spec).

%% simple and limited select implementation
select2(S, L, [{Pattern, [], [Result]}]) ->
    case Pattern of
        {'$1', '$2', '$3'} ->
            case Result of
                '$$' ->
                    [ [obj, X, Y] || #obj{key=X,val=Y} <- L ];
                '$_' ->
                    [ X || X <- L ];
                true ->
                    length(L)
            end;
        #obj{key='_', val='$1'} ->
            case Result of
                '$$' ->
                    [ [X] || #obj{val=X} <- L ];
                '$_' ->
                    [ X || X <- L ];
                true ->
                    length(L)
            end;
        #obj{key='$1', val='_'} ->
            case Result of
                '$$' ->
                    [ [X] || #obj{key=X} <- L ];
                '$_' ->
                    [ X || X <- L ];
                true ->
                    length(L)
            end;
        #obj{key=P, val='$1'} ->
            case Result of
                '$$' ->
                    [ [X] || #obj{key=Y, val=X} <- L, eq(Y, P, S) ];
                '$_' ->
                    [ X || #obj{key=Y}=X <- L, eq(Y, P, S) ];
                true ->
                    length([ X || #obj{key=Y}=X <- L, eq(Y, P, S) ])
            end;
        #obj{key='$1', val=P} ->
            case Result of
                '$$' ->
                    [ [X] || #obj{key=X, val=Y} <- L, eq(Y, P, S) ];
                '$_' ->
                    [ X || #obj{val=Y}=X <- L, eq(Y, P, S) ];
                true ->
                    length([ X || #obj{val=Y}=X <- L, eq(Y, P, S) ])
            end
    end.

eq(X, Y, #state{type=set, impl=ets}) ->
    X =:= Y;
eq(X, Y, #state{type=ordered_set, impl=ets}) ->
    X == Y;
eq(X, Y, #state{type=set}) ->
    term_to_binary(X) == term_to_binary(Y);
eq(X, Y, #state{type=ordered_set}) ->
    sext:encode(X) == sext:encode(Y).

neq(X, Y, #state{type=set, impl=ets}) ->
    X =/= Y;
neq(X, Y, #state{type=ordered_set, impl=ets}) ->
    X /= Y;
neq(X, Y, #state{type=set}) ->
    term_to_binary(X) /= term_to_binary(Y);
neq(X, Y, #state{type=ordered_set}) ->
    sext:encode(X) /= sext:encode(Y).

lteq(X, Y, #state{impl=ets}) ->
    X =< Y;
lteq(X, Y, #state{type=set}) ->
    term_to_binary(X) =< term_to_binary(Y);
lteq(X, Y, #state{type=ordered_set}) ->
    sext:encode(X) =< sext:encode(Y).

gteq(X, Y, #state{impl=ets}) ->
    X >= Y;
gteq(X, Y, #state{type=set}) ->
    term_to_binary(X) >= term_to_binary(Y);
gteq(X, Y, #state{type=ordered_set}) ->
    sext:encode(X) >= sext:encode(Y).

sort(#state{impl=ets, objs=L}) ->
    lists:sort(L);
sort(#state{objs=L}) ->
    [ sext:decode(X) || X <- lists:sort([ sext:encode(Y) || Y <- L ]) ].

rsort(S) ->
    lists:reverse(sort(S)).


%%%----------------------------------------------------------------------
%%% Internal - Implementation
%%%----------------------------------------------------------------------

%% @NOTE simplify test model by combining match/3 and match/1 into
%% single match31/3 command

match31(Tab, Pattern, Limit) ->
    catch match31(?IMPL:match(Tab, Pattern, Limit), Pattern, Limit, []).

match31('$end_of_table', _Pattern, _Limit, Acc) ->
    Acc;
match31({Match, Cont}, Pattern, Limit, Acc) when length(Match) =< Limit ->
    match31(?IMPL:match(Cont), Pattern, Limit, Acc ++ Match).

%% @NOTE simplify test model by combining match_object/3 and
%% match_object/1 into single match_object31/3 command

match_object31(Tab, Pattern, Limit) ->
    catch match_object31(?IMPL:match_object(Tab, Pattern, Limit), Pattern, Limit, []).

match_object31('$end_of_table', _Pattern, _Limit, Acc) ->
    Acc;
match_object31({Match, Cont}, Pattern, Limit, Acc) when length(Match) =< Limit ->
    match_object31(?IMPL:match_object(Cont), Pattern, Limit, Acc ++ Match).

%% @NOTE simplify test model by combining select/3 and select/1 into
%% single select31/3 command

select31(Tab, Spec, Limit) ->
    catch select31(?IMPL:select(Tab, Spec, Limit), Spec, Limit, []).

select31('$end_of_table', _Spec, _Limit, Acc) ->
    Acc;
select31({Match, Cont}, Spec, Limit, Acc) when length(Match) =< Limit ->
    select31(?IMPL:select(Cont), Spec, Limit, Acc ++ Match).

%% @NOTE simplify test model by combining select_reverse/3 and
%% select_reverse/1 into single select_reverse31/3 command

select_reverse31(Tab, Spec, Limit) ->
    catch select_reverse31(?IMPL:select_reverse(Tab, Spec, Limit), Spec, Limit, []).

select_reverse31('$end_of_table', _Spec, _Limit, Acc) ->
    Acc;
select_reverse31({Match, Cont}, Spec, Limit, Acc) when length(Match) =< Limit ->
    select_reverse31(?IMPL:select_reverse(Cont), Spec, Limit, Acc ++ Match).


-endif. %% -ifdef(QC).
