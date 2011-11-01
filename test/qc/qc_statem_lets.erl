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

-module(qc_statem_lets).

-ifdef(QC).

%% qc_statem Callbacks
-behaviour(qc_statem).
-export([command_gen/2]).
-export([initial_state/0, state_is_sane/1, next_state/3, precondition/2, postcondition/3]).
-export([commands_setup/1, commands_teardown/1, commands_teardown/2]).

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

-record(obj, {key :: integer() | float() | binary(), val :: integer() | float() | binary()}).

-type obj() :: #obj{}.
-type ets_type() :: set | ordered_set.  %% default is set
-type ets_impl() :: drv | nif | ets.    %% default is drv

-record(state, {
          parallel=false :: boolean(),
          type=undefined :: undefined | ets_type(),
          impl=undefined :: undefined | ets_impl(),
          exists=false   :: boolean(),
          tab=undefined  :: undefined | tuple(),
          objs=[]        :: [obj()]
         }).


%%%----------------------------------------------------------------------
%%% qc_statem Callbacks
%%%----------------------------------------------------------------------

command_gen(Mod,#state{parallel=false}=S) ->
    serial_command_gen(Mod,S);
command_gen(Mod,#state{parallel=true}=S) ->
    parallel_command_gen(Mod,S).

serial_command_gen(_Mod,#state{tab=undefined, type=undefined, impl=undefined}=S) ->
    {call,?IMPL,new,[?TAB,gen_options(new,S)]};
serial_command_gen(_Mod,#state{tab=undefined}=S) ->
    {call,?IMPL,new,[undefined,?TAB,gen_options(new,S)]};
serial_command_gen(_Mod,#state{tab=Tab, type=Type}=S) ->
    %% @TODO insert/3, insert_new/3, delete/3, delete_all_objs/2 write_gen_options
    %% @TODO lookup/3 read_gen_options
    oneof([{call,?IMPL,insert,[Tab,oneof([gen_obj(S),gen_objs(S)])]}]
          ++ [{call,?IMPL,insert_new,[Tab,oneof([gen_obj(S),gen_objs(S)])]} || Type == ets]
          %% @TODO ++ [{call,?IMPL,delete,[Tab]}]
          ++ [{call,?IMPL,delete,[Tab]}]
          ++ [{call,?IMPL,delete,[Tab,gen_key(S)]}]
          ++ [{call,?IMPL,delete_all_objs,[Tab]} || Type == ets]
          ++ [{call,?IMPL,lookup,[Tab,gen_key(S)]}]
          ++ [{call,?IMPL,first,[Tab]}]
          ++ [{call,?IMPL,next,[Tab,gen_key(S)]}]
          %% @TODO info
          ++ [{call,?IMPL,tab2list,[Tab]}]
         ).

parallel_command_gen(_Mod,#state{tab=undefined, type=undefined, impl=undefined}=S) ->
    {call,?IMPL,new,[?TAB,gen_options(new,S)]};
parallel_command_gen(_Mod,#state{tab=Tab, type=Type}=S) ->
    %% @TODO insert/3, insert_new/3, delete_all_objs/2 write_gen_options
    %% @TODO lookup/3 read_gen_options
    oneof([{call,?IMPL,insert,[Tab,oneof([gen_obj(S),gen_objs(S)])]}]
          ++ [{call,?IMPL,insert_new,[Tab,oneof([gen_obj(S),gen_objs(S)])]} || Type == ets]
          ++ [{call,?IMPL,delete,[Tab,gen_key(S)]}]
          ++ [{call,?IMPL,delete_all_objs,[Tab]} || Type == ets]
          ++ [{call,?IMPL,lookup,[Tab,gen_key(S)]}]
          ++ [{call,?IMPL,first,[Tab]}]
          ++ [{call,?IMPL,next,[Tab,gen_key(S)]}]
         ).

-spec initial_state() -> #state{}.
initial_state() ->
    ?LET(Parallel,parameter(parallel,false),
         #state{parallel=Parallel}).

-spec state_is_sane(#state{}) -> boolean().
state_is_sane(_S) ->
    %% @TODO
    true.

-spec next_state(#state{}, term(), tuple()) -> #state{}.
next_state(#state{tab=undefined, type=undefined, impl=undefined}=S, V, {call,_,new,[?TAB,Options]}) ->
    %% @TODO Options
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
    S#state{type=Type, impl=Impl, exists=true, tab=V};
next_state(#state{tab=undefined}=S, V, {call,_,new,[_Tab,?TAB,Options]}) ->
    %% @TODO Options
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
    S#state{type=Type, impl=Impl, exists=true, tab=V};
next_state(#state{impl=Impl}=S, _V, {call,_,destroy,[_Tab,?TAB,_Options]})
  when Impl /= ets ->
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
next_state(#state{impl=ets}=S, _V, {call,_,delete_all_objs,[_Tab]}) ->
    S#state{objs=[]};
next_state(S, _V, {call,_,delete_all_objs,[_Tab]}) ->
    S;
next_state(S, _V, {call,_,_,_}) ->
    S.

-spec precondition(#state{}, tuple()) -> boolean().
precondition(#state{tab=undefined, type=undefined, impl=undefined}, {call,_,new,[?TAB,Options]}) ->
    L = proplists:get_value(db, Options, []),
    proplists:get_bool(create_if_missing, L) andalso proplists:get_bool(error_if_exists, L);
precondition(#state{tab=undefined, type=undefined, impl=undefined}, {call,_,new,[_Tab,?TAB,Options]}) ->
    L = proplists:get_value(db, Options, []),
    proplists:get_bool(create_if_missing, L) andalso proplists:get_bool(error_if_exists, L);
precondition(#state{tab=Tab}, {call,_,new,[?TAB,_Options]}) when Tab /= undefined ->
    false;
precondition(#state{tab=Tab}, {call,_,new,[_Tab,?TAB,_Options]}) when Tab /= undefined ->
    false;
precondition(_S, {call,_,_,_}) ->
    true.

-spec postcondition(#state{}, tuple(), term()) -> boolean().
postcondition(#state{tab=undefined}, {call,_,new,[?TAB,_Options]}, Res) ->
    ?IMPL:is_table(Res);
postcondition(_S, {call,_,new,[?TAB,_Options]}, Res) ->
    case Res of
        {'EXIT', {badarg, _}} ->
            true;
        _ ->
            false
    end;
postcondition(#state{tab=undefined}, {call,_,new,[_Tab,?TAB,_Options]}, Res) ->
    ?IMPL:is_table(Res);
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
postcondition(#state{impl=ets}=_S, {call,_,delete_all_objs,[_Tab]}, Res) ->
    Res =:= true;
postcondition(_S, {call,_,delete_all_objs,[_Tab]}, {'EXIT',{badarg,_}}) ->
    true;
postcondition(S, {call,_,lookup,[_Tab,Key]}, Res) ->
    Res =:= keyfind(Key, S);
postcondition(#state{objs=[]}, {call,_,first,[_Tab]}, Res) ->
    Res =:= '$end_of_table';
postcondition(#state{type=set}=S, {call,_,first,[_Tab]}, Res) ->
    keymember(Res, S);
postcondition(#state{type=ordered_set}=S, {call,_,first,[_Tab]}, Res) ->
    #obj{key=K} = hd(sort(S)),
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
postcondition(#state{type=ordered_set, objs=[]}, {call,_,next,[_Tab, _Key]}, Res) ->
    Res =:= '$end_of_table';
postcondition(#state{type=ordered_set}=S, {call,_,next,[_Tab, Key]}, Res) ->
    case lists:dropwhile(fun(#obj{key=X}) -> lteq(X, Key, S) end, sort(S)) of
        [] ->
            Res =:= '$end_of_table';
        [#obj{key=K}|_] ->
            Res =:= K
    end;
postcondition(#state{type=set}=S, {call,_,tab2list,[_Tab]}, Res) ->
    [] == (S#state.objs -- Res);
postcondition(#state{type=ordered_set}=S, {call,_,tab2list,[_Tab]}, Res) ->
    sort(S) =:= Res;
postcondition(_S, {call,_,_,_}, _Res) ->
    false.

-spec commands_setup(boolean()) -> {ok, term()}.
commands_setup(_Hard) ->
    ?IMPL:teardown(?TAB),
    {ok, unused}.

-spec commands_teardown(term()) -> ok.
commands_teardown(unused) ->
    ?IMPL:teardown(?TAB),
    ok.

-spec commands_teardown(term(), #state{}) -> ok.
commands_teardown(Ref, _State) ->
    commands_teardown(Ref).


%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------

gen_options(Op,#state{tab=undefined, type=undefined, impl=undefined}=S) ->
    ?LET({Type,Impl}, {gen_ets_type(), gen_ets_impl()},
         gen_options(Op,S#state{type=Type, impl=Impl}));
gen_options(Op,#state{type=Type, impl=drv=Impl}=S) ->
    [Type, public, named_table, {keypos,#obj.key}, Impl]
        ++ gen_leveldb_options(Op,S);
gen_options(Op,#state{type=Type, impl=nif=Impl}=S) ->
    [Type, public, named_table, {keypos,#obj.key}, Impl]
        ++ gen_leveldb_options(Op,S);
gen_options(_Op,#state{type=Type, impl=ets=Impl}) ->
    [Type, public, named_table, {keypos,#obj.key}, Impl].

gen_leveldb_options(Op,S) ->
    [gen_db_options(Op,S), gen_db_read_options(Op,S), gen_db_write_options(Op,S)].

gen_db_options(new,#state{exists=Exists}) ->
    ExistsOptions = if Exists -> []; true -> [create_if_missing, error_if_exists] end,
    ?LET(Options, ulist(gen_db_options()), {db, Options ++ ExistsOptions});
gen_db_options(_Op,_S) ->
    ?LET(Options, ulist(gen_db_options()), {db, Options}).

gen_db_read_options(_Op,_S) ->
    ?LET(Options, ulist(gen_db_read_options()), {db_read, Options}).

gen_db_write_options(_Op,_S) ->
    ?LET(Options, ulist(gen_db_write_options()), {db_write, Options}).

gen_db_options() ->
    oneof([paranoid_checks, {paranoid_checks,gen_boolean()}, {write_buffer_size,gen_pos_integer()}, {max_open_files,gen_pos_integer()}, {block_cache_size,gen_pos_integer()}, {block_size,gen_pos_integer()}, {block_restart_interval,gen_pos_integer()}]).

gen_db_read_options() ->
    oneof([verify_checksums, {verify_checksums,gen_boolean()}, fill_cache, {fill_cache,gen_boolean()}]).

gen_db_write_options() ->
    oneof([sync, {sync,gen_boolean()}]).

gen_boolean() ->
    oneof([true, false]).

gen_pos_integer() ->
    nat().

gen_ets_type() ->
    noshrink(oneof([set, ordered_set])).

gen_ets_impl() ->
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

keyfind(X, #state{objs=L}=S) ->
    lists:filter(fun(#obj{key=K}) -> eq(X, K, S) end, L).

keymember(X, S) ->
    [] /= keyfind(X, S).

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

sort(#state{impl=ets, objs=L}) ->
    lists:sort(L);
sort(#state{objs=L}) ->
    [ sext:decode(X) || X <- lists:sort([ sext:encode(Y) || Y <- L ]) ].

-endif. %% -ifdef(QC).
