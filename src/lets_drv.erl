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

-module(lets_drv).

-include("lets.hrl").

%% External exports
-export([open/4
         , destroy/4
         , repair/4
         , delete/1
         , delete/2
         , delete_all_objects/1
         , first/1
         %% , first_iter/1
         , foldl/3
         , foldr/3
         %% , nfoldl/4
         %% , nfoldr/4
         %% , nfoldl/1
         %% , nfoldr/1
         , info_memory/1
         , info_size/1
         , insert/2
         , insert_new/2
         , last/1
         %% , last_iter/1
         , lookup/2
         , lookup_element/3
         , match/2
         , match/3
         , match/1
         , match_delete/2
         , match_object/2
         , match_object/3
         , match_object/1
         , member/2
         , next/2
         %% , next_iter/2
         , prev/2
         %% , prev_iter/2
         , select/2
         , select/3
         , select/1
         , select_count/2
         , select_delete/2
         , select_reverse/2
         , select_reverse/3
         , select_reverse/1
         , tab2list/1
        ]).


%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-define(LETS_BADARG,              16#00).
-define(LETS_TRUE,                16#01).
-define(LETS_FALSE,               16#02).
-define(LETS_END_OF_TABLE,        16#03).
-define(LETS_BINARY,              16#04).

-define(LETS_OPEN6,               16#00).
-define(LETS_DESTROY6,            16#01).
-define(LETS_REPAIR6,             16#02).
-define(LETS_DELETE1,             16#03).
-define(LETS_DELETE2,             16#04).
-define(LETS_DELETE_ALL_OBJECTS1, 16#05).
-define(LETS_FIRST1,              16#06).
-define(LETS_FIRST_ITER1,         16#07).
-define(LETS_INFO_MEMORY1,        16#08).
-define(LETS_INFO_SIZE1,          16#09).
-define(LETS_INSERT2,             16#0A).
-define(LETS_INSERT3,             16#0B).
-define(LETS_INSERT_NEW2,         16#0C).
-define(LETS_INSERT_NEW3,         16#0D).
-define(LETS_LAST1,               16#0E).
-define(LETS_LAST_ITER1,          16#0F).
-define(LETS_LOOKUP2,             16#10).
-define(LETS_MEMBER2,             16#11).
-define(LETS_NEXT2,               16#12).
-define(LETS_NEXT_ITER2,          16#13).
-define(LETS_PREV2,               16#14).
-define(LETS_PREV_ITER2,          16#15).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

init() ->
    Path =
        case code:priv_dir(lets) of
            {error, bad_name} ->
                "../priv/lib";
            Dir ->
                filename:join([Dir, "lib"])
        end,
    case erl_ddll:load_driver(Path, lets_drv) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, permanent} -> ok;
        {error, {open_error, _}=Err} ->
            FormattedErr = erl_ddll:format_error(Err),
            error_logger:error_msg("Failed to load the driver library lets_drv. "
                                   ++ "Error: ~p, Path: ~p~n",
                                   [FormattedErr,
                                    filename:join(Path, lets_drv)
                                   ]),
            erlang:exit({Err, FormattedErr})
    end,
    open_port({spawn, "lets_drv"}, [binary]).

open(#tab{name=_Name, named_table=_Named, type=Type, protection=Protection}=Tab, Options, ReadOptions, WriteOptions) ->
    {value, {path,Path}, NewOptions} = lists:keytake(path, 1, Options),
    Impl = impl_open(Type, Protection, Path, NewOptions, ReadOptions, WriteOptions),
    %% @TODO implement named Impl (of sorts)
    Tab#tab{impl=Impl}.

destroy(#tab{type=Type, protection=Protection}, Options, ReadOptions, WriteOptions) ->
    {value, {path,Path}, NewOptions} = lists:keytake(path, 1, Options),
    impl_destroy(Type, Protection, Path, NewOptions, ReadOptions, WriteOptions).

repair(#tab{type=Type, protection=Protection}, Options, ReadOptions, WriteOptions) ->
    {value, {path,Path}, NewOptions} = lists:keytake(path, 1, Options),
    impl_repair(Type, Protection, Path, NewOptions, ReadOptions, WriteOptions).

delete(#tab{impl=Impl}) ->
    impl_delete(Impl).

delete(#tab{type=Type, impl=Impl}, Key) ->
    impl_delete(Impl, encode(Type, Key)).

delete_all_objects(#tab{impl=Impl}) ->
    impl_delete_all_objects(Impl).

first(#tab{type=Type, impl=Impl}) ->
    case impl_first(Impl) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            decode(Type, Key)
    end.

first_iter(#tab{type=Type, impl=Impl}) ->
    case impl_first_iter(Impl) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            decode(Type, Key)
    end.

last(#tab{type=Type, impl=Impl}) ->
    case impl_last(Impl) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            decode(Type, Key)
    end.

last_iter(#tab{type=Type, impl=Impl}) ->
    case impl_last_iter(Impl) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            decode(Type, Key)
    end.

info_memory(#tab{impl=Impl}) ->
    case impl_info_memory(Impl) of
        Memory when is_integer(Memory) ->
            erlang:round(Memory / erlang:system_info(wordsize));
        Else ->
            Else
    end.

info_size(#tab{impl=Impl}) ->
    impl_info_size(Impl).

insert(#tab{keypos=KeyPos, type=Type, impl=Impl}, Object) when is_tuple(Object) ->
    Key = element(KeyPos, Object),
    Val = Object,
    impl_insert(Impl, encode(Type, Key), encode(Type, Val));
insert(#tab{keypos=KeyPos, type=Type, impl=Impl}, Objects) when is_list(Objects) ->
    List = [{encode(Type, element(KeyPos, Object)), encode(Type, Object)} || Object <- Objects ],
    impl_insert(Impl, List).

insert_new(#tab{keypos=KeyPos, type=Type, impl=Impl}, Object) when is_tuple(Object) ->
    Key = element(KeyPos, Object),
    Val = Object,
    impl_insert_new(Impl, encode(Type, Key), encode(Type, Val));
insert_new(#tab{keypos=KeyPos, type=Type, impl=Impl}, Objects) when is_list(Objects) ->
    List = [{encode(Type, element(KeyPos, Object)), encode(Type, Object)} || Object <- Objects ],
    impl_insert_new(Impl, List).

lookup(#tab{type=Type, impl=Impl}, Key) ->
    case impl_lookup(Impl, encode(Type, Key)) of
        '$end_of_table' ->
            [];
        Object when is_binary(Object) ->
            [decode(Type, Object)]
    end.

lookup_element(#tab{type=Type, impl=Impl}, Key, Pos) ->
    Element =
        case impl_lookup(Impl, encode(Type, Key)) of
            '$end_of_table' ->
                '$end_of_table';
            Object when is_binary(Object) ->
                decode(Type, Object)
        end,
    element(Pos, Element).

member(#tab{type=Type, impl=Impl}, Key) ->
    impl_member(Impl, encode(Type, Key)).

next(#tab{type=Type, impl=Impl}, Key) ->
    case impl_next(Impl, encode(Type, Key)) of
        '$end_of_table' ->
            '$end_of_table';
        Next ->
            decode(Type, Next)
    end.

next_iter(#tab{type=Type, impl=Impl}, Key) ->
    case impl_next_iter(Impl, encode(Type, Key)) of
        '$end_of_table' ->
            '$end_of_table';
        Next ->
            decode(Type, Next)
    end.

prev(#tab{type=Type, impl=Impl}, Key) ->
    case impl_prev(Impl, encode(Type, Key)) of
        '$end_of_table' ->
            '$end_of_table';
        Prev ->
            decode(Type, Prev)
    end.

prev_iter(#tab{type=Type, impl=Impl}, Key) ->
    case impl_prev_iter(Impl, encode(Type, Key)) of
        '$end_of_table' ->
            '$end_of_table';
        Prev ->
            decode(Type, Prev)
    end.

foldl(Fun, Acc0, Tab) ->
    foldl(Fun, Acc0, Tab, first_iter(Tab)).

foldr(Fun, Acc0, Tab) ->
    foldr(Fun, Acc0, Tab, last_iter(Tab)).

nfoldl(Fun, Acc0, Tab, Limit) when Limit > 0 ->
    nfoldl(Fun, Acc0, Acc0, Tab, Limit, Limit, first_iter(Tab));
nfoldl(_Fun, _Acc0, _Tab, Limit) ->
    exit({badarg,Limit}).

nfoldl('$end_of_table') ->
    '$end_of_table';
nfoldl({_Fun, _Acc0, _Tab, _Limit0, '$end_of_table'}) ->
    '$end_of_table';
nfoldl({Fun, Acc0, Tab, Limit0, Key}) ->
    nfoldl(Fun, Acc0, Acc0, Tab, Limit0, Limit0, next_iter(Tab, Key)).

nfoldr(Fun, Acc0, Tab, Limit) when Limit > 0 ->
    nfoldr(Fun, Acc0, Acc0, Tab, Limit, Limit, last_iter(Tab));
nfoldr(_Fun, _Acc0, _Tab, Limit) ->
    exit({badarg,Limit}).

nfoldr('$end_of_table') ->
    '$end_of_table';
nfoldr({_Fun, _Acc0, _Tab, _Limit0, '$end_of_table'}) ->
    '$end_of_table';
nfoldr({Fun, Acc0, Tab, Limit0, Key}) ->
    nfoldr(Fun, Acc0, Acc0, Tab, Limit0, Limit0, prev_iter(Tab, Key)).

tab2list(Tab) ->
    foldr(fun(X, Acc) -> [X|Acc] end, [], Tab).

match(Tab, Pattern) ->
    select(Tab, [{Pattern, [], ['$$']}]).

match(Tab, Pattern, Limit) ->
    select(Tab, [{Pattern, [], ['$$']}], Limit).

match(Cont) ->
    select(Cont).

match_delete(Tab, Pattern) ->
    select_delete(Tab, [{Pattern, [], [true]}]),
    true.

match_object(Tab, Pattern) ->
    select(Tab, [{Pattern, [], ['$_']}]).

match_object(Tab, Pattern, Limit) ->
    select(Tab, [{Pattern, [], ['$_']}], Limit).

match_object(Cont) ->
    select(Cont).

select(Tab, Spec) ->
    Fun = fun(_Object, Match, Acc) -> [Match|Acc] end,
    selectr(Fun, [], Tab, Spec).

select(Tab, Spec, Limit) ->
    Fun = fun(_Object, Match, Acc) -> [Match|Acc] end,
    case nselectl(Fun, [], Tab, Spec, Limit) of
        {Acc, Cont} ->
            {lists:reverse(Acc), Cont};
        Cont ->
            Cont
    end.

select(Cont0) ->
    case nselectl(Cont0) of
        {Acc, Cont} ->
            {lists:reverse(Acc), Cont};
        Cont ->
            Cont
    end.

select_count(Tab, Spec) ->
    Fun = fun(_Object, true, Acc) ->
                  Acc + 1;
             (_Object, _Match, Acc) ->
                  Acc
          end,
    selectl(Fun, 0, Tab, Spec).

select_delete(#tab{keypos=KeyPos}=Tab, Spec) ->
    Fun = fun(Object, true, Acc) ->
                  Key = element(KeyPos, Object),
                  delete(Tab, Key),
                  Acc + 1;
             (_Object, _Match, Acc) ->
                  Acc
          end,
    selectl(Fun, 0, Tab, Spec).

select_reverse(Tab, Spec) ->
    Fun = fun(_Object, Match, Acc) -> [Match|Acc] end,
    selectl(Fun, [], Tab, Spec).

select_reverse(Tab, Spec, Limit) ->
    Fun = fun(_Object, Match, Acc) -> [Match|Acc] end,
    case nselectr(Fun, [], Tab, Spec, Limit) of
        {Acc, Cont} ->
            {lists:reverse(Acc), Cont};
        Cont ->
            Cont
    end.

select_reverse(Cont0) ->
    case nselectr(Cont0) of
        {Acc, Cont} ->
            {lists:reverse(Acc), Cont};
        Cont ->
            Cont
    end.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

foldl(_Fun, Acc, _Tab, '$end_of_table') ->
    Acc;
foldl(Fun, Acc, #tab{keypos=KeyPos}=Tab, Object) ->
    Key = element(KeyPos, Object),
    foldl(Fun, Fun(Object, Acc), Tab, next_iter(Tab, Key)).

foldr(_Fun, Acc, _Tab, '$end_of_table') ->
    Acc;
foldr(Fun, Acc, #tab{keypos=KeyPos}=Tab, Object) ->
    Key = element(KeyPos, Object),
    foldr(Fun, Fun(Object, Acc), Tab, prev_iter(Tab, Key)).

nfoldl(_Fun, Acc0, Acc0, _Tab, _Limit0, _Limit, '$end_of_table') ->
    '$end_of_table';
nfoldl(_Fun, _Acc0, Acc, _Tab, _Limit0, _Limit, '$end_of_table'=Cont) ->
    {Acc, Cont};
nfoldl(Fun, Acc0, Acc, #tab{keypos=KeyPos}=Tab, Limit0, Limit, Object) ->
    Key = element(KeyPos, Object),
    case Fun(Object, Acc) of
        {true, NewAcc} ->
            if Limit > 1 ->
                    nfoldl(Fun, Acc0, NewAcc, Tab, Limit0, Limit-1, next_iter(Tab, Key));
               true ->
                    Cont = {Fun, Acc0, Tab, Limit0, Key},
                    {NewAcc, Cont}
            end;
        {false, NewAcc} ->
            nfoldl(Fun, Acc0, NewAcc, Tab, Limit0, Limit, next_iter(Tab, Key))
    end.

nfoldr(_Fun, Acc0, Acc0, _Tab, _Limit0, _Limit, '$end_of_table') ->
    '$end_of_table';
nfoldr(_Fun, _Acc0, Acc, _Tab, _Limit0, _Limit, '$end_of_table'=Cont) ->
    {Acc, Cont};
nfoldr(Fun, Acc0, Acc, #tab{keypos=KeyPos}=Tab, Limit0, Limit, Object) ->
    Key = element(KeyPos, Object),
    case Fun(Object, Acc) of
        {true, NewAcc} ->
            if Limit > 1 ->
                    nfoldr(Fun, Acc0, NewAcc, Tab, Limit0, Limit-1, prev_iter(Tab, Key));
               true ->
                    Cont = {Fun, Acc0, Tab, Limit0, Key},
                    {NewAcc, Cont}
            end;
        {false, NewAcc} ->
            nfoldr(Fun, Acc0, NewAcc, Tab, Limit0, Limit, prev_iter(Tab, Key))
    end.

selectl(Fun, Acc0, Tab, Spec) ->
    foldl(selectfun(Fun, Spec), Acc0, Tab).

selectr(Fun, Acc0, Tab, Spec) ->
    foldr(selectfun(Fun, Spec), Acc0, Tab).

nselectl(Fun, Acc0, Tab, Spec, Limit0) ->
    nfoldl(nselectfun(Fun, Spec), Acc0, Tab, Limit0).

nselectr(Fun, Acc0, Tab, Spec, Limit0) ->
    nfoldr(nselectfun(Fun, Spec), Acc0, Tab, Limit0).

nselectl(Cont) ->
    nfoldl(Cont).

nselectr(Cont) ->
    nfoldr(Cont).

selectfun(Fun, Spec) ->
    CMSpec = ets:match_spec_compile(Spec),
    fun(Object, Acc) ->
            case ets:match_spec_run([Object], CMSpec) of
                [] ->
                    Acc;
                [Match] ->
                    Fun(Object, Match, Acc)
            end
    end.

nselectfun(Fun, Spec) ->
    CMSpec = ets:match_spec_compile(Spec),
    fun(Object, Acc) ->
            case ets:match_spec_run([Object], CMSpec) of
                [] ->
                    {false, Acc};
                [Match] ->
                    {true, Fun(Object, Match, Acc)}
            end
    end.

encode(set, Term) ->
    term_to_binary(Term);
encode(ordered_set, Term) ->
    sext:encode(Term).

decode(set, Term) ->
    binary_to_term(Term);
decode(ordered_set, Term) ->
    sext:decode(Term).

call(Impl, Tuple) ->
    Data = term_to_binary(Tuple),
    port_command(Impl, Data),
    receive
        {Impl, ?LETS_BINARY, Reply} ->
            Reply;
        {Impl, ?LETS_TRUE} ->
            true;
        {Impl, ?LETS_FALSE} ->
            false;
        {Impl, ?LETS_END_OF_TABLE} ->
            '$end_of_table';
        {Impl, ?LETS_BADARG} ->
            erlang:error(badarg, [Impl])
    end.

impl_open(Type, Protection, Path, Options, ReadOptions, WriteOptions) ->
    Impl = init(),
    true = call(Impl, {?LETS_OPEN6, Type, Protection, Path, Options, ReadOptions, WriteOptions}),
    Impl.

impl_destroy(Type, Protection, Path, Options, ReadOptions, WriteOptions) ->
    Impl = init(),
    true = call(Impl, {?LETS_OPEN6, Type, Protection, Path, Options, ReadOptions, WriteOptions}),
    _ = port_close(Impl),
    _ = erl_ddll:unload(lets_drv),
    true.

impl_repair(Type, Protection, Path, Options, ReadOptions, WriteOptions) ->
    Impl = init(),
    true = call(Impl, {?LETS_REPAIR6, Type, Protection, Path, Options, ReadOptions, WriteOptions}),
    _ = port_close(Impl),
    _ = erl_ddll:unload(lets_drv),
    true.

impl_delete(Impl) ->
    Res = call(Impl, {?LETS_DELETE1}),
    _ = port_close(Impl),
    _ = erl_ddll:unload(lets_drv),
    Res.

impl_delete(Impl, Key) ->
    call(Impl, {?LETS_DELETE2, Key}).

impl_delete_all_objects(Impl) ->
    call(Impl, {?LETS_DELETE_ALL_OBJECTS1}).

impl_first(Impl) ->
    call(Impl, {?LETS_FIRST1}).

impl_first_iter(Impl) ->
    call(Impl, {?LETS_FIRST_ITER1}).

impl_last(Impl) ->
    call(Impl, {?LETS_LAST1}).

impl_last_iter(Impl) ->
    call(Impl, {?LETS_LAST_ITER1}).

impl_info_memory(Impl) ->
    call(Impl, {?LETS_INFO_MEMORY1}).

impl_info_size(Impl) ->
    call(Impl, {?LETS_INFO_SIZE1}).

impl_insert(Impl, Key, Object) ->
    call(Impl, {?LETS_INSERT3, Key, Object}).

impl_insert(Impl, List) ->
    call(Impl, {?LETS_INSERT2, List}).

impl_insert_new(Impl, Key, Object) ->
    call(Impl, {?LETS_INSERT_NEW3, Key, Object}).

impl_insert_new(Impl, List) ->
    call(Impl, {?LETS_INSERT_NEW2, List}).

impl_lookup(Impl, Key) ->
    call(Impl, {?LETS_LOOKUP2, Key}).

impl_member(Impl, Key) ->
    call(Impl, {?LETS_MEMBER2, Key}).

impl_next(Impl, Key) ->
    call(Impl, {?LETS_NEXT2, Key}).

impl_next_iter(Impl, Key) ->
    call(Impl, {?LETS_NEXT_ITER2, Key}).

impl_prev(Impl, Key) ->
    call(Impl, {?LETS_PREV2, Key}).

impl_prev_iter(Impl, Key) ->
    call(Impl, {?LETS_PREV_ITER2, Key}).
