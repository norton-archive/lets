%%% The MIT License
%%%
%%% Copyright (C) 2011-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
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

-module(hets_impl_drv).
-behaviour(gen_ets_ns).

-include("lets.hrl").

%% External exports
-export([open/2
         , destroy/2
         , repair/2
         , delete/1
         , delete/2
         , delete_all_objects/1
         , first/1
         , first_iter/1
         , info_memory/1
         , info_size/1
         , insert/2
         , insert_new/2
         , last/1
         , last_iter/1
         , lookup/2
         , lookup_element/3
         , member/2
         , next/2
         , next_iter/2
         , prev/2
         , prev_iter/2
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
    case erl_ddll:load_driver(Path, hets_impl_drv) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, permanent} -> ok;
        {error, {open_error, _}=Err} ->
            FormattedErr = erl_ddll:format_error(Err),
            error_logger:error_msg("Failed to load the driver library hets_impl_drv. "
                                   ++ "Error: ~p, Path: ~p~n",
                                   [FormattedErr,
                                    filename:join(Path, hets_impl_drv)
                                   ]),
            erlang:exit({Err, FormattedErr})
    end,
    open_port({spawn, "hets_impl_drv"}, [binary]).

open(Tid, Options) ->
    create(fun impl_open/6, Tid, Options).

destroy(Tid, Options) ->
    create(fun impl_destroy/6, Tid, Options).

repair(Tid, Options) ->
    create(fun impl_repair/6, Tid, Options).

delete(#gen_tid{impl=Impl}) ->
    impl_delete(Impl).

delete(#gen_tid{type=Type, impl=Impl}, Key) ->
    impl_delete(Impl, encode(Type, Key)).

delete_all_objects(#gen_tid{impl=Impl}) ->
    impl_delete_all_objects(Impl).

first(#gen_tid{type=Type, impl=Impl}) ->
    case impl_first(Impl) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            decode(Type, Key)
    end.

first_iter(#gen_tid{type=Type, impl=Impl}) ->
    case impl_first_iter(Impl) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            decode(Type, Key)
    end.

last(#gen_tid{type=Type, impl=Impl}) ->
    case impl_last(Impl) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            decode(Type, Key)
    end.

last_iter(#gen_tid{type=Type, impl=Impl}) ->
    case impl_last_iter(Impl) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            decode(Type, Key)
    end.

info_memory(#gen_tid{impl=Impl}) ->
    case impl_info_memory(Impl) of
        Memory when is_integer(Memory) ->
            erlang:round(Memory / erlang:system_info(wordsize));
        Else ->
            Else
    end.

info_size(#gen_tid{impl=Impl}) ->
    impl_info_size(Impl).

insert(#gen_tid{keypos=KeyPos, type=Type, impl=Impl}, Object) when is_tuple(Object) ->
    Key = element(KeyPos, Object),
    Val = Object,
    impl_insert(Impl, encode(Type, Key), encode(Type, Val));
insert(#gen_tid{keypos=KeyPos, type=Type, impl=Impl}, Objects) when is_list(Objects) ->
    List = [{encode(Type, element(KeyPos, Object)), encode(Type, Object)} || Object <- Objects ],
    impl_insert(Impl, List).

insert_new(#gen_tid{keypos=KeyPos, type=Type, impl=Impl}, Object) when is_tuple(Object) ->
    Key = element(KeyPos, Object),
    Val = Object,
    impl_insert_new(Impl, encode(Type, Key), encode(Type, Val));
insert_new(#gen_tid{keypos=KeyPos, type=Type, impl=Impl}, Objects) when is_list(Objects) ->
    List = [{encode(Type, element(KeyPos, Object)), encode(Type, Object)} || Object <- Objects ],
    impl_insert_new(Impl, List).

lookup(#gen_tid{type=Type, impl=Impl}, Key) ->
    case impl_lookup(Impl, encode(Type, Key)) of
        '$end_of_table' ->
            [];
        Object when is_binary(Object) ->
            [decode(Type, Object)]
    end.

lookup_element(#gen_tid{type=Type, impl=Impl}, Key, Pos) ->
    Element =
        case impl_lookup(Impl, encode(Type, Key)) of
            '$end_of_table' ->
                '$end_of_table';
            Object when is_binary(Object) ->
                decode(Type, Object)
        end,
    element(Pos, Element).

member(#gen_tid{type=Type, impl=Impl}, Key) ->
    impl_member(Impl, encode(Type, Key)).

next(#gen_tid{type=Type, impl=Impl}, Key) ->
    case impl_next(Impl, encode(Type, Key)) of
        '$end_of_table' ->
            '$end_of_table';
        Next ->
            decode(Type, Next)
    end.

next_iter(#gen_tid{type=Type, impl=Impl}, Key) ->
    case impl_next_iter(Impl, encode(Type, Key)) of
        '$end_of_table' ->
            '$end_of_table';
        Next ->
            decode(Type, Next)
    end.

prev(#gen_tid{type=Type, impl=Impl}, Key) ->
    case impl_prev(Impl, encode(Type, Key)) of
        '$end_of_table' ->
            '$end_of_table';
        Prev ->
            decode(Type, Prev)
    end.

prev_iter(#gen_tid{type=Type, impl=Impl}, Key) ->
    case impl_prev_iter(Impl, encode(Type, Key)) of
        '$end_of_table' ->
            '$end_of_table';
        Prev ->
            decode(Type, Prev)
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

create(Fun, #gen_tid{type=Type, protection=Protection}, Options) ->
    DbOptions = proplists:get_value(db, Options, []),
    ReadOptions = proplists:get_value(db_read, Options, []),
    WriteOptions = proplists:get_value(db_write, Options, []),
    {value, {path,Path}, NewDbOptions} = lists:keytake(path, 1, DbOptions),
    Fun(Type, Protection, Path, NewDbOptions, ReadOptions, WriteOptions).

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
    true = call(Impl, {?LETS_DESTROY6, Type, Protection, Path, Options, ReadOptions, WriteOptions}),
    _ = port_close(Impl),
    _ = erl_ddll:unload(hets_impl_drv),
    true.

impl_repair(Type, Protection, Path, Options, ReadOptions, WriteOptions) ->
    Impl = init(),
    true = call(Impl, {?LETS_REPAIR6, Type, Protection, Path, Options, ReadOptions, WriteOptions}),
    _ = port_close(Impl),
    _ = erl_ddll:unload(hets_impl_drv),
    true.

impl_delete(Impl) ->
    Res = call(Impl, {?LETS_DELETE1}),
    _ = port_close(Impl),
    _ = erl_ddll:unload(hets_impl_drv),
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
