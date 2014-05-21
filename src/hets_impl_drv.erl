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
-define(LETS_DELETE2,             16#03).
-define(LETS_DELETE3,             16#04).
-define(LETS_DELETE_ALL_OBJECTS2, 16#05).
-define(LETS_FIRST2,              16#06).
-define(LETS_FIRST_ITER2,         16#07).
-define(LETS_INFO_MEMORY1,        16#08).
-define(LETS_INFO_SIZE1,          16#09).
-define(LETS_INSERT3,             16#0A).
-define(LETS_INSERT4,             16#0B).
-define(LETS_INSERT_NEW3,         16#0C).
-define(LETS_INSERT_NEW4,         16#0D).
-define(LETS_LAST2,               16#0E).
-define(LETS_LAST_ITER2,          16#0F).
-define(LETS_LOOKUP3,             16#10).
-define(LETS_MEMBER3,             16#11).
-define(LETS_NEXT3,               16#12).
-define(LETS_NEXT_ITER3,          16#13).
-define(LETS_PREV3,               16#14).
-define(LETS_PREV_ITER3,          16#15).

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
    case erl_ddll:load_driver(Path, ?MODULE) of
        ok -> ok;
        {error, already_loaded} -> ok;
        {error, permanent} -> ok;
        {error, {open_error, _}=Err} ->
            FormattedErr = erl_ddll:format_error(Err),
            error_logger:error_msg("Failed to load the driver library " ++ ?MODULE_STRING ++ ". "
                                   ++ "Error: ~p, Path: ~p~n",
                                   [FormattedErr,
                                    filename:join(Path, ?MODULE)
                                   ]),
            erlang:exit({Err, FormattedErr})
    end,
    open_port({spawn, ?MODULE_STRING}, [binary]).

open(Tid, Opts) ->
    create(fun impl_open/6, Tid, Opts).

destroy(Tid, Opts) ->
    create(fun impl_destroy/6, Tid, Opts).

repair(Tid, Opts) ->
    create(fun impl_repair/6, Tid, Opts).

delete(#gen_tid{impl=Impl, impl_opts=Opts}) ->
    impl_delete(Impl, opts(db_write, Opts)).

delete(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    impl_delete(Impl, opts(db_write, Opts), encode(Type, Key)).

delete_all_objects(#gen_tid{impl=Impl, impl_opts=Opts}) ->
    impl_delete_all_objects(Impl, opts(db_write, Opts)).

first(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}) ->
    case impl_first(Impl, opts(db_read, Opts)) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            decode(Type, Key)
    end.

first_iter(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}) ->
    case impl_first_iter(Impl, opts(db_read, Opts)) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            decode(Type, Key)
    end.

last(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}) ->
    case impl_last(Impl, opts(db_read, Opts)) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            decode(Type, Key)
    end.

last_iter(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}) ->
    case impl_last_iter(Impl, opts(db_read, Opts)) of
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

insert(#gen_tid{keypos=KeyPos, type=Type, impl=Impl, impl_opts=Opts}, Object) when is_tuple(Object) ->
    Key = element(KeyPos, Object),
    Val = Object,
    impl_insert(Impl, opts(db_write, Opts), encode(Type, Key), encode(Type, Val));
insert(#gen_tid{keypos=KeyPos, type=Type, impl=Impl, impl_opts=Opts}, Objects) when is_list(Objects) ->
    List = [{encode(Type, element(KeyPos, Object)), encode(Type, Object)} || Object <- Objects ],
    impl_insert(Impl, opts(db_write, Opts), List).

insert_new(#gen_tid{keypos=KeyPos, type=Type, impl=Impl, impl_opts=Opts}, Object) when is_tuple(Object) ->
    Key = element(KeyPos, Object),
    Val = Object,
    impl_insert_new(Impl, opts(db_write, Opts), encode(Type, Key), encode(Type, Val));
insert_new(#gen_tid{keypos=KeyPos, type=Type, impl=Impl, impl_opts=Opts}, Objects) when is_list(Objects) ->
    List = [{encode(Type, element(KeyPos, Object)), encode(Type, Object)} || Object <- Objects ],
    impl_insert_new(Impl, opts(db_write, Opts), List).

lookup(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    case impl_lookup(Impl, opts(db_read, Opts), encode(Type, Key)) of
        '$end_of_table' ->
            [];
        Object when is_binary(Object) ->
            [decode(Type, Object)]
    end.

lookup_element(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key, Pos) ->
    Element =
        case impl_lookup(Impl, opts(db_read, Opts), encode(Type, Key)) of
            '$end_of_table' ->
                '$end_of_table';
            Object when is_binary(Object) ->
                decode(Type, Object)
        end,
    element(Pos, Element).

member(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    impl_member(Impl, opts(db_read, Opts), encode(Type, Key)).

next(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    case impl_next(Impl, opts(db_read, Opts), encode(Type, Key)) of
        '$end_of_table' ->
            '$end_of_table';
        Next ->
            decode(Type, Next)
    end.

next_iter(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    case impl_next_iter(Impl, opts(db_read, Opts), encode(Type, Key)) of
        '$end_of_table' ->
            '$end_of_table';
        Next ->
            decode(Type, Next)
    end.

prev(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    case impl_prev(Impl, opts(db_read, Opts), encode(Type, Key)) of
        '$end_of_table' ->
            '$end_of_table';
        Prev ->
            decode(Type, Prev)
    end.

prev_iter(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    case impl_prev_iter(Impl, opts(db_read, Opts), encode(Type, Key)) of
        '$end_of_table' ->
            '$end_of_table';
        Prev ->
            decode(Type, Prev)
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

create(Fun, #gen_tid{type=Type, protection=Protection}, Opts) ->
    DbOpts = opts(db, Opts),
    ReadOpts = opts(db_read, Opts),
    WriteOpts = opts(db_write, Opts),
    {value, {path,Path}, NewDbOpts} = lists:keytake(path, 1, DbOpts),
    Fun(Type, Protection, Path, NewDbOpts, ReadOpts, WriteOpts).

opts(_Opt, undefined) ->
    undefined;
opts(Key, Opts) ->
    proplists:get_value(Key, Opts, []).

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

impl_open(Type, Protection, Path, Opts, ReadOpts, WriteOpts) ->
    Impl = init(),
    true = call(Impl, {?LETS_OPEN6, Type, Protection, Path, Opts, ReadOpts, WriteOpts}),
    Impl.

impl_destroy(Type, Protection, Path, Opts, ReadOpts, WriteOpts) ->
    Impl = init(),
    true = call(Impl, {?LETS_DESTROY6, Type, Protection, Path, Opts, ReadOpts, WriteOpts}),
    _ = port_close(Impl),
    _ = erl_ddll:unload(?MODULE),
    true.

impl_repair(Type, Protection, Path, Opts, ReadOpts, WriteOpts) ->
    Impl = init(),
    true = call(Impl, {?LETS_REPAIR6, Type, Protection, Path, Opts, ReadOpts, WriteOpts}),
    _ = port_close(Impl),
    _ = erl_ddll:unload(?MODULE),
    true.

impl_delete(Impl, Opts) ->
    Res = call(Impl, {?LETS_DELETE2, Opts}),
    _ = port_close(Impl),
    _ = erl_ddll:unload(?MODULE),
    Res.

impl_delete(Impl, Opts, Key) ->
    call(Impl, {?LETS_DELETE3, Opts, Key}).

impl_delete_all_objects(Impl, Opts) ->
    call(Impl, {?LETS_DELETE_ALL_OBJECTS2, Opts}).

impl_first(Impl, Opts) ->
    call(Impl, {?LETS_FIRST2, Opts}).

impl_first_iter(Impl, Opts) ->
    call(Impl, {?LETS_FIRST_ITER2, Opts}).

impl_last(Impl, Opts) ->
    call(Impl, {?LETS_LAST2, Opts}).

impl_last_iter(Impl, Opts) ->
    call(Impl, {?LETS_LAST_ITER2, Opts}).

impl_info_memory(Impl) ->
    call(Impl, {?LETS_INFO_MEMORY1}).

impl_info_size(Impl) ->
    call(Impl, {?LETS_INFO_SIZE1}).

impl_insert(Impl, Opts, Key, Object) ->
    call(Impl, {?LETS_INSERT4, Opts, Key, Object}).

impl_insert(Impl, Opts, List) ->
    call(Impl, {?LETS_INSERT3, Opts, List}).

impl_insert_new(Impl, Opts, Key, Object) ->
    call(Impl, {?LETS_INSERT_NEW4, Opts, Key, Object}).

impl_insert_new(Impl, Opts, List) ->
    call(Impl, {?LETS_INSERT_NEW3, Opts, List}).

impl_lookup(Impl, Opts, Key) ->
    call(Impl, {?LETS_LOOKUP3, Opts, Key}).

impl_member(Impl, Opts, Key) ->
    call(Impl, {?LETS_MEMBER3, Opts, Key}).

impl_next(Impl, Opts, Key) ->
    call(Impl, {?LETS_NEXT3, Opts, Key}).

impl_next_iter(Impl, Opts, Key) ->
    call(Impl, {?LETS_NEXT_ITER3, Opts, Key}).

impl_prev(Impl, Opts, Key) ->
    call(Impl, {?LETS_PREV3, Opts, Key}).

impl_prev_iter(Impl, Opts, Key) ->
    call(Impl, {?LETS_PREV_ITER3, Opts, Key}).
