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

-module(lets_impl_nif).
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

-on_load(init/0).

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-define(NIF_STUB, nif_stub_error(?LINE)).

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
    erlang:load_nif(filename:join(Path, ?MODULE_STRING), 0).

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

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

impl_open(_Type, _Protection, _Path, _Opts, _ReadOpts, _WriteOpts) ->
    ?NIF_STUB.

impl_destroy(_Type, _Protection, _Path, _Opts, _ReadOpts, _WriteOpts) ->
    ?NIF_STUB.

impl_repair(_Type, _Protection, _Path, _Opts, _ReadOpts, _WriteOpts) ->
    ?NIF_STUB.

impl_delete(_Impl, _Opts) ->
    ?NIF_STUB.

impl_delete(_Impl, _Opts, _Key) ->
    ?NIF_STUB.

impl_delete_all_objects(_Impl, _Opts) ->
    ?NIF_STUB.

impl_first(_Impl, _Opts) ->
    ?NIF_STUB.

impl_first_iter(_Impl, _Opts) ->
    ?NIF_STUB.

impl_last(_Impl, _Opts) ->
    ?NIF_STUB.

impl_last_iter(_Impl, _Opts) ->
    ?NIF_STUB.

impl_info_memory(_Impl) ->
    ?NIF_STUB.

impl_info_size(_Impl) ->
    ?NIF_STUB.

impl_insert(_Impl, _Opts, _Key, _Object) ->
    ?NIF_STUB.

impl_insert(_Impl, _Opts, _List) ->
    ?NIF_STUB.

impl_insert_new(_Impl, _Opts, _Key, _Object) ->
    ?NIF_STUB.

impl_insert_new(_Impl, _Opts, _List) ->
    ?NIF_STUB.

impl_lookup(_Impl, _Opts, _Key) ->
    ?NIF_STUB.

impl_member(_Impl, _Opts, _Key) ->
    ?NIF_STUB.

impl_next(_Impl, _Opts, _Key) ->
    ?NIF_STUB.

impl_next_iter(_Impl, _Opts, _Key) ->
    ?NIF_STUB.

impl_prev(_Impl, _Opts, _Key) ->
    ?NIF_STUB.

impl_prev_iter(_Impl, _Opts, _Key) ->
    ?NIF_STUB.
