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

-module(hets_impl_nif).
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
        , notify/4
        , first/2
        , first_iter/2
        , last/2
        , last_iter/2
        , next/3
        , next_iter/3
        , prev/3
        , prev_iter/3
        ]).

-export_type([tid/0, opts/0, key/0, pos/0, object/0]).

-on_load(init/0).

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-type tid() :: lets:lets_tid().
-type opts() :: lets:opts().
-type key() :: lets:key().
-type pos() :: lets:pos().
-type object() :: lets:object().

-define(NIF_STUB, nif_stub_error(?LINE)).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @see lets:new/2
-spec open(tid(), opts()) -> tid().
open(Tid, Opts) ->
    Tid#gen_tid{impl=create(fun impl_open/6, Tid, Opts)}.

%% @see lets:destroy/2
-spec destroy(tid(), opts()) -> true.
destroy(Tid, Opts) ->
    create(fun impl_destroy/6, Tid, Opts).

%% @see lets:repair/2
-spec repair(tid(), opts()) -> true.
repair(Tid, Opts) ->
    create(fun impl_repair/6, Tid, Opts).

%% @see lets:delete/1
-spec delete(tid()) -> true.
delete(#gen_tid{impl=Impl, impl_opts=Opts}) ->
    impl_delete(Impl, opts(db_write, Opts)).

%% @see lets:delete/2
-spec delete(tid(), key()) -> true.
delete(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    impl_delete(Impl, opts(db_write, Opts), encode(Type, Key)).

%% @see lets:delete_all_objects/1
-spec delete_all_objects(tid()) -> true.
delete_all_objects(#gen_tid{impl=Impl, impl_opts=Opts}) ->
    impl_delete_all_objects(Impl, opts(db_write, Opts)).

%% @see lets:first/1
-spec first(tid()) -> key() | '$end_of_table'.
first(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}) ->
    decode(Type, impl_first(Impl, opts(db_read, Opts), undefined)).

%% @see lets:first/1
-spec first_iter(tid()) -> object() | '$end_of_table'.
first_iter(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}) ->
    decode(Type, impl_first_iter(Impl, opts(db_read, Opts), undefined)).

%% @see lets:last/1
-spec last(tid()) -> key() | '$end_of_table'.
last(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}) ->
    decode(Type, impl_last(Impl, opts(db_read, Opts), undefined)).

%% @see lets:last/1
-spec last_iter(tid()) -> object() | '$end_of_table'.
last_iter(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}) ->
    decode(Type, impl_last_iter(Impl, opts(db_read, Opts), undefined)).

%% @see lets:info/1
-spec info_memory(tid()) -> non_neg_integer().
info_memory(#gen_tid{impl=Impl}) ->
    case impl_info_memory(Impl) of
        Memory when is_integer(Memory) ->
            erlang:round(Memory / erlang:system_info(wordsize));
        Else ->
            Else
    end.

%% @see lets:info/1
-spec info_size(tid()) -> non_neg_integer().
info_size(#gen_tid{impl=Impl}) ->
    impl_info_size(Impl).

%% @see lets:insert/2
-spec insert(tid(), object() | [object()]) -> true.
insert(#gen_tid{keypos=KeyPos, type=Type, impl=Impl, impl_opts=Opts}, Object) when is_tuple(Object) ->
    Key = element(KeyPos, Object),
    Val = Object,
    impl_insert(Impl, opts(db_write, Opts), encode(Type, Key), encode(Type, Val));
insert(#gen_tid{keypos=KeyPos, type=Type, impl=Impl, impl_opts=Opts}, Objects) when is_list(Objects) ->
    List = [{encode(Type, element(KeyPos, Object)), encode(Type, Object)} || Object <- Objects ],
    impl_insert(Impl, opts(db_write, Opts), List).

%% @see lets:insert_new/2
-spec insert_new(tid(), object() | [object()]) -> true.
insert_new(#gen_tid{keypos=KeyPos, type=Type, impl=Impl, impl_opts=Opts}, Object) when is_tuple(Object) ->
    Key = element(KeyPos, Object),
    Val = Object,
    impl_insert_new(Impl, opts(db_write, Opts), encode(Type, Key), encode(Type, Val));
insert_new(#gen_tid{keypos=KeyPos, type=Type, impl=Impl, impl_opts=Opts}, Objects) when is_list(Objects) ->
    List = [{encode(Type, element(KeyPos, Object)), encode(Type, Object)} || Object <- Objects ],
    impl_insert_new(Impl, opts(db_write, Opts), List).

%% @see lets:lookup/2
-spec lookup(tid(), key()) -> [object()].
lookup(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    case impl_lookup(Impl, opts(db_read, Opts), encode(Type, Key)) of
        '$end_of_table' ->
            [];
        Object when is_binary(Object) ->
            [decode(Type, Object)]
    end.

%% @see lets:lookup_element/3
-spec lookup_element(tid(), key(), pos()) -> term().
lookup_element(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key, Pos) ->
    Element =
        case impl_lookup(Impl, opts(db_read, Opts), encode(Type, Key)) of
            '$end_of_table' ->
                '$end_of_table';
            Object when is_binary(Object) ->
                decode(Type, Object)
        end,
    element(Pos, Element).

%% @see lets:member/2
-spec member(tid(), key()) -> true | false.
member(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    impl_member(Impl, opts(db_read, Opts), encode(Type, Key)).

%% @see lets:next/2
-spec next(#gen_tid{}, key()) -> key() | '$end_of_table'.
next(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    decode(Type, impl_next(Impl, opts(db_read, Opts), encode(Type, Key), undefined)).

%% @see lets:next/2
-spec next_iter(tid(), key()) -> object() | '$end_of_table'.
next_iter(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    decode(Type, impl_next_iter(Impl, opts(db_read, Opts), encode(Type, Key), undefined)).

%% @see lets:prev/2
-spec prev(#gen_tid{}, key()) -> key() | '$end_of_table'.
prev(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    decode(Type, impl_prev(Impl, opts(db_read, Opts), encode(Type, Key), undefined)).

%% @see lets:prev/2
-spec prev_iter(tid(), key()) -> object() | '$end_of_table'.
prev_iter(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key) ->
    decode(Type, impl_prev_iter(Impl, opts(db_read, Opts), encode(Type, Key), undefined)).

%% @doc Register the specified process to be sent the specified
%% message when the table is destroyed and return true.  Otherwise,
%% return false.
-spec notify(tid(), Event::when_destroyed, Pid::pid(), Msg::term()) -> true | false.
notify(#gen_tid{impl=Impl}, Event, Pid, Msg) ->
    impl_notify(Impl, Event, Pid, Msg).


%% @see lets:first/1
-spec first(tid(), pos_integer()) -> [key()] | '$end_of_table'.
first(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, N) ->
    decode(Type, impl_first(Impl, opts(db_read, Opts), N)).

%% @see lets:first/1
-spec first_iter(tid(), pos_integer()) -> [object()] | '$end_of_table'.
first_iter(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, N) ->
    decode(Type, impl_first_iter(Impl, opts(db_read, Opts), N)).

%% @see lets:last/1
-spec last(tid(), pos_integer()) -> [key()] | '$end_of_table'.
last(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, N) ->
    decode(Type, impl_last(Impl, opts(db_read, Opts), N)).

%% @see lets:last/1
-spec last_iter(tid(), pos_integer()) -> [object()] | '$end_of_table'.
last_iter(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, N) ->
    decode(Type, impl_last_iter(Impl, opts(db_read, Opts), N)).

%% @see lets:next/2
-spec next(#gen_tid{}, key(), pos_integer()) -> [key()] | '$end_of_table'.
next(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key, N) ->
    decode(Type, impl_next(Impl, opts(db_read, Opts), encode(Type, Key), N)).

%% @see lets:next/2
-spec next_iter(tid(), key(), pos_integer()) -> [object()] | '$end_of_table'.
next_iter(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key, N) ->
    decode(Type, impl_next_iter(Impl, opts(db_read, Opts), encode(Type, Key), N)).

%% @see lets:prev/2
-spec prev(#gen_tid{}, key(), pos_integer()) -> [key()] | '$end_of_table'.
prev(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key, N) ->
    decode(Type, impl_prev(Impl, opts(db_read, Opts), encode(Type, Key), N)).

%% @see lets:prev/2
-spec prev_iter(tid(), key(), pos_integer()) -> [object()] | '$end_of_table'.
prev_iter(#gen_tid{type=Type, impl=Impl, impl_opts=Opts}, Key, N) ->
    decode(Type, impl_prev_iter(Impl, opts(db_read, Opts), encode(Type, Key), N)).

%%%----------------------------------------------------------------------
%%% Internal functions
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

decode(_, '$end_of_table') ->
    '$end_of_table';
decode(set, List) when is_list(List) ->
    [ binary_to_term(Term) || Term <- List ];
decode(ordered_set, List) when is_list(List) ->
    [ sext:decode(Term) || Term <- List ];
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

impl_first(_Impl, _Opts, _N) ->
    ?NIF_STUB.

impl_first_iter(_Impl, _Opts, _N) ->
    ?NIF_STUB.

impl_last(_Impl, _Opts, _N) ->
    ?NIF_STUB.

impl_last_iter(_Impl, _Opts, _N) ->
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

impl_next(_Impl, _Opts, _Key, _N) ->
    ?NIF_STUB.

impl_next_iter(_Impl, _Opts, _Key, _N) ->
    ?NIF_STUB.

impl_prev(_Impl, _Opts, _Key, _N) ->
    ?NIF_STUB.

impl_prev_iter(_Impl, _Opts, _Key, _N) ->
    ?NIF_STUB.

impl_notify(_Impl, _Event, _Pid, _Msg) ->
    ?NIF_STUB.
