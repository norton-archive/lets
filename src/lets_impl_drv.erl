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

-module(lets_impl_drv).
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

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-type tid() :: lets:lets_tid().
-type opts() :: lets:opts().
-type key() :: lets:key().
-type pos() :: lets:pos().
-type object() :: lets:object().

-define(LETS_BADARG,              16#00).
-define(LETS_TRUE,                16#01).
-define(LETS_FALSE,               16#02).
-define(LETS_END_OF_TABLE,        16#03).
-define(LETS_BINARY,              16#04).
-define(LETS_SPEC,                16#05).

-define(LETS_OPEN6,               16#00).
-define(LETS_DESTROY6,            16#01).
-define(LETS_REPAIR6,             16#02).
-define(LETS_DELETE2,             16#03).
-define(LETS_DELETE3,             16#04).
-define(LETS_DELETE_ALL_OBJECTS2, 16#05).
-define(LETS_FIRST3,              16#06).
-define(LETS_FIRST_ITER3,         16#07).
-define(LETS_INFO_MEMORY1,        16#08).
-define(LETS_INFO_SIZE1,          16#09).
-define(LETS_INSERT3,             16#0A).
-define(LETS_INSERT4,             16#0B).
-define(LETS_INSERT_NEW3,         16#0C).
-define(LETS_INSERT_NEW4,         16#0D).
-define(LETS_LAST3,               16#0E).
-define(LETS_LAST_ITER3,          16#0F).
-define(LETS_LOOKUP3,             16#10).
-define(LETS_MEMBER3,             16#11).
-define(LETS_NEXT4,               16#12).
-define(LETS_NEXT_ITER4,          16#13).
-define(LETS_PREV4,               16#14).
-define(LETS_PREV_ITER4,          16#15).
-define(LETS_NOTIFY4,             16#16).

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
%% return false.  Currently, the specified process must be same as
%% calling process.  If not, a badarg error is raised.
-spec notify(tid(), Event::when_destroyed, Pid::pid(), Msg::term()) -> true | false.
notify(#gen_tid{impl=Impl}, Event, Pid, Msg) when Pid==self() ->
    impl_notify(Impl, Event, Pid, term_to_binary(Msg));
notify(Tid, Event, Pid, Msg) ->
    erlang:error(badarg, [Tid, Event, Pid, Msg]).

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

call(Impl, Tuple) ->
    Data = term_to_binary(Tuple),
    port_command(Impl, Data),
    receive
        {Impl, ?LETS_BINARY, Reply} ->
            Reply;
        {Impl, ?LETS_SPEC, Reply} ->
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
    Ref = make_ref(),
    IsReg = impl_notify(Impl, when_destroyed, self(), term_to_binary(Ref)),
    true = call(Impl, {?LETS_DESTROY6, Type, Protection, Path, Opts, ReadOpts, WriteOpts}),
    _ = port_close(Impl),
    _ = if IsReg -> receive Ref -> ok end; true -> ok end,
    _ = erl_ddll:unload(?MODULE),
    true.

impl_repair(Type, Protection, Path, Opts, ReadOpts, WriteOpts) ->
    Impl = init(),
    Ref = make_ref(),
    IsReg = impl_notify(Impl, when_destroyed, self(), term_to_binary(Ref)),
    true = call(Impl, {?LETS_REPAIR6, Type, Protection, Path, Opts, ReadOpts, WriteOpts}),
    _ = port_close(Impl),
    _ = if IsReg -> receive Ref -> ok end; true -> ok end,
    _ = erl_ddll:unload(?MODULE),
    true.

impl_delete(Impl, Opts) ->
    Ref = make_ref(),
    IsReg = impl_notify(Impl, when_destroyed, self(), term_to_binary(Ref)),
    Res = call(Impl, {?LETS_DELETE2, Opts}),
    _ = port_close(Impl),
    _ = if IsReg -> receive Ref -> ok end; true -> ok end,
    _ = erl_ddll:unload(?MODULE),
    Res.

impl_delete(Impl, Opts, Key) ->
    call(Impl, {?LETS_DELETE3, Opts, Key}).

impl_delete_all_objects(Impl, Opts) ->
    call(Impl, {?LETS_DELETE_ALL_OBJECTS2, Opts}).

impl_first(Impl, Opts, N) ->
    call(Impl, {?LETS_FIRST3, Opts, N}).

impl_first_iter(Impl, Opts, N) ->
    call(Impl, {?LETS_FIRST_ITER3, Opts, N}).

impl_last(Impl, Opts, N) ->
    call(Impl, {?LETS_LAST3, Opts, N}).

impl_last_iter(Impl, Opts, N) ->
    call(Impl, {?LETS_LAST_ITER3, Opts, N}).

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

impl_next(Impl, Opts, Key, N) ->
    call(Impl, {?LETS_NEXT4, Opts, Key, N}).

impl_next_iter(Impl, Opts, Key, N) ->
    call(Impl, {?LETS_NEXT_ITER4, Opts, Key, N}).

impl_prev(Impl, Opts, Key, N) ->
    call(Impl, {?LETS_PREV4, Opts, Key, N}).

impl_prev_iter(Impl, Opts, Key, N) ->
    call(Impl, {?LETS_PREV_ITER4, Opts, Key, N}).

impl_notify(Impl, Event, Pid, Msg) ->
    call(Impl, {?LETS_NOTIFY4, Event, Pid, Msg}).
