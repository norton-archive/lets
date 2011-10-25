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

-module(lets_nif).

-include("lets.hrl").

%% External exports
-export([open/4
         , destroy/4
         , repair/4
         , insert/3
         , insert_new/3
         , delete/2
         , delete/3
         , delete_all_objects/2
         , lookup/3
         , first/2
         , next/3
         , info_memory/2
         , info_size/2
         , tab2list/2
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
    erlang:load_nif(filename:join(Path, "lets_nif"), 0).

open(#tab{name=_Name, named_table=_Named, type=Type, protection=Protection}=Tab, Options, ReadOptions, WriteOptions) ->
    {value, {path,Path}, NewOptions} = lists:keytake(path, 1, Options),
    Nif = impl_open(Type, Protection, Path, NewOptions, ReadOptions, WriteOptions),
    %% @TODO implement named Nif (of sorts)
    Tab#tab{nif=Nif}.

destroy(#tab{type=Type, protection=Protection}, Options, ReadOptions, WriteOptions) ->
    {value, {path,Path}, NewOptions} = lists:keytake(path, 1, Options),
    impl_destroy(Type, Protection, Path, NewOptions, ReadOptions, WriteOptions).

repair(#tab{type=Type, protection=Protection}, Options, ReadOptions, WriteOptions) ->
    {value, {path,Path}, NewOptions} = lists:keytake(path, 1, Options),
    impl_repair(Type, Protection, Path, NewOptions, ReadOptions, WriteOptions).

insert(#tab{keypos=KeyPos, type=Type}, Nif, Object) when is_tuple(Object) ->
    Key = element(KeyPos,Object),
    Val = Object,
    impl_insert(Nif, encode(Type, Key), encode(Type, Val));
insert(#tab{keypos=KeyPos, type=Type}, Nif, Objects) when is_list(Objects) ->
    List = [{encode(Type, element(KeyPos,Object)), encode(Type, Object)} || Object <- Objects ],
    impl_insert(Nif, List).

insert_new(#tab{keypos=KeyPos, type=Type}, Nif, Object) when is_tuple(Object) ->
    Key = element(KeyPos,Object),
    Val = Object,
    impl_insert_new(Nif, encode(Type, Key), encode(Type, Val));
insert_new(#tab{keypos=KeyPos, type=Type}, Nif, Objects) when is_list(Objects) ->
    List = [{encode(Type, element(KeyPos,Object)), encode(Type, Object)} || Object <- Objects ],
    impl_insert_new(Nif, List).

delete(_Tab, Nif) ->
    impl_delete(Nif).

delete(#tab{type=Type}, Nif, Key) ->
    impl_delete(Nif, encode(Type, Key)).

delete_all_objects(_Tab, Nif) ->
    impl_delete_all_objects(Nif).

lookup(#tab{type=Type}, Nif, Key) ->
    case impl_lookup(Nif, encode(Type, Key)) of
        true ->
            [];
        Object when is_binary(Object) ->
            [decode(Type, Object)]
    end.

first(#tab{type=Type}, Nif) ->
    case impl_first(Nif) of
        '$end_of_table' ->
            '$end_of_table';
        Key ->
            decode(Type, Key)
    end.

next(#tab{type=Type}, Nif, Key) ->
    case impl_next(Nif, encode(Type, Key)) of
        '$end_of_table' ->
            '$end_of_table';
        Next ->
            decode(Type, Next)
    end.

info_memory(_Tab, Nif) ->
    case impl_info_memory(Nif) of
        Memory when is_integer(Memory) ->
            erlang:round(Memory / erlang:system_info(wordsize));
        Else ->
            Else
    end.

info_size(_Tab, Nif) ->
    impl_info_size(Nif).

tab2list(Tab, Nif) ->
    tab2list(Tab, Nif, impl_first(Nif), []).

tab2list(_Tab, _Nif, '$end_of_table', Acc) ->
    lists:reverse(Acc);
tab2list(#tab{type=Type}=Tab, Nif, Key, Acc) ->
    NewAcc =
        case impl_lookup(Nif, Key) of
            true ->
                %% @NOTE This is not an atomic operation
                Acc;
            Object when is_binary(Object) ->
                [decode(Type, Object)|Acc]
        end,
    tab2list(Tab, Nif, impl_next(Nif, Key), NewAcc).


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

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

impl_open(_Type, _Protection, _Path, _Options, _ReadOptions, _WriteOptions) ->
    ?NIF_STUB.

impl_destroy(_Type, _Protection, _Path, _Options, _ReadOptions, _WriteOptions) ->
    ?NIF_STUB.

impl_repair(_Type, _Protection, _Path, _Options, _ReadOptions, _WriteOptions) ->
    ?NIF_STUB.

impl_insert(_Nif, _Key, _Object) ->
    ?NIF_STUB.

impl_insert(_Nif, _List) ->
    ?NIF_STUB.

impl_insert_new(_Nif, _Key, _Object) ->
    ?NIF_STUB.

impl_insert_new(_Nif, _List) ->
    ?NIF_STUB.

impl_delete(_Nif) ->
    ?NIF_STUB.

impl_delete(_Nif, _Key) ->
    ?NIF_STUB.

impl_delete_all_objects(_Nif) ->
    ?NIF_STUB.

impl_lookup(_Nif, _Key) ->
    ?NIF_STUB.

impl_first(_Nif) ->
    ?NIF_STUB.

impl_next(_Nif, _Key) ->
    ?NIF_STUB.

impl_info_memory(_Nif) ->
    ?NIF_STUB.

impl_info_size(_Nif) ->
    ?NIF_STUB.
