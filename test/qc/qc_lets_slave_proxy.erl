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

-module(qc_lets_slave_proxy).

%% API
-export([%% test
         teardown/1
         , is_table/1
         %% lets
         , new/2
         , new/3
         , destroy/3
         , repair/3
         , delete/1
         , delete/2
         , delete_all_objects/1
         , first/1
         , foldl/3
         , foldr/3
         , info/1
         , info/2
         , insert/2
         , insert_new/2
         , last/1
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
         , prev/2
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


%%%===================================================================
%%% API
%%%===================================================================

teardown(Name) ->
    stop_slave(),
    qc_lets_proxy:teardown(Name).

is_table(Tab) ->
    qc_lets_proxy:is_table(Tab).

new(Name, Options) ->
    call_slave(new, [Name, Options]).

new(_Tab, Name, Options) ->
    %% _Tab is to help control generators and shrinking
    new(Name, Options).

destroy(Tab, Name, Options) ->
    qc_lets_raw:destroy(Tab, Name, Options).

repair(Tab, Name, Options) ->
    qc_lets_raw:repair(Tab, Name, Options).

delete(Tab) ->
    qc_lets_proxy:delete(Tab).

delete(Tab, Key) ->
    qc_lets_proxy:delete(Tab, Key).

delete_all_objects(Tab) ->
    qc_lets_proxy:delete_all_objects(Tab).

first(Tab) ->
    qc_lets_proxy:first(Tab).

foldl(Function, Acc0, Tab) ->
    qc_lets_proxy:foldl(Function, Acc0, Tab).

foldr(Function, Acc0, Tab) ->
    qc_lets_proxy:foldr(Function, Acc0, Tab).

info(Tab) ->
    qc_lets_proxy:info(Tab).

info(Tab, Item) ->
    qc_lets_proxy:info(Tab, Item).

insert(Tab, ObjOrObjs) ->
    qc_lets_proxy:insert(Tab, ObjOrObjs).

insert_new(Tab, ObjOrObjs) ->
    qc_lets_proxy:insert_new(Tab, ObjOrObjs).

last(Tab) ->
    qc_lets_proxy:last(Tab).

lookup(Tab, Key) ->
    qc_lets_proxy:lookup(Tab, Key).

lookup_element(Tab, Key, Pos) ->
    qc_lets_proxy:lookup_element(Tab, Key, Pos).

match(Tab, Pattern) ->
    qc_lets_proxy:match(Tab, Pattern).

match(Tab, Pattern, Limit) ->
    qc_lets_proxy:match(Tab, Pattern, Limit).

match(Cont) ->
    qc_lets_proxy:match(Cont).

match_delete(Tab, Pattern) ->
    qc_lets_proxy:match_delete(Tab, Pattern).

match_object(Tab, Pattern) ->
    qc_lets_proxy:match_object(Tab, Pattern).

match_object(Tab, Pattern, Limit) ->
    qc_lets_proxy:match_object(Tab, Pattern, Limit).

match_object(Cont) ->
    qc_lets_proxy:match_object(Cont).

member(Tab, Key) ->
    qc_lets_proxy:member(Tab, Key).

next(Tab, Key) ->
    qc_lets_proxy:next(Tab, Key).

prev(Tab, Key) ->
    qc_lets_proxy:prev(Tab, Key).

select(Tab, Spec) ->
    qc_lets_proxy:select(Tab, Spec).

select(Tab, Spec, Limit) ->
    qc_lets_proxy:select(Tab, Spec, Limit).

select(Cont) ->
    qc_lets_proxy:select(Cont).

select_count(Tab, Spec) ->
    qc_lets_proxy:select_count(Tab, Spec).

select_delete(Tab, Spec) ->
    qc_lets_proxy:select_delete(Tab, Spec).

select_reverse(Tab, Spec) ->
    qc_lets_proxy:select_reverse(Tab, Spec).

select_reverse(Tab, Spec, Limit) ->
    qc_lets_proxy:select_reverse(Tab, Spec, Limit).

select_reverse(Cont) ->
    qc_lets_proxy:select_reverse(Cont).

tab2list(Tab) ->
    qc_lets_proxy:tab2list(Tab).


%%%===================================================================
%%% Internal
%%%===================================================================

stop_slave() ->
    qc_slave:stop_slave(?MODULE).

call_slave(Function, Args) ->
    Slave = qc_slave:restart_slave(?MODULE),
    case rpc:call(Slave, qc_lets_proxy, Function, Args) of
        {badrpc, {'EXIT', _}=Exit} ->
            Exit;
        {badrpc, _}=BadRpc ->
            BadRpc;
        Res ->
            Res
    end.
