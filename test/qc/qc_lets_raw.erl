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

-module(qc_lets_raw).

-include("../../src/lets.hrl").

%% API
-export([%% test
         teardown/1
         , is_table/1
         %% lets
         , all/1
         , tid/1
         , tid/2
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
    %% @TODO make this more robust
    _ = [ true=lets:delete(Tab) || Tab <- lets:all() ],
    catch exit(whereis(Name), kill),
    _ = os:cmd("find . -name '" ++ ?MODULE_STRING ++ ".*' -exec rm -rf {} \;"),
    _ = os:cmd("rm -rf " ++ ?MODULE_STRING).

is_table(Res) ->
    is_record(Res, gen_tid).

all(_Tab) ->
    catch lets:all().

tid(Tab) ->
    catch lets:tid(Tab).

tid(Tab, Options) ->
    catch lets:tid(Tab, Options).

new(Name, Options) ->
    ok = filelib:ensure_dir(?MODULE_STRING),
    catch lets:new(Name, filter_options(Options)).

new(_Tab, Name, Options) ->
    %% _Tab is to help control generators and shrinking
    new(Name, Options).

destroy(_Tab, Name, Options) ->
    %% _Tab is to help control generators and shrinking
    catch lets:destroy(Name, filter_options(Options)).

repair(_Tab, Name, Options) ->
    %% _Tab is to help control generators and shrinking
    catch lets:repair(Name, filter_options(Options)).

delete(Tab) ->
    catch lets:delete(Tab).

delete(Tab, Key) ->
    catch lets:delete(Tab, Key).

delete_all_objects(Tab) ->
    catch lets:delete_all_objects(Tab).

first(Tab) ->
    catch lets:first(Tab).

foldl(Function, Acc0, Tab) ->
    catch lets:foldl(Function, Acc0, Tab).

foldr(Function, Acc0, Tab) ->
    catch lets:foldr(Function, Acc0, Tab).

info(Tab) ->
    catch lets:info(Tab).

info(Tab, Item) ->
    catch lets:info(Tab, Item).

insert(Tab, ObjOrObjs) ->
    catch lets:insert(Tab, ObjOrObjs).

insert_new(Tab, ObjOrObjs) ->
    catch lets:insert_new(Tab, ObjOrObjs).

last(Tab) ->
    catch lets:last(Tab).

lookup(Tab, Key) ->
    catch lets:lookup(Tab, Key).

lookup_element(Tab, Key, Pos) ->
    catch lets:lookup_element(Tab, Key, Pos).

match(Tab, Pattern) ->
    catch lets:match(Tab, Pattern).

match(Tab, Pattern, Limit) ->
    catch lets:match(Tab, Pattern, Limit).

match(Cont) ->
    catch lets:match(Cont).

match_delete(Tab, Pattern) ->
    catch lets:match_delete(Tab, Pattern).

match_object(Tab, Pattern) ->
    catch lets:match_object(Tab, Pattern).

match_object(Tab, Pattern, Limit) ->
    catch lets:match_object(Tab, Pattern, Limit).

match_object(Cont) ->
    catch lets:match_object(Cont).

member(Tab, Key) ->
    catch lets:member(Tab, Key).

next(Tab, Key) ->
    catch lets:next(Tab, Key).

prev(Tab, Key) ->
    catch lets:prev(Tab, Key).

select(Tab, Spec) ->
    catch lets:select(Tab, Spec).

select(Tab, Spec, Limit) ->
    catch lets:select(Tab, Spec, Limit).

select(Cont) ->
    catch lets:select(Cont).

select_count(Tab, Spec) ->
    catch lets:select_count(Tab, Spec).

select_delete(Tab, Spec) ->
    catch lets:select_delete(Tab, Spec).

select_reverse(Tab, Spec) ->
    catch lets:select_reverse(Tab, Spec).

select_reverse(Tab, Spec, Limit) ->
    catch lets:select_reverse(Tab, Spec, Limit).

select_reverse(Cont) ->
    catch lets:select_reverse(Cont).

tab2list(Tab) ->
    catch lets:tab2list(Tab).


%%%===================================================================
%%% Internal
%%%===================================================================

filter_options(Options) ->
    X = proplists:get_value(db, Options, []),
    Y = [{path, ?MODULE_STRING}|proplists:delete(path, X)],
    [{db, Y}|proplists:delete(db, Options)].
