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

-module(qc_lets_raw).

-include("../../src/lets.hrl").

%% API
-export([%% test
         teardown/1
         , is_table/1
         %% lets
         , new/2
         , new/3
         , destroy/3
         , repair/3
         , insert/2
         , insert_new/2
         , delete/1
         , delete/2
         , delete_all_objects/1
         , lookup/2
         , first/1
         , next/2
         , info/2
         , tab2list/1
        ]).


%%%===================================================================
%%% API
%%%===================================================================

teardown(Name) ->
    %% @TODO make this more robust
    catch ets:delete(Name),
    catch exit(whereis(Name), kill),
    os:cmd("rm -rf " ++ ?MODULE_STRING).

is_table(Res) ->
    is_record(Res, tab).

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

insert(Tab, ObjOrObjs) ->
    catch lets:insert(Tab, ObjOrObjs).

insert_new(Tab, ObjOrObjs) ->
    catch lets:insert_new(Tab, ObjOrObjs).

delete(Tab) ->
    catch lets:delete(Tab).

delete(Tab, Key) ->
    catch lets:delete(Tab, Key).

delete_all_objects(Tab) ->
    catch lets:delete_all_objects(Tab).

lookup(Tab, Key) ->
    catch lets:lookup(Tab, Key).

first(Tab) ->
    catch lets:first(Tab).

next(Tab, Key) ->
    catch lets:next(Tab, Key).

info(Tab, Item) ->
    catch lets:info(Tab, Item).

tab2list(Tab) ->
    catch lets:tab2list(Tab).


%%%===================================================================
%%% Internal
%%%===================================================================

filter_options(Options) ->
    X = proplists:get_value(db, Options, []),
    Y = [{path, ?MODULE_STRING}|proplists:delete(path, X)],
    [{db, Y}|proplists:delete(db, Options)].
