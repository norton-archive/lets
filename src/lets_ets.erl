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

-module(lets_ets).

-include("lets.hrl").

%% External exports
-export([open/1
         , destroy/1
         , repair/1
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


%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

open(#tab{name=Name, named_table=NamedTable, type=Type, keypos=KeyPos, protection=Protection, compressed=Compressed}=Tab) ->
    Opts =
        [Type, {keypos,KeyPos}, Protection] ++
        [named_table || NamedTable ] ++
        [compressed || Compressed ],
    Ets = ets:new(Name, Opts),
    Tab#tab{ets=Ets}.

destroy(#tab{}) ->
    true.

repair(#tab{}) ->
    true.

insert(_Tab, Ets, ObjectOrObjects) ->
    ets:insert(Ets, ObjectOrObjects).

insert_new(_Tab, Ets, ObjectOrObjects) ->
    ets:insert_new(Ets, ObjectOrObjects).

delete(_Tab, Ets) ->
    ets:delete(Ets).

delete(_Tab, Ets, Key) ->
    ets:delete(Ets, Key).

delete_all_objects(_Tab, Ets) ->
    ets:delete_all_objects(Ets).

lookup(_Tab, Ets, Key) ->
    ets:lookup(Ets, Key).

first(_Tab, Ets) ->
    ets:first(Ets).

next(_Tab, Ets, Key) ->
    ets:next(Ets, Key).

info_memory(_Tab, Ets) ->
    ets:info(Ets, memory).

info_size(_Tab, Ets) ->
    ets:info(Ets, size).

tab2list(_Tab, Ets) ->
    ets:tab2list(Ets).
