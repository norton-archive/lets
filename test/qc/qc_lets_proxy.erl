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

-module(qc_lets_proxy).

-behaviour(gen_server).

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

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tab}).


%%%===================================================================
%%% API
%%%===================================================================

teardown(Name) ->
    qc_lets_raw:teardown(Name).

is_table(Tab) ->
    is_pid(Tab).

new(Name, Options) ->
    {ok, Pid} = gen_server:start({local, Name}, ?MODULE, [Name, Options], []),
    Pid.

new(_Tab, Name, Options) ->
    %% _Tab is to help control generators and shrinking
    new(Name, Options).

destroy(Tab, Name, Options) ->
    qc_lets_raw:destroy(Tab, Name, Options).

repair(Tab, Name, Options) ->
    qc_lets_raw:repair(Tab, Name, Options).

insert(Tab, ObjOrObjs) ->
    gen_server:call(Tab, {insert, ObjOrObjs}).

insert_new(Tab, ObjOrObjs) ->
    gen_server:call(Tab, {insert_new, ObjOrObjs}).

delete(Tab) ->
    gen_server:call(Tab, delete).

delete(Tab, Key) ->
    gen_server:call(Tab, {delete, Key}).

delete_all_objects(Tab) ->
    gen_server:call(Tab, delete_all_objects).

lookup(Tab, Key) ->
    gen_server:call(Tab, {lookup, Key}).

first(Tab) ->
    gen_server:call(Tab, first).

next(Tab, Key) ->
    gen_server:call(Tab, {next, Key}).

info(Tab, Item) ->
    gen_server:call(Tab, {info, Item}).

tab2list(Tab) ->
    gen_server:call(Tab, tab2list).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%--------------------------------------------------------------------
init([Name, Options]) ->
    Tab = qc_lets_raw:new(Name, Options),
    {ok, #state{tab=Tab}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%--------------------------------------------------------------------
handle_call({insert, ObjOrObjs}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:insert(Tab, ObjOrObjs),
    {reply, Reply, State};
handle_call({insert_new, ObjOrObjs}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:insert_new(Tab, ObjOrObjs),
    {reply, Reply, State};
handle_call(delete, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:delete(Tab),
    {stop, normal, Reply, State#state{tab=undefined}};
handle_call({delete, Key}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:delete(Tab, Key),
    {reply, Reply, State};
handle_call(delete_all_objects, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:delete_all_objects(Tab),
    {reply, Reply, State};
handle_call({lookup, Key}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:lookup(Tab, Key),
    {reply, Reply, State};
handle_call(first, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:first(Tab),
    {reply, Reply, State};
handle_call({next, Key}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:next(Tab, Key),
    {reply, Reply, State};
handle_call({info, Item}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:info(Tab, Item),
    {reply, Reply, State};
handle_call(tab2list, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:tab2list(Tab),
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, #state{tab=undefined}) ->
    ok;
terminate(_Reason, #state{tab=Tab}) ->
    qc_lets_raw:delete(Tab).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
