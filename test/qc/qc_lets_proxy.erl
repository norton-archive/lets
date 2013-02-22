%%% The MIT License
%%%
%%% Copyright (C) 2011-2013 by Joseph Wayne Norton <norton@alum.mit.edu>
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
         , all/1
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

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {tab, name, conts=dict:new()}).


%%%===================================================================
%%% API
%%%===================================================================

teardown(Name) ->
    qc_lets_raw:teardown(Name).

is_table(Tab) ->
    is_pid(Tab).

all(Tab) ->
    gen_server:call(Tab, all).

new(Name, Options) ->
    %% @NOTE needed for foldr's and foldl's anonymous funs
    _ = code:ensure_loaded(qc_statem_lets),
    {ok, Pid} = gen_server:start({local, Name}, ?MODULE, [Name, Options], []),
    Pid.

new(_Tab, Name, Options) ->
    %% _Tab is to help control generators and shrinking
    new(Name, Options).

destroy(Tab, Name, Options) ->
    qc_lets_raw:destroy(Tab, Name, Options).

repair(Tab, Name, Options) ->
    qc_lets_raw:repair(Tab, Name, Options).

delete(Tab) ->
    gen_server:call(Tab, delete).

delete(Tab, Key) ->
    gen_server:call(Tab, {delete, Key}).

delete_all_objects(Tab) ->
    gen_server:call(Tab, delete_all_objects).

first(Tab) ->
    gen_server:call(Tab, first).

foldl(Function, Acc0, Tab) ->
    gen_server:call(Tab, {foldl, Function, Acc0}).

foldr(Function, Acc0, Tab) ->
    gen_server:call(Tab, {foldr, Function, Acc0}).

info(Tab) ->
    gen_server:call(Tab, info).

info(Tab, Item) ->
    gen_server:call(Tab, {info, Item}).

insert(Tab, ObjOrObjs) ->
    gen_server:call(Tab, {insert, ObjOrObjs}).

insert_new(Tab, ObjOrObjs) ->
    gen_server:call(Tab, {insert_new, ObjOrObjs}).

last(Tab) ->
    gen_server:call(Tab, last).

lookup(Tab, Key) ->
    gen_server:call(Tab, {lookup, Key}).

lookup_element(Tab, Key, Pos) ->
    gen_server:call(Tab, {lookup_element, Key, Pos}).

match(Tab, Pattern) ->
    gen_server:call(Tab, {match, Pattern}).

match(Tab, Pattern, Limit) ->
    gen_server:call(Tab, {match, Pattern, Limit}).

match({wcont, _, _}=Cont0) ->
    {Tab, Cont} = unwrap_cont(Cont0),
    gen_server:call(Tab, {match_cont, Cont});
match('$end_of_table') ->
    '$end_of_table'.

match_delete(Tab, Pattern) ->
    gen_server:call(Tab, {match_delete, Pattern}).

match_object(Tab, Pattern) ->
    gen_server:call(Tab, {match_object, Pattern}).

match_object(Tab, Pattern, Limit) ->
    gen_server:call(Tab, {match_object, Pattern, Limit}).

match_object({wcont, _, _}=Cont0) ->
    {Tab, Cont} = unwrap_cont(Cont0),
    gen_server:call(Tab, {match_object_cont, Cont});
match_object('$end_of_table') ->
    '$end_of_table'.

member(Tab, Key) ->
    gen_server:call(Tab, {member, Key}).

next(Tab, Key) ->
    gen_server:call(Tab, {next, Key}).

prev(Tab, Key) ->
    gen_server:call(Tab, {prev, Key}).

select(Tab, Spec) ->
    gen_server:call(Tab, {select, Spec}).

select(Tab, Spec, Limit) ->
    gen_server:call(Tab, {select, Spec, Limit}).

select({wcont, _, _}=Cont0) ->
    {Tab, Cont} = unwrap_cont(Cont0),
    gen_server:call(Tab, {select_cont, Cont});
select('$end_of_table') ->
    '$end_of_table'.

select_count(Tab, Spec) ->
    gen_server:call(Tab, {select_count, Spec}).

select_delete(Tab, Spec) ->
    gen_server:call(Tab, {select_delete, Spec}).

select_reverse(Tab, Spec) ->
    gen_server:call(Tab, {select_reverse, Spec}).

select_reverse(Tab, Spec, Limit) ->
    gen_server:call(Tab, {select_reverse, Spec, Limit}).

select_reverse({wcont, _, _}=Cont0) ->
    {Tab, Cont} = unwrap_cont(Cont0),
    gen_server:call(Tab, {select_reverse_cont, Cont});
select_reverse('$end_of_table') ->
    '$end_of_table'.

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
    {ok, #state{tab=Tab, name=Name}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%--------------------------------------------------------------------
handle_call(all, _From, State) ->
    Reply = qc_lets_raw:all(unused),
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
handle_call(first, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:first(Tab),
    {reply, Reply, State};
handle_call({foldl, Function, Acc0}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:foldl(Function, Acc0, Tab),
    {reply, Reply, State};
handle_call({foldr, Function, Acc0}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:foldr(Function, Acc0, Tab),
    {reply, Reply, State};
handle_call(info, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:info(Tab),
    {reply, Reply, State};
handle_call({info, Item}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:info(Tab, Item),
    {reply, Reply, State};
handle_call({insert, ObjOrObjs}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:insert(Tab, ObjOrObjs),
    {reply, Reply, State};
handle_call({insert_new, ObjOrObjs}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:insert_new(Tab, ObjOrObjs),
    {reply, Reply, State};
handle_call(last, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:last(Tab),
    {reply, Reply, State};
handle_call({lookup, Key}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:lookup(Tab, Key),
    {reply, Reply, State};
handle_call({lookup_element, Key, Pos}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:lookup_element(Tab, Key, Pos),
    {reply, Reply, State};
handle_call({match, Pattern}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:match(Tab, Pattern),
    {reply, Reply, State};
handle_call({match, Pattern, Limit}, _From, #state{tab=Tab}=State) ->
    {Reply, NewState} = wrap_cont(qc_lets_raw:match(Tab, Pattern, Limit), State),
    {reply, Reply, NewState};
handle_call({match_cont, Cont0}, _From, State0) ->
    {Cont, State} = unwrap_cont(Cont0, State0),
    {Reply, NewState} = wrap_cont(qc_lets_raw:match(Cont), State),
    {reply, Reply, NewState};
handle_call({match_delete, Pattern}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:match_delete(Tab, Pattern),
    {reply, Reply, State};
handle_call({match_object, Pattern}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:match_object(Tab, Pattern),
    {reply, Reply, State};
handle_call({match_object, Pattern, Limit}, _From, #state{tab=Tab}=State) ->
    {Reply, NewState} = wrap_cont(qc_lets_raw:match_object(Tab, Pattern, Limit), State),
    {reply, Reply, NewState};
handle_call({match_object_cont, Cont0}, _From, State0) ->
    {Cont, State} = unwrap_cont(Cont0, State0),
    {Reply, NewState} = wrap_cont(qc_lets_raw:match_object(Cont), State),
    {reply, Reply, NewState};
handle_call({member, Key}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:member(Tab, Key),
    {reply, Reply, State};
handle_call({next, Key}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:next(Tab, Key),
    {reply, Reply, State};
handle_call({prev, Key}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:prev(Tab, Key),
    {reply, Reply, State};
handle_call({select_count, Spec}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:select_count(Tab, Spec),
    {reply, Reply, State};
handle_call({select_delete, Spec}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:select_delete(Tab, Spec),
    {reply, Reply, State};
handle_call({select, Spec}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:select(Tab, Spec),
    {reply, Reply, State};
handle_call({select, Spec, Limit}, _From, #state{tab=Tab}=State) ->
    {Reply, NewState} = wrap_cont(qc_lets_raw:select(Tab, Spec, Limit), State),
    {reply, Reply, NewState};
handle_call({select_cont, Cont0}, _From, State0) ->
    {Cont, State} = unwrap_cont(Cont0, State0),
    {Reply, NewState} = wrap_cont(qc_lets_raw:select(Cont), State),
    {reply, Reply, NewState};
handle_call({select_reverse, Spec}, _From, #state{tab=Tab}=State) ->
    Reply = qc_lets_raw:select_reverse(Tab, Spec),
    {reply, Reply, State};
handle_call({select_reverse, Spec, Limit}, _From, #state{tab=Tab}=State) ->
    {Reply, NewState} = wrap_cont(qc_lets_raw:select_reverse(Tab, Spec, Limit), State),
    {reply, Reply, NewState};
handle_call({select_reverse_cont, Cont0}, _From, State0) ->
    {Cont, State} = unwrap_cont(Cont0, State0),
    {Reply, NewState} = wrap_cont(qc_lets_raw:select_reverse(Cont), State),
    {reply, Reply, NewState};
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
%%% Internal
%%%===================================================================

wrap_cont({'EXIT', _}=Err, State) ->
    {Err, State};
wrap_cont('$end_of_table'=Reply, State) ->
    {Reply, State};
wrap_cont({_Match, '$end_of_table'}=Reply, State) ->
    {Reply, State};
wrap_cont({_Match, Cont}=Reply, #state{name=Name, conts=Conts}=State) ->
    ServerRef = {Name, node()},
    Ref = make_ref(),
    NewState = State#state{conts=dict:store(Ref, Cont, Conts)},
    {setelement(2, Reply, wrap_cont({ServerRef, Ref})), NewState}.

unwrap_cont(Ref, #state{conts=Conts}=State) ->
    Cont = dict:fetch(Ref, Conts),
    NewState = State#state{conts=dict:erase(Ref, Conts)},
    {Cont, NewState}.

wrap_cont({ServerRef, Cont}) ->
    {wcont, ServerRef, Cont}.

unwrap_cont({wcont, ServerRef, Cont}) ->
    {ServerRef, Cont}.
