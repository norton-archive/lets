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

-module(lets).

-include("lets.hrl").

%% External exports
-export([new/2
         , destroy/2
         , repair/2
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

-export_type([tab/0]).

%%
%% ETS exports
%%
%% -export([all/0
%%          , delete/1              %% mnesia
%%          , delete/2              %% mnesia
%%          , delete_all_objects/1
%%          , delete_object/2
%%          , file2tab/1
%%          , file2tab/2
%%          , filter/3              %% mnesia
%%          , first/1               %% mnesia
%%          , foldl/3               %% mnesia
%%          , foldr/3
%%          , from_dets/2
%%          , fun2ms/1
%%          , give_away/3
%%          , i/0
%%          , i/1
%%          , info/1
%%          , info/2                %% mnesia
%%          , init_table/2          %% mnesia
%%          , insert/2              %% mnesia
%%          , insert_new/2
%%          , is_compiled_ms/1
%%          , last/1                %% mnesia
%%          , lookup/2              %% mnesia
%%          , lookup_element/3      %% mnesia
%%          , match/1
%%          , match/2               %% mnesia
%%          , match/3
%%          , match_delete/2        %% mnesia
%%          , match_object/1
%%          , match_object/2        %% mnesia
%%          , match_object/3
%%          , match_spec_compile/1
%%          , match_spec_run/2      %% mnesia
%%          , member/2
%%          , new/2                 %% mnesia
%%          , next/2                %% mnesia
%%          , prev/2                %% mnesia
%%          , rename/2
%%          , repair_continuation/2 %% mnesia
%%          , safe_fixtable/2
%%          , select/1
%%          , select/2
%%          , select/3
%%          , select_count/2
%%          , select_delete/2
%%          , select_reverse/1
%%          , select_reverse/2
%%          , select_reverse/3
%%          , setopts/2
%%          , slot/2                %% mnesia
%%          , tab2file/2
%%          , tab2file/3
%%          , tab2list/1            %% mnesia
%%          , tabfile_info/1
%%          , table/1
%%          , table/2
%%          , test_ms/2
%%          , to_dets/2
%%          , update_counter/3      %% mnesia
%%          , update_element/3
%%         ]).


%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-opaque tab()         :: #tab{}.

-type opts()          :: [ets_opt() | impl_opt() | db_opts() | db_read_opts() | db_write_opts()].
-type ets_opt()       :: set | ordered_set | named_table | {key_pos,pos_integer()} | public | protected | private | compressed | async.
-type impl_opt()      :: drv | nif | ets.

-type db_opts()       :: {db, [{path,file:filename()} | create_if_missing | {create_if_missing,boolean()} | error_if_exists | {error_if_exists,boolean()} | paranoid_checks | {paranoid_checks,boolean()} | {write_buffer_size,pos_integer()} | {max_open_files,pos_integer()} | {block_cache_size,pos_integer()} | {block_size,pos_integer()} | {block_restart_interval,pos_integer()} | {filter_policy,no | {bloom,pos_integer()}}]}.
-type db_read_opts()  :: {db_read, [verify_checksums | {verify_checksums,boolean()} | fill_cache | {fill_cache,boolean()}]}.
-type db_write_opts() :: {db_write, [sync | {sync,boolean()}]}.

-type key()           :: binary().
-type object()        :: term().

-type name()          :: atom().
-type item()          :: owner | name | named_table | type | keypos | protection | compressed | async | memory | size.
-type pos()           :: pos_integer().
-type pattern()       :: atom() | tuple(). %% ets:match_pattern() is not exported!
-type spec()          :: ets:match_spec().
-type match()         :: term().
-type limit()         :: pos_integer().
-opaque cont()        :: {cont, tab(), term()}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @doc Creates a new table and returns a table identifier which can
%% be used in subsequent operations.  The table identifier can be sent
%% to other processes so that a table can be shared between different
%% processes within a node.
%%
%% Valid LETS properties for +Options+ are:
%%
%% - +set+ The table is a set table - one key, one object, no order
%%   among objects. This is the default table type.
%%
%% - +ordered_set+ The table is an ordered_set table - one key, one
%%   object, ordered in Erlang term order, which is the order implied
%%   by the +<+ and +>+ operators.
%%
%% - +named_table+ If this option is present, the name +Name+ is
%%   associated with the table identifier.  _only the ets
%%   implementation_
%%
%% - +{key_pos,pos_integer()}+ Specfies which element in the stored
%%   tuples should be used as key. By default, it is the first
%%   element, i.e. +Pos=1+.
%%
%% - +public+ Any process may read or write to the table.
%%
%% - +protected+ The owner process can read and write to the table.
%%   Other processes can only read the table. This is the default
%%   setting for the access rights.
%%
%% - +private+ Only the owner process can read or write to the table.
%%
%% - +compressed+ If this option is present, the table data will be
%%   stored in a compressed format.
%%
%% - +async+ If this option is present, the emulator\'s async thread
%%   pool will be used when accessing the table data.  _only the drv
%%   implementation_
%%
%% - +drv+ If this option is present, the table data will be stored
%%   with LevelDB backend via an Erlang Driver.  This is the default
%%   setting for the table implementation.
%%
%% - +nif+ If this option is present, the table data will be stored
%%   with LevelDB backend via an Erlang NIF.
%%
%% - +ets+ If this option is present, the table data will be stored
%%   with ETS as the backend.
%%
%% - +{db, [db_opts()]}+ LevelDB database options.
%%
%% - +{db_read, [db_read_opts()]}+ LevelDB read options.
%%
%% - +{db_write, [db_write_opts()]}+ LevelDB write options.
%%
%% Valid LevelDB database properties for +db_opts()+ are:
%%
%% - +{path, file:filename()}+ Open the database with the specified
%%   path.  The default is +Name+.
%%
%% - +create_if_missing | {create_if_missing, boolean()}+ If +true+,
%%   the database will be created if it is missing.  The default is
%%   +false+.
%%
%% - +error_if_exists | {error_if_exists, boolean()}+ If +true+, an
%%   error is raised if the database already exists. The default is
%%   +false+.
%%
%% - +paranoid_checks | {paranoid_checks, boolean()}+ If +true+, the
%%   implementation will do aggressive checking of the data it is
%%   processing and will stop early if it detects any errors. The
%%   default is +false+.
%%
%% - +{write_buffer_size, pos_integer()}+ The default is 4MB.
%%
%% - +{max_open_files, pos_integer()}+ The default is 1000.
%%
%% - +{block_cache_size, pos_integer()}+ The default is 8MB.
%%
%% - +{block_size, pos_integer()}+ The default is 4K.
%%
%% - +{block_restart_interval, pos_integer()}+ The default is 16.
%%
%% - +{filter_policy, no | {bloom, pos_integer()}}+ The default is +no+.
%%
%% Valid LevelDB read properties for +db_read_opts()+ are:
%%
%% - +verify_checksums | {verify_checksums, boolean()}+ If +true+, all
%%   data read from underlying storage will be verified against
%%   corresponding checksums. The default is +false+.
%%
%% - +fill_cache | {fill_cache, boolean()}+ If +true+, the data read
%%   should be cached in memory. The default is +true+.
%%
%% Valid LevelDB write properties for +db_write_opts()+ are:
%%
%% - +sync | {sync, boolean()}+ If +true+, the write will be flushed
%%   from the operating system buffer cache before the write is
%%   considered complete. The default is +false+.
%%
%% @end
%% @see ets:new/2

-spec new(name(), opts()) -> tab().
new(Name, Opts) ->
    create(open, Name, Opts).

%% @doc Destroy the contents of the specified table.  This function
%% only applies to +driver+ and +nif+ implementations.
%% @end

-spec destroy(name(), opts()) -> true.
destroy(Name, Opts) ->
    create(destroy, Name, Opts).

%% @doc If a table cannot be opened, you may attempt to call this
%% method to resurrect as much of the contents of the table as
%% possible.  Some data may be lost, so be careful when calling this
%% function on a table that contains important information. This
%% function only applies to +driver+ and +nif+ implementations.
%% @end

-spec repair(name(), opts()) -> true.
repair(Name, Opts) ->
    create(repair, Name, Opts).

%% @doc Deletes the entire table +Tab+.
%% @end
%% @see ets:delete/1

-spec delete(tab()) -> true.
delete(Tab) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:delete(Tab)
    end.

%% @doc Deletes all objects with the key +Key+ from the table +Tab+.
%% @end
%% @see ets:delete/2

-spec delete(tab(), key()) -> true.
delete(Tab, Key) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:delete(Tab, Key)
    end.

%% @doc Delete all objects in the table +Tab+. The operation is
%% guaranteed to be atomic and isolated.  This function only applies
%% to the +ets+ implementation.
%% @end
%% @see ets:delete_all_objects/1

-spec delete_all_objects(tab()) -> true.
delete_all_objects(Tab) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:delete_all_objects(Tab)
    end.

%% @doc Returns the first key +Key+ in the table +Tab+.  If the table
%% is empty, +'$end_of_table'+ will be returned.
%% @end
%% @see ets:first/1

-spec first(tab()) -> key() | '$end_of_table'.
first(Tab) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:first(Tab)
    end.

%% @doc Fold from left to right over the elements of the table.
%% @end
%% @see ets:foldl/3

-spec foldl(Fun, Acc0::term(), tab()) -> Acc1::term() when
      Fun :: fun((Element::term(), AccIn::term()) -> AccOut::term()).
foldl(Function, Acc0, Tab) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:foldl(Function, Acc0, Tab)
    end.

%% @doc Fold from right to left over the elements of the table.
%% @end
%% @see ets:foldr/3

-spec foldr(Fun, Acc0::term(), tab()) -> Acc1::term() when
      Fun :: fun((Element::term(), AccIn::term()) -> AccOut::term()).
foldr(Function, Acc0, Tab) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:foldr(Function, Acc0, Tab)
    end.

%% @doc Returns information about the table +Tab+ as a list of +{Item,
%% Value}+ tuples.
%%
%% @end
%% @see info/2

-spec info(tab()) -> [{item(), term()}].
info(Tab) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            [{owner, Tab#tab.owner},
             {name, Tab#tab.name},
             {named_table, Tab#tab.named_table},
             {type, Tab#tab.type},
             {keypos, Tab#tab.keypos},
             {protection, Tab#tab.protection},
             {compressed, Tab#tab.compressed},
             {async, Tab#tab.async},
             {memory, Mod:info_memory(Tab)},
             {size, Mod:info_size(Tab)}]
    end.

%% @doc Returns the information associated with +Item+ for the table +Tab+.
%%
%% Valid +Item+ options are:
%%
%% - +owner+
%% - +name+
%% - +named_table+ _only the ets implementation_
%% - +type+
%% - +keypos+
%% - +protection+
%% - +compressed+
%% - +async+ _only the drv implementation_
%% - +memory+ _only the ets implementation_
%% - +size+ _only the ets implementation_
%%
%% @end
%% @see ets:info/2

-spec info(tab(), item()) -> term().
info(Tab, Item) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            case Item of
                owner ->
                    Tab#tab.owner;
                name ->
                    Tab#tab.name;
                named_table ->
                    Tab#tab.named_table;
                type ->
                    Tab#tab.type;
                keypos ->
                    Tab#tab.keypos;
                protection ->
                    Tab#tab.protection;
                compressed ->
                    Tab#tab.compressed;
                async ->
                    Tab#tab.async;
                memory ->
                    Mod:info_memory(Tab);
                size ->
                    Mod:info_size(Tab);
                _ ->
                    erlang:error(badarg, [Tab, Item])
            end
    end.

%% @doc Inserts the object or all of the objects in the list
%% +ObjOrObjs+ into the table +Tab+.
%% @end
%% @see ets:insert/2

-spec insert(tab(), object() | [object()]) -> true.
insert(Tab, ObjOrObjs) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:insert(Tab, ObjOrObjs)
    end.

%% @doc This function works exactly like +insert/2+, with the
%% exception that instead of overwriting objects with the same key, it
%% simply returns false.  This function only applies to the +ets+
%% implementation.
%% @end
%% @see ets:insert_new/2

-spec insert_new(tab(), object() | [object()]) -> true.
insert_new(Tab, ObjOrObjs) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:insert_new(Tab, ObjOrObjs)
    end.

%% @doc Returns the last key +Key+ in the table +Tab+.  If the table
%% is empty, +'$end_of_table'+ will be returned.
%% @end
%% @see ets:last/1

-spec last(tab()) -> key() | '$end_of_table'.
last(Tab) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:last(Tab)
    end.

%% @doc Returns a list of all objects with the key +Key+ in the table
%% +Tab+.
%% @end
%% @see ets:lookup/2

-spec lookup(tab(), key()) -> [object()].
lookup(Tab, Key) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:lookup(Tab, Key)
    end.

%% @doc Returns the +Pos+:th element of the object with the key +Key+
%% in the table +Tab+.
%% @end
%% @see ets:lookup_element/3

-spec lookup_element(tab(), key(), pos()) -> term().
lookup_element(Tab, Key, Pos) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:lookup_element(Tab, Key, Pos)
    end.

%% @doc Matches the objects in the table +Tab+ against the pattern
%% +Pattern+.
%% @end
%% @see ets:match/2

-spec match(tab(), pattern()) -> [match()].
match(Tab, Pattern) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:match(Tab, Pattern)
    end.

%% @doc Matches the objects in the table +Tab+ against the pattern
%% +Pattern+ and returns a limited (+Limit+) number of matching
%% objects.
%% @end
%% @see ets:match/3

-spec match(tab(), pattern(), limit()) -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
match(Tab, Pattern, Limit) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            wrap_cont_reply(Tab, Mod:match(Tab, Pattern, Limit))
    end.

%% @doc Continues a match started with +match/3+.
%% @end
%% @see ets:match/1

-spec match(cont() | '$end_of_table') -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
match({cont, Tab, Cont}) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            wrap_cont_reply(Tab, Mod:match(Cont))
    end;
match('$end_of_table') ->
    '$end_of_table'.

%% @doc Deletes all objects which match the pattern +Pattern+ from the
%% table +Tab+.
%% @end
%% @see ets:match_delete/2

-spec match_delete(tab(), pattern()) -> true.
match_delete(Tab, Pattern) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:match_delete(Tab, Pattern)
    end.

%% @doc Matches the objects in the table +Tab+ against the pattern
%% +Pattern+.
%% @end
%% @see ets:match_object/2

-spec match_object(tab(), pattern()) -> [match()].
match_object(Tab, Pattern) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:match_object(Tab, Pattern)
    end.

%% @doc Matches the objects in the table +Tab+ against the pattern
%% +Pattern+ and returns a limited (+Limit+) number of matching
%% objects.
%% @end
%% @see ets:match_object/3

-spec match_object(tab(), pattern(), limit()) -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
match_object(Tab, Pattern, Limit) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            wrap_cont_reply(Tab, Mod:match_object(Tab, Pattern, Limit))
    end.

%% @doc Continues a match started with +match_object/3+.
%% @end
%% @see ets:match_object/1

-spec match_object(cont() | '$end_of_table') -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
match_object({cont, Tab, Cont}) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            wrap_cont_reply(Tab, Mod:match_object(Cont))
    end;
match_object('$end_of_table') ->
    '$end_of_table'.

%% @doc Returns +true+ if one or more elements in the table +Tab+ has
%% the key +Key+, +false+ otherwise.
%% @end
%% @see ets:member/2

-spec member(tab(), key()) -> true | false.
member(Tab, Key) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:member(Tab, Key)
    end.

%% @doc Returns the next key +Key2+, following the key +Key1+ in the
%% table +Tab+.  If there is no next key, +'$end_of_table'+ is
%% returned.
%% @end
%% @see ets:next/2

-spec next(tab(), key()) -> key() | '$end_of_table'.
next(Tab, Key) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:next(Tab, Key)
    end.

%% @doc Returns the previous key +Key2+, following the key +Key1+ in
%% the table +Tab+.  If there is no previous key, +'$end_of_table'+ is
%% returned.
%% @end
%% @see ets:prev/2

-spec prev(tab(), key()) -> key() | '$end_of_table'.
prev(Tab, Key) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:prev(Tab, Key)
    end.

%% repair_continuation/2

%% @doc Matches the objects in the table +Tab+ against the spec
%% +Spec+.
%% @end
%% @see ets:select/2

-spec select(tab(), spec()) -> [match()].
select(Tab, Spec) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:select(Tab, Spec)
    end.

%% @doc Matches the objects in the table +Tab+ against the spec +Spec+
%% and returns a limited (+Limit+) number of matching objects.
%% @end
%% @see ets:select/3

-spec select(tab(), spec(), limit()) -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
select(Tab, Spec, Limit) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            wrap_cont_reply(Tab, Mod:select(Tab, Spec, Limit))
    end.

%% @doc Continues a select started with +select/3+.
%% @end
%% @see ets:select/1

-spec select(cont() | '$end_of_table') -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
select({cont, Tab, Cont}) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            wrap_cont_reply(Tab, Mod:select(Cont))
    end;
select('$end_of_table') ->
    '$end_of_table'.

%% @doc Counts all objects which match the spec +Spec+ from the
%% table +Tab+ and returns the number matched.
%% @end
%% @see ets:select_count/2

-spec select_count(tab(), pattern()) -> pos_integer().
select_count(Tab, Spec) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:select_count(Tab, Spec)
    end.

%% @doc Deletes all objects which match the spec +Spec+ from the
%% table +Tab+ and returns the number deleted.
%% @end
%% @see ets:select_delete/2

-spec select_delete(tab(), pattern()) -> pos_integer().
select_delete(Tab, Spec) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:select_delete(Tab, Spec)
    end.

%% @doc Matches in reverse the objects in the table +Tab+ against the
%% spec +Spec+.
%% @end
%% @see ets:select_reverse/2

-spec select_reverse(tab(), spec()) -> [match()].
select_reverse(Tab, Spec) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:select_reverse(Tab, Spec)
    end.

%% @doc Matches in reverse the objects in the table +Tab+ against the
%% spec +Spec+ and returns a limited (+Limit+) number of matching
%% objects.
%% @end
%% @see ets:select_reverse/3

-spec select_reverse(tab(), spec(), limit()) -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
select_reverse(Tab, Spec, Limit) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            wrap_cont_reply(Tab, Mod:select_reverse(Tab, Spec, Limit))
    end.

%% @doc Continues a select reverse started with +select_reverse/3+.
%% @end
%% @see ets:select_reverse/1

-spec select_reverse(cont() | '$end_of_table') -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
select_reverse({cont, Tab, Cont}) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            wrap_cont_reply(Tab, Mod:select_reverse(Cont))
    end;
select_reverse('$end_of_table') ->
    '$end_of_table'.

%% @doc Returns a list of all objects in the table +Tab+. The
%% operation is *not* guaranteed to be atomic and isolated.
%% @end
%% @see ets:tab2list/1

-spec tab2list(tab()) -> [object()].
tab2list(Tab) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        Mod ->
            Mod:tab2list(Tab)
    end.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

check_access(#tab{impl=undefined}) ->
    undefined;
check_access(#tab{protection=Protection, owner=Owner, impl=Impl})
  when Protection==public orelse Owner==self() ->
    if is_port(Impl) ->
            lets_drv;
       is_atom(Impl) orelse is_integer(Impl) ->
            lets_ets;
       true ->
            lets_nif
    end;
check_access(_Tab) ->
    undefined.

create(Op, Name, Opts) ->
    case options(Opts) of
        {POpts, []} ->
            POpts;
        {POpts, BadArgs} ->
            erlang:error(badarg, [Name, BadArgs])
    end,

    Owner = self(),
    NamedTable = proplists:get_bool(named_table, POpts),
    Type =
        case proplists:get_bool(ordered_set, POpts) of
            true ->
                ordered_set;
            false ->
                set
        end,
    KeyPos = proplists:get_value(keypos, POpts, 1),
    Protection =
        case proplists:get_bool(private, POpts) of
            true ->
                private;
            false ->
                case proplists:get_bool(protected, POpts) of
                    true ->
                        protected;
                    false ->
                        case proplists:get_bool(public, POpts) of
                            true ->
                                public;
                            false ->
                                protected
                        end
                end
        end,
    Compressed = proplists:get_bool(compressed, POpts),
    Async = proplists:get_bool(async, POpts),
    Drv = proplists:get_bool(drv, POpts),
    Nif = proplists:get_bool(nif, POpts),
    Ets = proplists:get_bool(ets, POpts),

    Tab = #tab{owner=Owner,
               name=Name,
               named_table=NamedTable,
               type=Type,
               keypos=KeyPos,
               protection=Protection,
               compressed=Compressed,
               async=Async},

    DBOptions = fix_db_options(Tab, proplists:get_value(db, POpts, [])),
    DBReadOptions = proplists:get_value(db_read, POpts, []),
    DBWriteOptions = proplists:get_value(db_write, POpts, []),

    if Drv ->
            lets_drv:Op(Tab, DBOptions, DBReadOptions, DBWriteOptions);
       Nif ->
            lets_nif:Op(Tab, DBOptions, DBReadOptions, DBWriteOptions);
       Ets ->
            lets_ets:Op(Tab);
       true ->
            lets_drv:Op(Tab, DBOptions, DBReadOptions, DBWriteOptions)
    end.

fix_db_options(#tab{name=Name, compressed=Compressed, async=Async}, Options0) ->
    Options1 = fix_db_options_path(Name, Options0),
    Options2 = fix_db_options_compression(Compressed, Options1),
    Options3 = fix_db_options_async(Async, Options2),
    Options3.

fix_db_options_path(Name, Options) ->
    case proplists:lookup(path, Options) of
        none ->
            [{path, binify(Name)}|Options];
        {path, Path} ->
            [{path, binify(Path)}|proplists:delete(path, Options)]
    end.

fix_db_options_compression(false, Options) ->
    [{compression, no}|Options];
fix_db_options_compression(true, Options) ->
    [{compression, snappy}|Options].

fix_db_options_async(false, Options) ->
    [{async, false}|Options];
fix_db_options_async(true, Options) ->
    [{async, true}|Options].

binify(X) when is_atom(X) ->
    list_to_binary(atom_to_list(X));
binify(X) when is_list(X) ->
    list_to_binary(X);
binify(X) when is_binary(X) ->
    X.

options(Options) ->
    Keys = [set, ordered_set, named_table, keypos, public, protected, private, compressed, async, drv, nif, ets, db, db_read, db_write],
    options(Options, Keys).

options(Options, Keys) when is_list(Options) ->
    options(Options, Keys, []);
options(Option, Keys) ->
    options([Option], Keys, []).

options(Options, [Key|Keys], L) when is_list(Options) ->
    case proplists:lookup(Key, Options) of
        none ->
            options(Options, Keys, L);
        {Key, Value} ->
            case Key of
                Key when Key == db; Key == db_read; Key == db_write ->
                    sub_options(Key, Value, Options, Keys, L);
                _ ->
                    NewOptions = proplists:delete(Key, Options),
                    options(NewOptions, Keys, [{Key,Value}|L])
            end
    end;
options(Options, [], L) ->
    {lists:reverse(L), Options}.

sub_options(db=Key, Value, Options, Keys, L) ->
    SubKeys = [path, create_if_missing, error_if_exists, paranoid_checks, write_buffer_size, max_open_files, block_cache_size, block_size, block_restart_interval, filter_policy],
    sub_options(Key, Value, Options, Keys, L, SubKeys);
sub_options(db_read=Key, Value, Options, Keys, L) ->
    %% @TODO snapshot
    SubKeys = [verify_checksums, fill_cache],
    sub_options(Key, Value, Options, Keys, L, SubKeys);
sub_options(db_write=Key, Value, Options, Keys, L) ->
    %% @TODO snapshot
    SubKeys = [sync],
    sub_options(Key, Value, Options, Keys, L, SubKeys).

sub_options(Key, Value, Options, Keys, L, SubKeys) ->
    case options(Value, SubKeys) of
        {NewValue, []} ->
            NewOptions = proplists:delete(Key, Options),
            options(NewOptions, Keys, [{Key,NewValue}|L]);
        {_NewValue, _} ->
            options(Options, Keys, L)
    end.

wrap_cont_reply(_Tab, '$end_of_table'=Reply) ->
    Reply;
wrap_cont_reply(_Tab, {_Match, '$end_of_table'}=Reply) ->
    Reply;
wrap_cont_reply(Tab, {_Match, Cont}=Reply) ->
    setelement(2, Reply, {cont, Tab, Cont}).
