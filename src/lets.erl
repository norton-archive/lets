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

-module(lets).

-include("lets.hrl").

%% External exports
-export([all/0
         , tid/1
         , tid/2
         , new/2
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

%% DEBUG -compile(export_all).

-export_type([lets_tab/0, lets_tid/0, name/0, opts/0, key/0, object/0, match_pattern/0, match_spec/0, cont/0]).

%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-type lets_tid()      :: gen_ets_ns:gen_tid().
-type lets_tab()      :: gen_ets_ns:gen_tab().

-type opts()          :: [ets_opt() | impl_opt() | {db, db_opts()} | {db_read, db_read_opts()} | {db_write, db_write_opts()}].
-type ets_opt()       :: set | ordered_set | named_table | {keypos,pos_integer()} | public | protected | private | compressed | async.
-type impl_opt()      :: drv | nif | hyper | rocks | ets.

-type db_opts()       :: [{path,file:filename()} | create_if_missing | {create_if_missing,boolean()} | error_if_exists | {error_if_exists,boolean()} | paranoid_checks | {paranoid_checks,boolean()} | {write_buffer_size,pos_integer()} | {max_open_files,pos_integer()} | {block_cache_size,pos_integer()} | {block_size,pos_integer()} | {block_restart_interval,pos_integer()} | {filter_policy,no | {bloom,pos_integer()}}].
-type db_read_opts()  :: [verify_checksums | {verify_checksums,boolean()} | fill_cache | {fill_cache,boolean()}].
-type db_write_opts() :: [sync | {sync,boolean()}].

-type key()           :: gen_ets_ns:key().
-type object()        :: gen_ets_ns:object().

-type name()          :: gen_ets_ns:gen_name().
-type item()          :: owner | name | named_table | type | keypos | protection | compressed | async | memory | size.
-type pos()           :: pos_integer().
-type match_pattern() :: gen_ets_ns:match_pattern().
-type match_spec()    :: gen_ets_ns:match_spec().
-type match()         :: term().
-type limit()         :: pos_integer().
-opaque cont()        :: {cont, lets_tid(), term()}.

-define(NS, lets_reg).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% @doc Returns a list of all tables at the node.
%% @end
%% @see ets:all/0

-spec all() -> [lets_tab()].
all() ->
    gen_ets_ns:all(?NS).

%% @doc Returns a table\'s identifier.
%% @end

-spec tid(lets_tab()) -> lets_tid().
tid(Tab) ->
    gen_ets_ns:tid(?NS, Tab).

%% @doc Returns a copy of a table\'s identifier with given LevelDB
%% read and write options.  If Opts is not undefined, the given read
%% and write options are used when the returned copy is passed as the
%% first argument to table operations.  Otherwise, the original read
%% and write options given at the time of calling new/2 are used.
%% @end

-spec tid(lets_tab(), Opts :: undefined | [{db_read, db_read_opts()} | {db_write, db_write_opts()}]) -> lets_tid().
tid(Tab, undefined) ->
    gen_ets_ns:tid(?NS, Tab, undefined);
tid(Tab, Opts) ->
    POpts = case options(Opts, [db_read, db_write]) of
                {POpts0, []} ->
                    POpts0;
                {_POpts0, BadArgs} ->
                    erlang:error(badarg, [Tab, BadArgs])
            end,
    gen_ets_ns:tid(?NS, Tab, POpts).

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
%%   associated with the table identifier.
%%
%% - +{keypos,pos_integer()}+ Specfies which element in the stored
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
%% - +compressed+ If this option is present, the table data is stored
%%   in a compressed format.
%%
%% - +async+ If this option is present, the emulator\'s async thread
%%   pool is used when accessing the table data.  _only the drv
%%   implementation_
%%
%% - +drv+ If this option is present, the table data is stored with
%%   LevelDB backend via an Erlang Driver.  This is the default
%%   setting for the table implementation.
%%
%% - +nif+ If this option is present, the table data is stored with
%%   LevelDB backend via an Erlang NIF.
%%
%% - +hyper+ If this option is present, the HyperLeveDB version of the
%%   LevelDB implementation is used as the backend.  The original
%%   LevelDB implementation is the default backend.  _not applicable
%%   to the ets implementation_
%%
%% - +rocks+ If this option is present, the RocksDB version of the
%%   LevelDB implementation is used as the backend.  The original
%%   LevelDB implementation is the default backend.  _not applicable
%%   to the ets implementation_
%%
%% - +ets+ If this option is present, the table data is stored with
%%   ETS as the backend.
%%
%% - +{db, db_opts()}+ LevelDB database options.
%%
%% - +{db_read, db_read_opts()}+ LevelDB read options.
%%
%% - +{db_write, db_write_opts()}+ LevelDB write options.
%%
%% Valid LevelDB database properties for +db_opts()+ are:
%%
%% - +{path, file:filename()}+ Open the database with the specified
%%   path.  The default is +Name+.
%%
%% - +create_if_missing | {create_if_missing, boolean()}+ If +true+,
%%   the database is created if it is missing.  The default is
%%   +false+.
%%
%% - +error_if_exists | {error_if_exists, boolean()}+ If +true+, an
%%   error is raised if the database already exists. The default is
%%   +false+.
%%
%% - +paranoid_checks | {paranoid_checks, boolean()}+ If +true+, the
%%   implementation does aggressive checking of the data it is
%%   processing and stops early if it detects any errors. The default
%%   is +false+.
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
%%   data read from underlying storage is verified against
%%   corresponding checksums. The default is +false+.
%%
%% - +fill_cache | {fill_cache, boolean()}+ If +true+, the data read
%%   should be cached in memory. The default is +true+.
%%
%% Valid LevelDB write properties for +db_write_opts()+ are:
%%
%% - +sync | {sync, boolean()}+ If +true+, the write is flushed from
%%   the operating system buffer cache before the write is considered
%%   complete. The default is +false+.
%%
%% @end
%% @see ets:new/2

-spec new(name(), opts()) -> lets_tab().
new(Name, Opts) ->
    create(new, Name, Opts).

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

-spec delete(lets_tab()) -> true.
delete(Tab) ->
    gen_ets_ns:delete(?NS, Tab).

%% @doc Deletes all objects with the key +Key+ from the table +Tab+.
%% @end
%% @see ets:delete/2

-spec delete(lets_tab(), key()) -> true.
delete(Tab, Key) ->
    gen_ets_ns:delete(?NS, Tab, Key).

%% @doc Delete all objects in the table +Tab+. The operation is
%% guaranteed to be atomic and isolated.  This function only applies
%% to the +ets+ implementation.
%% @end
%% @see ets:delete_all_objects/1

-spec delete_all_objects(lets_tab()) -> true.
delete_all_objects(Tab) ->
    gen_ets_ns:delete_all_objects(?NS, Tab).

%% @doc Returns the first key +Key+ in the table +Tab+.  If the table
%% is empty, +'$end_of_table'+ is returned.
%% @end
%% @see ets:first/1

-spec first(lets_tab()) -> key() | '$end_of_table'.
first(Tab) ->
    gen_ets_ns:first(?NS, Tab).

%% @doc Fold from left to right over the elements of the table.
%% @end
%% @see ets:foldl/3

-spec foldl(Fun, Acc0::term(), lets_tab()) -> Acc1::term() when
      Fun :: fun((Element::term(), AccIn::term()) -> AccOut::term()).
foldl(Function, Acc0, Tab) ->
    gen_ets_ns:foldl(?NS, Function, Acc0, Tab).

%% @doc Fold from right to left over the elements of the table.
%% @end
%% @see ets:foldr/3

-spec foldr(Fun, Acc0::term(), lets_tab()) -> Acc1::term() when
      Fun :: fun((Element::term(), AccIn::term()) -> AccOut::term()).
foldr(Function, Acc0, Tab) ->
    gen_ets_ns:foldr(?NS, Function, Acc0, Tab).

%% @doc Returns information about the table +Tab+ as a list of +{Item,
%% Value}+ tuples.
%%
%% @end
%% @see info/2

-spec info(lets_tab()) -> [{item(), term()}].
info(Tab) ->
    gen_ets_ns:info(?NS, Tab).

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

-spec info(lets_tab(), item()) -> term().
info(Tab, Item) ->
    gen_ets_ns:info(?NS, Tab, Item).

%% @doc Inserts the object or all of the objects in the list
%% +ObjOrObjs+ into the table +Tab+.
%% @end
%% @see ets:insert/2

-spec insert(lets_tab(), object() | [object()]) -> true.
insert(Tab, ObjOrObjs) ->
    gen_ets_ns:insert(?NS, Tab, ObjOrObjs).

%% @doc This function works exactly like +insert/2+, with the
%% exception that instead of overwriting objects with the same key, it
%% simply returns false.  This function only applies to the +ets+
%% implementation.
%% @end
%% @see ets:insert_new/2

-spec insert_new(lets_tab(), object() | [object()]) -> true.
insert_new(Tab, ObjOrObjs) ->
    gen_ets_ns:insert_new(?NS, Tab, ObjOrObjs).

%% @doc Returns the last key +Key+ in the table +Tab+.  If the table
%% is empty, +'$end_of_table'+ is returned.
%% @end
%% @see ets:last/1

-spec last(lets_tab()) -> key() | '$end_of_table'.
last(Tab) ->
    gen_ets_ns:last(?NS, Tab).

%% @doc Returns a list of all objects with the key +Key+ in the table
%% +Tab+.
%% @end
%% @see ets:lookup/2

-spec lookup(lets_tab(), key()) -> [object()].
lookup(Tab, Key) ->
    gen_ets_ns:lookup(?NS, Tab, Key).

%% @doc Returns the +Pos+:th element of the object with the key +Key+
%% in the table +Tab+.
%% @end
%% @see ets:lookup_element/3

-spec lookup_element(lets_tab(), key(), pos()) -> term().
lookup_element(Tab, Key, Pos) ->
    gen_ets_ns:lookup_element(?NS, Tab, Key, Pos).

%% @doc Matches the objects in the table +Tab+ against the pattern
%% +Pattern+.
%% @end
%% @see ets:match/2

-spec match(lets_tab(), match_pattern()) -> [match()].
match(Tab, Pattern) ->
    gen_ets_ns:match(?NS, Tab, Pattern).

%% @doc Matches the objects in the table +Tab+ against the pattern
%% +Pattern+ and returns a limited (+Limit+) number of matching
%% objects.
%% @end
%% @see ets:match/3

-spec match(lets_tab(), match_pattern(), limit()) -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
match(Tab, Pattern, Limit) ->
    gen_ets_ns:match(?NS, Tab, Pattern, Limit).

%% @doc Continues a match started with +match/3+.
%% @end
%% @see ets:match/1

-spec match(cont() | '$end_of_table') -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
match(Cont) ->
    gen_ets_ns:match(?NS, Cont).

%% @doc Deletes all objects which match the pattern +Pattern+ from the
%% table +Tab+.
%% @end
%% @see ets:match_delete/2

-spec match_delete(lets_tab(), match_pattern()) -> true.
match_delete(Tab, Pattern) ->
    gen_ets_ns:match_delete(?NS, Tab, Pattern).

%% @doc Matches the objects in the table +Tab+ against the pattern
%% +Pattern+.
%% @end
%% @see ets:match_object/2

-spec match_object(lets_tab(), match_pattern()) -> [match()].
match_object(Tab, Pattern) ->
    gen_ets_ns:match_object(?NS, Tab, Pattern).

%% @doc Matches the objects in the table +Tab+ against the pattern
%% +Pattern+ and returns a limited (+Limit+) number of matching
%% objects.
%% @end
%% @see ets:match_object/3

-spec match_object(lets_tab(), match_pattern(), limit()) -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
match_object(Tab, Pattern, Limit) ->
    gen_ets_ns:match_object(?NS, Tab, Pattern, Limit).

%% @doc Continues a match started with +match_object/3+.
%% @end
%% @see ets:match_object/1

-spec match_object(cont() | '$end_of_table') -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
match_object(Cont) ->
    gen_ets_ns:match_object(?NS, Cont).

%% @doc Returns +true+ if one or more elements in the table +Tab+ has
%% the key +Key+, +false+ otherwise.
%% @end
%% @see ets:member/2

-spec member(lets_tab(), key()) -> true | false.
member(Tab, Key) ->
    gen_ets_ns:member(?NS, Tab, Key).

%% @doc Returns the next key +Key2+, following the key +Key1+ in the
%% table +Tab+.  If there is no next key, +'$end_of_table'+ is
%% returned.
%% @end
%% @see ets:next/2

-spec next(lets_tab(), key()) -> key() | '$end_of_table'.
next(Tab, Key) ->
    gen_ets_ns:next(?NS, Tab, Key).

%% @doc Returns the previous key +Key2+, following the key +Key1+ in
%% the table +Tab+.  If there is no previous key, +'$end_of_table'+ is
%% returned.
%% @end
%% @see ets:prev/2

-spec prev(lets_tab(), key()) -> key() | '$end_of_table'.
prev(Tab, Key) ->
    gen_ets_ns:prev(?NS, Tab, Key).

%% @doc Matches the objects in the table +Tab+ against the spec
%% +Spec+.
%% @end
%% @see ets:select/2

-spec select(lets_tab(), match_spec()) -> [match()].
select(Tab, Spec) ->
    gen_ets_ns:select(?NS, Tab, Spec).

%% @doc Matches the objects in the table +Tab+ against the spec +Spec+
%% and returns a limited (+Limit+) number of matching objects.
%% @end
%% @see ets:select/3

-spec select(lets_tab(), match_spec(), limit()) -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
select(Tab, Spec, Limit) ->
    gen_ets_ns:select(?NS, Tab, Spec, Limit).

%% @doc Continues a select started with +select/3+.
%% @end
%% @see ets:select/1

-spec select(cont() | '$end_of_table') -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
select(Cont) ->
    gen_ets_ns:select(?NS, Cont).

%% @doc Counts all objects which match the spec +Spec+ from the
%% table +Tab+ and returns the number matched.
%% @end
%% @see ets:select_count/2

-spec select_count(lets_tab(), match_spec()) -> pos_integer().
select_count(Tab, Spec) ->
    gen_ets_ns:select_count(?NS, Tab, Spec).

%% @doc Deletes all objects which match the spec +Spec+ from the
%% table +Tab+ and returns the number deleted.
%% @end
%% @see ets:select_delete/2

-spec select_delete(lets_tab(), match_spec()) -> pos_integer().
select_delete(Tab, Spec) ->
    gen_ets_ns:select_delete(?NS, Tab, Spec).

%% @doc Matches in reverse the objects in the table +Tab+ against the
%% spec +Spec+.
%% @end
%% @see ets:select_reverse/2

-spec select_reverse(lets_tab(), match_spec()) -> [match()].
select_reverse(Tab, Spec) ->
    gen_ets_ns:select_reverse(?NS, Tab, Spec).

%% @doc Matches in reverse the objects in the table +Tab+ against the
%% spec +Spec+ and returns a limited (+Limit+) number of matching
%% objects.
%% @end
%% @see ets:select_reverse/3

-spec select_reverse(lets_tab(), match_spec(), limit()) -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
select_reverse(Tab, Spec, Limit) ->
    gen_ets_ns:select_reverse(?NS, Tab, Spec, Limit).

%% @doc Continues a select reverse started with +select_reverse/3+.
%% @end
%% @see ets:select_reverse/1

-spec select_reverse(cont() | '$end_of_table') -> {[match()], cont() | '$end_of_table'} | '$end_of_table'.
select_reverse(Cont) ->
    gen_ets_ns:select_reverse(?NS, Cont).

%% @doc Returns a list of all objects in the table +Tab+. The
%% operation is *not* guaranteed to be atomic and isolated.
%% @end
%% @see ets:tab2list/1

-spec tab2list(lets_tab()) -> [object()].
tab2list(Tab) ->
    gen_ets_ns:tab2list(?NS, Tab).

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

create(Op, Name, Opts) ->
    POpts = case options(Opts) of
                {POpts0, []} ->
                    POpts0;
                {_POpts0, BadArgs} ->
                    erlang:error(badarg, [Name, BadArgs])
            end,

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
    Hyper = proplists:get_bool(hyper, POpts),
    Rocks = proplists:get_bool(rocks, POpts),
    Ets = proplists:get_bool(ets, POpts),

    DBOptions = fix_db_options(Name, Compressed, Async, proplists:get_value(db, POpts, [])),
    DBReadOptions = proplists:get_value(db_read, POpts, []),
    DBWriteOptions = proplists:get_value(db_write, POpts, []),

    GenOpts = [Type, {keypos, KeyPos}, Protection] ++
        [ named_table || NamedTable ] ++
        [ compressed || Compressed ] ++
        [ async || Async ],
    DBOpts = [{db, DBOptions}, {db_read, DBReadOptions}, {db_write, DBWriteOptions}],

    if Drv ->
            _ = if Hyper andalso Rocks -> erlang:error(badarg, [Name, [proplists:get_value(hyper, POpts), proplists:get_value(rocks, POpts)]]); true -> true end,
            Impl = if Hyper -> hets_impl_drv; Rocks -> rets_impl_drv; true -> lets_impl_drv end,
            DrvOpts = [{impl, {Impl, DBOpts}} | GenOpts],
            gen_ets_ns:Op(?NS, Name, DrvOpts);
       Nif ->
            _ = if Hyper andalso Rocks -> erlang:error(badarg, [Name, [proplists:get_value(hyper, POpts), proplists:get_value(rocks, POpts)]]); true -> true end,
            Impl = if Hyper -> hets_impl_nif; Rocks -> rets_impl_nif; true -> lets_impl_nif end,
            NifOpts = [{impl, {Impl, DBOpts}} | GenOpts],
            gen_ets_ns:Op(?NS, Name, NifOpts);
       Ets ->
            _ = if Hyper -> erlang:error(badarg, [Name, [proplists:get_value(hyper, POpts)]]); true -> true end,
            _ = if Rocks -> erlang:error(badarg, [Name, [proplists:get_value(rocks, POpts)]]); true -> true end,
            EtsOpts = [{impl, {gen_ets_impl_ets, []}} | GenOpts],
            gen_ets_ns:Op(?NS, Name, EtsOpts);
       true ->
            Impl = if Hyper -> hets_impl_drv; Rocks -> rets_impl_drv; true -> lets_impl_drv end,
            DrvOpts = [{impl, {Impl, DBOpts}} | GenOpts],
            gen_ets_ns:Op(?NS, Name, DrvOpts)
    end.

fix_db_options(Name, Compressed, Async, Options0) ->
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
    Keys = [set, ordered_set, named_table, keypos, public, protected, private, compressed, async, drv, nif, hyper, rocks, ets, db, db_read, db_write],
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
