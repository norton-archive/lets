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

-module(lets).

-include("lets.hrl").

%% External exports
-export([new/2
         , destroy/2
         , repair/2
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

-export_type([tab/0]).


%%%----------------------------------------------------------------------
%%% Types/Specs/Records
%%%----------------------------------------------------------------------

-opaque tab()         :: #tab{}.

-type opts()          :: [ets_opt() | impl_opt() | db_opts() | db_read_opts() | db_write_opts()].
-type ets_opt()       :: set | ordered_set | named_table | {key_pos,pos_integer()} | public | protected | private | compressed.
-type impl_opt()      :: drv | nif | ets.

-type db_opts()       :: {db, [{path,file:filename()} | create_if_missing | {create_if_missing,boolean()} | error_if_exists | {error_if_exists,boolean()} | paranoid_checks | {paranoid_checks,boolean()} | {write_buffer_size,pos_integer()} | {max_open_files,pos_integer()} | {block_cache_size,pos_integer()} | {block_size,pos_integer()} | {block_restart_interval,pos_integer()}]}.
-type db_read_opts()  :: {db_read, [verify_checksums | {verify_checksums,boolean()} | fill_cache | {fill_cache,boolean()}]}.
-type db_write_opts() :: {db_write, [sync | {sync,boolean()}]}.

-type key()           :: binary().
-type object()        :: term().


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

-spec new(Name::atom(), Options::opts()) -> tab().
new(Name, Opts) ->
    create(open, Name, Opts).

%% @doc Destroy the contents of the specified table.  This function
%% only applies to +driver+ and +nif+ implementations.
%% @end

-spec destroy(Name::atom(), Options::opts()) -> true.
destroy(Name, Opts) ->
    create(destroy, Name, Opts).


%% @doc If a table cannot be opened, you may attempt to call this
%% method to resurrect as much of the contents of the table as
%% possible.  Some data may be lost, so be careful when calling this
%% function on a table that contains important information. This
%% function only applies to +driver+ and +nif+ implementations.
%% @end

-spec repair(Name::atom(), Options::opts()) -> true.
repair(Name, Opts) ->
    create(repair, Name, Opts).

%% @doc Inserts the object or all of the objects in the list
%% +ObjectOrObjects+ into the table +Tab+.
%% @end
%% @see ets:insert/2

-spec insert(Tab::tab(), ObjectOrObjects::object() | [object()]) -> true.
insert(Tab, ObjectOrObjects) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        {Mod, Impl} ->
            Mod:insert(Tab, Impl, ObjectOrObjects)
    end.

%% @doc This function works exactly like +insert/2+, with the
%% exception that instead of overwriting objects with the same key, it
%% simply returns false.  This function only applies to the +ets+
%% implementation.
%% @end
%% @see ets:insert_new/2

-spec insert_new(tab(), object() | [object()]) -> true.
insert_new(Tab, ObjectOrObjects) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        {Mod, Impl} ->
            Mod:insert_new(Tab, Impl, ObjectOrObjects)
    end.

%% @doc Deletes the entire table +Tab+.
%% @end
%% @see ets:delete/1

-spec delete(tab()) -> true.
delete(Tab) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        {Mod, Impl} ->
            Mod:delete(Tab, Impl)
    end.

%% @doc Deletes all objects with the key +Key+ from the table +Tab+.
%% @end
%% @see ets:delete/2

-spec delete(tab(), key()) -> true.
delete(Tab, Key) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        {Mod, Impl} ->
            Mod:delete(Tab, Impl, Key)
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
        {Mod, Impl} ->
            Mod:delete_all_objects(Tab, Impl)
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
        {Mod, Impl} ->
            Mod:lookup(Tab, Impl, Key)
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
        {Mod, Impl} ->
            Mod:first(Tab, Impl)
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
        {Mod, Impl} ->
            Mod:next(Tab, Impl, Key)
    end.

%% @doc Returns information about the table +Tab+ as a list of +{Item,
%% Value}+ tuples.
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
%% - +memory+ _only the ets implementation_
%% - +size+ _only the ets implementation_
%%
%% @end
%% @see ets:info/2

-spec info(tab(), atom()) -> term().
info(Tab, Item) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        {Mod, Impl} ->
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
                memory ->
                    Mod:info_memory(Tab, Impl);
                size ->
                    Mod:info_size(Tab, Impl);
                _ ->
                    erlang:error(badarg, [Tab, Item])
            end
    end.

%% @doc Returns a list of all objects in the table +Tab+. The
%% operation is *not* guaranteed to be atomic and isolated.
%% @end
%% @see ets:tab2list/1

-spec tab2list(tab()) -> [object()].
tab2list(Tab) ->
    case check_access(Tab) of
        undefined ->
            erlang:error(badarg, [Tab]);
        {Mod, Impl} ->
            Mod:tab2list(Tab, Impl)
    end.


%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------


check_access(#tab{protection=Protection, owner=Owner, drv=Drv, nif=undefined, ets=undefined})
  when Protection==public orelse Owner==self() ->
    {lets_drv, Drv};
check_access(#tab{protection=Protection, owner=Owner, drv=undefined, nif=Nif, ets=undefined})
  when Protection==public orelse Owner==self() ->
    {lets_nif, Nif};
check_access(#tab{protection=Protection, owner=Owner, drv=undefined, nif=undefined, ets=Ets})
  when Protection==public orelse Owner==self() ->
    {lets_ets, Ets};
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
    Drv = proplists:get_bool(drv, POpts),
    Nif = proplists:get_bool(nif, POpts),
    Ets = proplists:get_bool(ets, POpts),

    Tab = #tab{owner=Owner,
               name=Name,
               named_table=NamedTable,
               type=Type,
               keypos=KeyPos,
               protection=Protection,
               compressed=Compressed},

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

fix_db_options(#tab{name=Name, compressed=Compressed}, Options) ->
    fix_db_options_compression(Compressed, fix_db_options_path(Name, Options)).

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

binify(X) when is_atom(X) ->
    list_to_binary(atom_to_list(X));
binify(X) when is_list(X) ->
    list_to_binary(X);
binify(X) when is_binary(X) ->
    X.

options(Options) ->
    Keys = [set, ordered_set, named_table, keypos, public, protected, private, compressed, drv, nif, ets, db, db_read, db_write],
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
    SubKeys = [path, create_if_missing, error_if_exists, paranoid_checks, write_buffer_size, max_open_files, block_cache_size, block_size, block_restart_interval],
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
