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

-module(qc_leveldb).

%% API
-export([%% test
         setup/0
         , teardown/0
         , is_db/1
         %% lets
         , open/0, open/1
         , reopen/0, reopen/1
         , destroy/0, destroy/1
         , repair/0, repair/1
         , close/1
         , put/2, put/3
         , delete/2, delete/3
         , get/2, get/3
         , first/1, first/2
         , last/1, last/2
         , next/2, next/3
         , prev/2, prev/3
        ]).


%%%===================================================================
%%% API
%%%===================================================================

setup() ->
    Options = [{c_src,"../c_src/leveldb/include/leveldb/c.h"},
               {additional_files, ["../c_src/leveldb/lib/libleveldb.a", "../c_src/snappy/lib/libsnappy.a"]},
               {cflags, "-lstdc++ -lpthread"}],
    DebugOptions = [%% verbose
                    %% valgrind , {exec_command_line, fun(Exe) -> {os:find_executable("valgrind"), [Exe]} end}
                   ],
    eqc_c:start(leveldb, Options ++ DebugOptions).

teardown() ->
    os:cmd("rm -rf " ++ ?MODULE_STRING).

is_db({ptr, {struct, leveldb_t}, _}) ->
    true;
is_db(_) ->
    false.

open() ->
    open([]).

open(Opts) ->
    Options = leveldb:leveldb_options_create(),
    try
        set_openoptions(Opts, Options),
        leveldb:leveldb_options_set_create_if_missing(Options, 1),
        leveldb:leveldb_options_set_error_if_exists(Options, 1),
        open1(Options)
    after
        leveldb:leveldb_options_destroy(Options)
    end.

reopen() ->
    reopen([]).

reopen(Opts) ->
    Options = leveldb:leveldb_options_create(),
    try
        set_openoptions(Opts, Options),
        open1(Options)
    after
        leveldb:leveldb_options_destroy(Options)
    end.

destroy() ->
    destroy([]).

destroy(Opts) ->
    Options = leveldb:leveldb_options_create(),
    try
        set_openoptions(Opts, Options),
        destroy1(Options)
    after
        leveldb:leveldb_options_destroy(Options)
    end.

repair() ->
    repair([]).

repair(Opts) ->
    Options = leveldb:leveldb_options_create(),
    try
        set_openoptions(Opts, Options),
        repair1(Options)
    after
        leveldb:leveldb_options_repair(Options)
    end.

close(Db) ->
    ok == leveldb:leveldb_close(Db).

put(Db, Obj) ->
    put(Db, Obj, []).

put(Db, Obj, Opts) ->
    Options = leveldb:leveldb_writeoptions_create(),
    try
        set_writeoptions(Opts, Options),
        put1(Db, Obj, Options)
    after
        leveldb:leveldb_writeoptions_destroy(Options)
    end.

delete(Db, Key) ->
    delete(Db, Key, []).

delete(Db, Key, Opts) ->
    Options = leveldb:leveldb_writeoptions_create(),
    try
        set_writeoptions(Opts, Options),
        delete1(Db, Key, Options)
    after
        leveldb:leveldb_writeoptions_destroy(Options)
    end.

get(Db, Key) ->
    get(Db, Key, []).

get(Db, Key, Opts) ->
    Options = leveldb:leveldb_readoptions_create(),
    try
        set_readoptions(Opts, Options),
        get1(Db, Key, Options)
    after
        leveldb:leveldb_readoptions_destroy(Options)
    end.

first(Db) ->
    first(Db, []).

first(Db, Opts) ->
    Options = leveldb:leveldb_readoptions_create(),
    try
        set_readoptions(Opts, Options),
        first1(Db, Options)
    after
        leveldb:leveldb_readoptions_destroy(Options)
    end.

last(Db) ->
    last(Db, []).

last(Db, Opts) ->
    Options = leveldb:leveldb_readoptions_create(),
    try
        set_readoptions(Opts, Options),
        last1(Db, Options)
    after
        leveldb:leveldb_readoptions_destroy(Options)
    end.

next(Db, Key) ->
    next(Db, Key, []).

next(Db, Key, Opts) ->
    Options = leveldb:leveldb_readoptions_create(),
    try
        set_readoptions(Opts, Options),
        next1(Db, Key, Options)
    after
        leveldb:leveldb_readoptions_destroy(Options)
    end.

prev(Db, Key) ->
    prev(Db, Key, []).

prev(Db, Key, Opts) ->
    Options = leveldb:leveldb_readoptions_create(),
    try
        set_readoptions(Opts, Options),
        prev1(Db, Key, Options)
    after
        leveldb:leveldb_readoptions_destroy(Options)
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

set_openoptions(Opts, Options) ->
    Fun = fun(paranoid_checks) ->
                  leveldb:leveldb_options_set_paranoid_checks(Options, 1);
             ({paranoid_checks, true}) ->
                  leveldb:leveldb_options_set_paranoid_checks(Options, 1);
             ({paranoid_checks, false}) ->
                  leveldb:leveldb_options_set_paranoid_checks(Options, 0);
             ({write_buffer_size, Size}) ->
                  leveldb:leveldb_options_set_write_buffer_size(Options, Size);
             ({max_open_files, Files}) ->
                  leveldb:leveldb_options_set_max_open_files(Options, Files);
             ({block_cache_size, Size}) ->
                  leveldb:leveldb_options_set_cache(Options, leveldb:leveldb_cache_create_lru(Size));
             ({block_size, Size}) ->
                  leveldb:leveldb_options_set_block_size(Options, Size);
             ({block_restart_interval, Interval}) ->
                  leveldb:leveldb_options_set_block_restart_interval(Options, Interval);
             (compression) ->
                  leveldb:leveldb_options_set_compression(Options, 1);
             ({compression, no}) ->
                  leveldb:leveldb_options_set_compression(Options, 0);
             ({compression, snappy}) ->
                  leveldb:leveldb_options_set_compression(Options, 1);
             ({filter_policy, no}) ->
                  leveldb:leveldb_options_set_filter_policy(Options, {ptr, {struct, leveldb_filterpolicy_t}, 0});
             ({filter_policy, {bloom, Bits}}) ->
                  leveldb:leveldb_options_set_filter_policy(Options, leveldb:leveldb_filterpolicy_create_bloom(Bits))
          end,
    lists:foreach(Fun, Opts).

set_writeoptions(Opts, Options) ->
    Fun = fun(sync) ->
                  leveldb:leveldb_writeoptions_set_sync(Options, 1);
             ({sync, true}) ->
                     leveldb:leveldb_writeoptions_set_sync(Options, 1);
             ({sync, false}) ->
                  leveldb:leveldb_writeoptions_set_sync(Options, 0)
          end,
    lists:foreach(Fun, Opts).

set_readoptions(Opts, Options) ->
    Fun = fun(verify_checksums) ->
                  leveldb:leveldb_readoptions_set_verify_checksums(Options, 1);
             ({verify_checksums, true}) ->
                  leveldb:leveldb_readoptions_set_verify_checksums(Options, 1);
             ({verify_checksums, false}) ->
                  leveldb:leveldb_readoptions_set_verify_checksums(Options, 0);
             (fill_cache) ->
                  leveldb:leveldb_readoptions_set_fill_cache(Options, 1);
             ({fill_cache, true}) ->
                  leveldb:leveldb_readoptions_set_fill_cache(Options, 1);
             ({fill_cache, false}) ->
                  leveldb:leveldb_readoptions_set_fill_cache(Options, 0)
          end,
    lists:foreach(Fun, Opts).

open1(Options) ->
    ErrPtr = errptr(),
    try
        case leveldb:leveldb_open(Options, ?MODULE_STRING, ErrPtr) of
            {ptr, {struct, leveldb_t}, 0} ->
                read_errptr(ErrPtr);
            {ptr, {struct, leveldb_t}, _}=Db ->
                Db
        end
    after
        free_ptr(ErrPtr)
    end.

destroy1(Options) ->
    ErrPtr = errptr(),
    try
        leveldb:leveldb_destroy(Options, ?MODULE_STRING, ErrPtr),
        read_errptr(ErrPtr)
    after
        free_ptr(ErrPtr)
    end.

repair1(Options) ->
    ErrPtr = errptr(),
    try
        leveldb:leveldb_repair(Options, ?MODULE_STRING, ErrPtr),
        read_errptr(ErrPtr)
    after
        free_ptr(ErrPtr)
    end.

put1(Db, {obj,Key,Val}, Options) ->
    ErrPtr = errptr(),
    try
        leveldb:leveldb_put(Db, Options, binary_to_list(Key), byte_size(Key), binary_to_list(Val), byte_size(Val), ErrPtr),
        read_errptr(ErrPtr)
    after
        free_ptr(ErrPtr)
    end.

delete1(Db, Key, Options) ->
    ErrPtr = errptr(),
    try
        leveldb:leveldb_delete(Db, Options, binary_to_list(Key), byte_size(Key), ErrPtr),
        read_errptr(ErrPtr)
    after
        free_ptr(ErrPtr)
    end.

get1(Db, Key, Options) ->
    ErrPtr = errptr(),
    LenPtr = lenptr(),
    try
        case leveldb:leveldb_get(Db, Options, binary_to_list(Key), byte_size(Key), LenPtr, ErrPtr) of
            0 ->
                read_errptr(ErrPtr);
            ValPtr ->
                read_binary(ValPtr, LenPtr)
        end
    after
        free_ptr(ErrPtr),
        free_ptr(LenPtr)
    end.

first1(Db, Options) ->
    Iter = leveldb:leveldb_create_iterator(Db, Options),
    LenPtr = lenptr(),
    try
        leveldb:leveldb_iter_seek_to_first(Iter),
        case leveldb:leveldb_iter_valid(Iter) of
            0 ->
                true;
            1 ->
                KeyPtr = leveldb:leveldb_iter_key(Iter, LenPtr),
                read_binary(KeyPtr, LenPtr)
        end
    after
        leveldb:leveldb_iter_destroy(Iter),
        free_ptr(LenPtr)
    end.

last1(Db, Options) ->
    Iter = leveldb:leveldb_create_iterator(Db, Options),
    LenPtr = lenptr(),
    try
        leveldb:leveldb_iter_seek_to_last(Iter),
        case leveldb:leveldb_iter_valid(Iter) of
            0 ->
                true;
            1 ->
                KeyPtr = leveldb:leveldb_iter_key(Iter, LenPtr),
                read_binary(KeyPtr, LenPtr)
        end
    after
        leveldb:leveldb_iter_destroy(Iter),
        free_ptr(LenPtr)
    end.

next1(Db, Key, Options) ->
    Iter = leveldb:leveldb_create_iterator(Db, Options),
    LenPtr = lenptr(),
    LenPtr1 = lenptr(),
    try
        leveldb:leveldb_iter_seek(Iter, binary_to_list(Key), byte_size(Key)),
        case leveldb:leveldb_iter_valid(Iter) of
            0 ->
                true;
            1 ->
                KeyPtr = leveldb:leveldb_iter_key(Iter, LenPtr),
                K = read_binary(KeyPtr, LenPtr),
                if K =/= Key ->
                        K;
                   true ->
                        leveldb:leveldb_iter_next(Iter),
                        case leveldb:leveldb_iter_valid(Iter) of
                            0 ->
                                true;
                            1 ->
                                KeyPtr1 = leveldb:leveldb_iter_key(Iter, LenPtr1),
                                read_binary(KeyPtr1, LenPtr1)
                        end
                end
        end
    after
        leveldb:leveldb_iter_destroy(Iter),
        free_ptr(LenPtr),
        free_ptr(LenPtr1)
    end.

prev1(Db, Key, Options) ->
    Iter = leveldb:leveldb_create_iterator(Db, Options),
    LenPtr = lenptr(),
    LenPtr1 = lenptr(),
    try
        leveldb:leveldb_iter_seek(Iter, binary_to_list(Key), byte_size(Key)),
        case leveldb:leveldb_iter_valid(Iter) of
            0 ->
                last1(Db, Options);
            1 ->
                leveldb:leveldb_iter_prev(Iter),
                case leveldb:leveldb_iter_valid(Iter) of
                    0 ->
                        true;
                    1 ->
                        KeyPtr1 = leveldb:leveldb_iter_key(Iter, LenPtr1),
                        read_binary(KeyPtr1, LenPtr1)
                end
        end
    after
        leveldb:leveldb_iter_destroy(Iter),
        free_ptr(LenPtr),
        free_ptr(LenPtr1)
    end.

errptr() ->
    eqc_c:alloc({ptr, char}, [0]).

lenptr() ->
    eqc_c:alloc(unsigned_long, 0).

free_ptr(Ptr) ->
    eqc_c:free(Ptr).

read_errptr(ErrPtr) ->
    case eqc_c:read_string(eqc_c:deref(ErrPtr)) of
        [] ->
            true;
        Err ->
            Err
    end.

read_lenptr(LenPtr) ->
    eqc_c:deref(LenPtr).

read_binary({ptr, char, 0}, _LenPtr) ->
    true;
read_binary(ValPtr, LenPtr) ->
    list_to_binary(eqc_c:read_array(ValPtr, read_lenptr(LenPtr))).
