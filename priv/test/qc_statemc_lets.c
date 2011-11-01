// %%% The MIT License
// %%%
// %%% Copyright (C) 2011 by Joseph Wayne Norton <norton@alum.mit.edu>
// %%%
// %%% Permission is hereby granted, free of charge, to any person obtaining a copy
// %%% of this software and associated documentation files (the "Software"), to deal
// %%% in the Software without restriction, including without limitation the rights
// %%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// %%% copies of the Software, and to permit persons to whom the Software is
// %%% furnished to do so, subject to the following conditions:
// %%%
// %%% The above copyright notice and this permission notice shall be included in
// %%% all copies or substantial portions of the Software.
// %%%
// %%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// %%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// %%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// %%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// %%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// %%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// %%% THE SOFTWARE.

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "qc_statemc_lets.h"


// %%%===================================================================
// %%% API
// %%%===================================================================
//
// setup() ->
//     Options = [{c_src,"../c_src/leveldb/include/leveldb/c.h"},
//                {additional_files, ["../c_src/leveldb/lib/libleveldb.a", "../c_src/snappy/lib/libsnappy.a"]},
//                {cflags, "-lstdc++ -lpthread"}],
//     eqc_c:start(leveldb, Options).
//
// teardown() ->
//     os:cmd("rm -rf " ++ ?MODULE_STRING).
//
// is_db({ptr, {struct, leveldb_t}, _}) ->
//     true;
// is_db(_) ->
//     false.


// open() ->
//     Options = leveldb:leveldb_options_create(),
//     try
//         Leveldb:leveldb_options_set_create_if_missing(Options, 1),
//         leveldb:leveldb_options_set_error_if_exists(Options, 1),
//         open(Options)
//     after
//         leveldb:leveldb_options_destroy(Options)
//     end.

leveldb_t* open0() {
    leveldb_options_t* Options = leveldb_options_create();
    leveldb_options_set_create_if_missing(Options, 1);
    leveldb_options_set_error_if_exists(Options, 1);
    leveldb_t* Db = open1(Options);
    leveldb_options_destroy(Options);
    return Db;
}


// open(Options) ->
//     ErrPtr = errptr(),
//     try
//         case leveldb:leveldb_open(Options, ?MODULE_STRING, ErrPtr) of
//             {ptr, {struct, leveldb_t}, 0} ->
//                 read_errptr(ErrPtr);
//             {ptr, {struct, leveldb_t}, _}=Db ->
//                 Db
//         end
//     after
//         free_ptr(ErrPtr)
//     end.

leveldb_t* open1(leveldb_options_t* Options) {
    char* ErrPtr = NULL;
    leveldb_t* Db = leveldb_open(Options, "qc_leveldb", &ErrPtr);
    assert(ErrPtr == NULL);
    return Db;
}


// reopen() ->
//     Options = leveldb:leveldb_options_create(),
//     try
//         reopen(Options)
//     after
//         leveldb:leveldb_options_destroy(Options)
//     end.

leveldb_t* reopen0() {
    leveldb_options_t* Options = leveldb_options_create();
    leveldb_t* Db = reopen1(Options);
    leveldb_options_destroy(Options);
    return Db;
}


// reopen(Options) ->
//     ErrPtr = errptr(),
//     try
//         case leveldb:leveldb_open(Options, ?MODULE_STRING, ErrPtr) of
//             {ptr, {struct, leveldb_t}, 0} ->
//                 read_errptr(ErrPtr);
//             {ptr, {struct, leveldb_t}, _}=Db ->
//                 Db
//         end
//     after
//         free_ptr(ErrPtr)
//     end.

leveldb_t* reopen1(leveldb_options_t* Options) {
    char* ErrPtr = NULL;
    leveldb_t* Db = leveldb_open(Options, "qc_leveldb", &ErrPtr);
    assert(ErrPtr == NULL);
    return Db;
}


// close(Db) ->
//     ok == leveldb:leveldb_close(Db).

void close1(leveldb_t* Db) {
    leveldb_close(Db);
    return;
}


// put(Db, Obj) ->
//     Options = leveldb:leveldb_writeoptions_create(),
//     try
//         put(Db, Options, Obj)
//     after
//         leveldb:leveldb_writeoptions_destroy(Options)
//     end.

void put2(leveldb_t* Db, char* Key, size_t KeyLen, char* Val, size_t ValLen) {
    leveldb_writeoptions_t* Options = leveldb_writeoptions_create();
    put3(Db, Key, KeyLen, Val, ValLen, Options);
    leveldb_writeoptions_destroy(Options);
    return;
}


// put(Db, Options, {obj,Key,Val}) ->
//     ErrPtr = errptr(),
//     try
//         leveldb:leveldb_put(Db, Options, binary_to_list(Key), byte_size(Key), binary_to_list(Val), byte_size(Val), ErrPtr),
//         read_errptr(ErrPtr)
//     after
//         free_ptr(ErrPtr)
//     end.

void put3(leveldb_t* Db, char* Key, size_t KeyLen, char* Val, size_t ValLen, const leveldb_writeoptions_t* Options) {
    char* ErrPtr = NULL;
    leveldb_put(Db, Options, Key, KeyLen, Val, ValLen, &ErrPtr);
    assert(ErrPtr == NULL);
    return;
}


// delete(Db, Key) ->
//     Options = leveldb:leveldb_writeoptions_create(),
//     try
//         delete(Db, Options, Key)
//     after
//         leveldb:leveldb_writeoptions_destroy(Options)
//     end.

void delete2(leveldb_t* Db, char* Key, size_t KeyLen) {
    leveldb_writeoptions_t* Options = leveldb_writeoptions_create();
    delete3(Db, Key, KeyLen, Options);
    leveldb_writeoptions_destroy(Options);
    return;
}


// delete(Db, Options, Key) ->
//     ErrPtr = errptr(),
//     try
//         leveldb:leveldb_delete(Db, Options, binary_to_list(Key), byte_size(Key), ErrPtr),
//         read_errptr(ErrPtr)
//     after
//         free_ptr(ErrPtr)
//     end.

void delete3(leveldb_t* Db, char* Key, size_t KeyLen, const leveldb_writeoptions_t* Options) {
    char* ErrPtr = NULL;
    leveldb_delete(Db, Options, Key, KeyLen, &ErrPtr);
    assert(ErrPtr == NULL);
    return;
}


// get(Db, Key) ->
//     Options = leveldb:leveldb_readoptions_create(),
//     try
//         get(Db, Options, Key)
//     after
//         leveldb:leveldb_readoptions_destroy(Options)
//     end.


char* get2(leveldb_t* Db, char* Key, size_t KeyLen, size_t* ValLen) {
    leveldb_readoptions_t* Options = leveldb_readoptions_create();
    char* Val = get3(Db, Key, KeyLen, ValLen, Options);
    leveldb_readoptions_destroy(Options);
    return Val;
}


// get(Db, Options, Key) ->
//     ErrPtr = errptr(),
//     LenPtr = lenptr(),
//     try
//         case leveldb:leveldb_get(Db, Options, binary_to_list(Key), byte_size(Key), LenPtr, ErrPtr) of
//             0 ->
//                 read_errptr(ErrPtr);
//             ValPtr ->
//                 read_binary(ValPtr, LenPtr)
//         end
//     after
//         free_ptr(ErrPtr),
//         free_ptr(LenPtr)
//     end.


char* get3(leveldb_t* Db, char* Key, size_t KeyLen, size_t* ValLen, const leveldb_readoptions_t* Options) {
    char* ErrPtr = NULL;
    char* Val = leveldb_get(Db, Options, Key, KeyLen, ValLen, &ErrPtr);
    assert(ErrPtr == NULL);
    return Val;
}


// first(Db) ->
//     Options = leveldb:leveldb_readoptions_create(),
//     try
//         first(Db, Options)
//     after
//         leveldb:leveldb_readoptions_destroy(Options)
//     end.

char* first1(leveldb_t* Db, size_t *KeyLen) {
    leveldb_readoptions_t* Options = leveldb_readoptions_create();
    char* Key = first2(Db, KeyLen, Options);
    leveldb_readoptions_destroy(Options);
    return Key;
}


// first(Db, Options) ->
//     Iter = leveldb:leveldb_create_iterator(Db, Options),
//     LenPtr = lenptr(),
//     try
//         leveldb:leveldb_iter_seek_to_first(Iter),
//         case leveldb:leveldb_iter_valid(Iter) of
//             0 ->
//                 true;
//             1 ->
//                 KeyPtr = leveldb:leveldb_iter_key(Iter, LenPtr),
//                 read_binary(KeyPtr, LenPtr)
//         end
//     after
//         leveldb:leveldb_iter_destroy(Iter),
//         free_ptr(LenPtr)
//     end.

char* first2(leveldb_t* Db, size_t* KeyLen, const leveldb_readoptions_t* Options) {
    leveldb_iterator_t* Iter = leveldb_create_iterator(Db, Options);
    char* Key = NULL;
    *KeyLen = 0;

    leveldb_iter_seek_to_first(Iter);
    if (leveldb_iter_valid(Iter)) {
        Key = (char*) leveldb_iter_key(Iter, KeyLen);
        Key = read_binary(Key, KeyLen);
    }

    leveldb_iter_destroy(Iter);
    return Key;
}

// last(Db) ->
//     Options = leveldb:leveldb_readoptions_create(),
//     try
//         last(Db, Options)
//     after
//         leveldb:leveldb_readoptions_destroy(Options)
//     end.

char* last1(leveldb_t* Db, size_t *KeyLen) {
    leveldb_readoptions_t* Options = leveldb_readoptions_create();
    char* Key = last2(Db, KeyLen, Options);
    leveldb_readoptions_destroy(Options);
    return Key;
}


// last(Db, Options) ->
//     Iter = leveldb:leveldb_create_iterator(Db, Options),
//     LenPtr = lenptr(),
//     try
//         leveldb:leveldb_iter_seek_to_last(Iter),
//         case leveldb:leveldb_iter_valid(Iter) of
//             0 ->
//                 true;
//             1 ->
//                 KeyPtr = leveldb:leveldb_iter_key(Iter, LenPtr),
//                 read_binary(KeyPtr, LenPtr)
//         end
//     after
//         leveldb:leveldb_iter_destroy(Iter),
//         free_ptr(LenPtr)
//     end.

char* last2(leveldb_t* Db, size_t* KeyLen, const leveldb_readoptions_t* Options) {
    leveldb_iterator_t* Iter = leveldb_create_iterator(Db, Options);
    char* Key = NULL;
    *KeyLen = 0;

    leveldb_iter_seek_to_last(Iter);
    if (leveldb_iter_valid(Iter)) {
        Key = (char*) leveldb_iter_key(Iter, KeyLen);
        Key = read_binary(Key, KeyLen);
    }

    leveldb_iter_destroy(Iter);
    return Key;
}


// next(Db, Key) ->
//     Options = leveldb:leveldb_readoptions_create(),
//     try
//         next(Db, Key, Options)
//     after
//         leveldb:leveldb_readoptions_destroy(Options)
//     end.

char* next2(leveldb_t* Db, char* Key, size_t KeyLen, size_t *NextKeyLen) {
    leveldb_readoptions_t* Options = leveldb_readoptions_create();
    char* NextKey = next3(Db, Key, KeyLen, NextKeyLen, Options);
    leveldb_readoptions_destroy(Options);
    return NextKey;
}


// next(Db, Key, Options) ->
//     Iter = leveldb:leveldb_create_iterator(Db, Options),
//     LenPtr = lenptr(),
//     LenPtr1 = lenptr(),
//     try
//         leveldb:leveldb_iter_seek(Iter, binary_to_list(Key), byte_size(Key)),
//         case leveldb:leveldb_iter_valid(Iter) of
//             0 ->
//                 true;
//             1 ->
//                 KeyPtr = leveldb:leveldb_iter_key(Iter, LenPtr),
//                 K = read_binary(KeyPtr, LenPtr),
//                 if K =/= Key ->
//                         K;
//                    true ->
//                         leveldb:leveldb_iter_next(Iter),
//                         case leveldb:leveldb_iter_valid(Iter) of
//                             0 ->
//                                 true;
//                             1 ->
//                                 KeyPtr1 = leveldb:leveldb_iter_key(Iter, LenPtr1),
//                                 read_binary(KeyPtr1, LenPtr1)
//                         end
//                 end
//         end
//     after
//         leveldb:leveldb_iter_destroy(Iter),
//         free_ptr(LenPtr),
//         free_ptr(LenPtr1)
//     end.

char* next3(leveldb_t* Db, char* Key, size_t KeyLen, size_t* NextKeyLen, const leveldb_readoptions_t* Options) {
    leveldb_iterator_t* Iter = leveldb_create_iterator(Db, Options);
    char* NextKey = NULL;
    *NextKeyLen = 0;

    leveldb_iter_seek(Iter, Key, KeyLen);
    if (leveldb_iter_valid(Iter)) {
        NextKey = (char*) leveldb_iter_key(Iter, NextKeyLen);

        if (0 == compare_binary(Key, KeyLen, NextKey, *NextKeyLen)) {
            leveldb_iter_next(Iter);

            if (leveldb_iter_valid(Iter)) {
                NextKey = (char*) leveldb_iter_key(Iter, NextKeyLen);
                NextKey = read_binary(NextKey, NextKeyLen);
            } else {
                NextKey = NULL;
                *NextKeyLen = 0;
            }
        } else {
            NextKey = read_binary(NextKey, NextKeyLen);
        }
    }

    leveldb_iter_destroy(Iter);
    return NextKey;
}

// %%%===================================================================
// %%% Internal
// %%%===================================================================
//
// errptr() ->
//     eqc_c:alloc({ptr, char}, [0]).
//
// lenptr() ->
//     eqc_c:alloc(unsigned_long, 0).
//
// free_ptr(Ptr) ->
//     eqc_c:free(Ptr).
//
// read_errptr(ErrPtr) ->
//     case eqc_c:read_string(eqc_c:deref(ErrPtr)) of
//         [] ->
//             true;
//         Err ->
//             Err
//     end.
//
// read_lenptr(LenPtr) ->
//     eqc_c:deref(LenPtr).
//
// read_binary({ptr, char, 0}, _LenPtr) ->
//     true;
// read_binary(ValPtr, LenPtr) ->
//     list_to_binary(eqc_c:read_array(ValPtr, read_lenptr(LenPtr))).


char* read_binary(const char* B, size_t* BLen) {
    char* tmp = malloc(*BLen);
    if (tmp) {
        tmp = memcpy(tmp, B, *BLen);
    } else {
        *BLen = 0;
    }

    return tmp;
}


int compare_binary(const char* A, size_t ALen, const char* B, size_t BLen) {
    if (ALen > BLen) {
        return 1;
    } else if (ALen < BLen) {
        return -1;
    } else {
        return memcmp(A, B, ALen);
    }
}
