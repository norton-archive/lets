// %%% The MIT License
// %%%
// %%% Copyright (C) 2011-2012 by Joseph Wayne Norton <norton@alum.mit.edu>
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

#ifndef QC_STATEMC_LETS_H
#define QC_STATEMC_LETS_H

#include "leveldb/c.h"

// -module(qc_leveldb).
//
// %% API
// -export([%% test
//          setup/0
//          , teardown/0
//          , is_db/1
//          %% lets
//          , open/0, open/1
//          , reopen/0, reopen/1
//          , close/1
//          , put/2, put/3
//          , delete/2, delete/3
//          , get/2, get/3
//          , first/1, first/2
//          , last/1, last/2
//          , next/2, next/3
//         ]).

extern leveldb_t* open0();
extern leveldb_t* open1(leveldb_options_t* Options);

extern leveldb_t* reopen0();
extern leveldb_t* reopen1(leveldb_options_t* Options);

extern void close1(leveldb_t* Db);

extern void put2(leveldb_t* Db, char* Key, size_t KeyLen, char* Val, size_t ValLen);
extern void put3(leveldb_t* Db, char* Key, size_t KeyLen, char* Val, size_t ValLen, const leveldb_writeoptions_t* Options);

extern void delete2(leveldb_t* Db, char* Key, size_t KeyLen);
extern void delete3(leveldb_t* Db, char* Key, size_t KeyLen, const leveldb_writeoptions_t* Options);

extern char* get2(leveldb_t* Db, char* Key, size_t KeyLen, size_t* ValLen);
extern char* get3(leveldb_t* Db, char* Key, size_t KeyLen, size_t* ValLen, const leveldb_readoptions_t* Options);

extern char* first1(leveldb_t* Db, size_t *KeyLen);
extern char* first2(leveldb_t* Db, size_t* KeyLen, const leveldb_readoptions_t* Options);

extern char* last1(leveldb_t* Db, size_t *KeyLen);
extern char* last2(leveldb_t* Db, size_t* KeyLen, const leveldb_readoptions_t* Options);

extern char* next2(leveldb_t* Db, char* Key, size_t KeyLen, size_t* NextKeyLen);
extern char* next3(leveldb_t* Db, char* Key, size_t KeyLen, size_t* NextKeyLen, const leveldb_readoptions_t* Options);

extern char* read_binary(const char* B, size_t* BLen);
extern int compare_binary(const char* A, size_t ALen, const char* B, size_t BLen);

#endif /* QC_STATEMC_LETS_H */
