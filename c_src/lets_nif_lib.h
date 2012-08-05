// The MIT License
//
// Copyright (C) 2011-2012 by Joseph Wayne Norton <norton@alum.mit.edu>
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

#ifndef LETS_NIF_LIB_H
#define LETS_NIF_LIB_H

#include <string>
#include <boost/shared_ptr.hpp>

#include "leveldb/db.h"
#include "leveldb/cache.h"
#include "leveldb/slice.h"
#include "leveldb/write_batch.h"
#include "leveldb/filter_policy.h"

#include "erl_nif.h"

#ifdef __cplusplus
extern "C" {
#endif

    typedef struct {
        long buflen;
        char buf[1];
    } Binary;

    enum Type {
        SET         = 0x0,
        ORDERED_SET = 0x1
    };

    enum PrivacyType {
        PRIVATE     = 0x0,
        PROTECTED   = 0x1,
        PUBLIC      = 0x2
    };

    enum DBOpType {
        OPEN        = 0x0,
        DESTROY     = 0x1,
        REPAIR      = 0x2,
        DELETEALL   = 0x3
    };

    typedef struct
    {
        typedef boost::shared_ptr<leveldb::DB> DBPtr;

        bool async;
        bool alive;
        char type;
        char privacy;
        std::string* name;
        leveldb::Options db_options;
        leveldb::ReadOptions db_read_options;
        leveldb::WriteOptions db_write_options;
        size_t db_block_cache_size;
        leveldb::Cache* db_block_cache;
        int db_filter_policy_bloom_bits_per_key;
        const leveldb::FilterPolicy* db_filter_policy;
        DBPtr db;
        ErlNifUInt64 db_memory;
        ErlNifUInt64 db_size;
    } lets_impl;

    extern ERL_NIF_TERM lets_atom_true;
    extern ERL_NIF_TERM lets_atom_false;
    extern ERL_NIF_TERM lets_atom_set;
    extern ERL_NIF_TERM lets_atom_ordered_set;
    extern ERL_NIF_TERM lets_atom_private;
    extern ERL_NIF_TERM lets_atom_protected;
    extern ERL_NIF_TERM lets_atom_public;
    extern ERL_NIF_TERM lets_atom_create_if_missing;
    extern ERL_NIF_TERM lets_atom_error_if_exists;
    extern ERL_NIF_TERM lets_atom_paranoid_checks;
    extern ERL_NIF_TERM lets_atom_write_buffer_size;
    extern ERL_NIF_TERM lets_atom_max_open_files;
    extern ERL_NIF_TERM lets_atom_block_cache_size;
    extern ERL_NIF_TERM lets_atom_block_size;
    extern ERL_NIF_TERM lets_atom_block_restart_interval;
    extern ERL_NIF_TERM lets_atom_compression;
    extern ERL_NIF_TERM lets_atom_no;
    extern ERL_NIF_TERM lets_atom_snappy;
    extern ERL_NIF_TERM lets_atom_async;
    extern ERL_NIF_TERM lets_atom_filter_policy;
    extern ERL_NIF_TERM lets_atom_bloom;
    extern ERL_NIF_TERM lets_atom_verify_checksums;
    extern ERL_NIF_TERM lets_atom_fill_cache;
    extern ERL_NIF_TERM lets_atom_sync;
    extern ERL_NIF_TERM lets_atom_end_of_table;

    // prototypes
    extern bool lets_nif_lib_init(ErlNifEnv* env);

    extern bool lets_init(lets_impl& impl,
                          const char type, const char privacy, const char* name, const size_t namelen);
    extern bool lets_create(lets_impl& impl,
                            const char op);

    extern bool lets_parse_options(ErlNifEnv* env, lets_impl& impl,
                                   ERL_NIF_TERM& options, const ERL_NIF_TERM& options_len);
    extern bool lets_parse_read_options(ErlNifEnv* env, lets_impl& impl,
                                        ERL_NIF_TERM& options, const ERL_NIF_TERM& options_len);
    extern bool lets_parse_write_options(ErlNifEnv* env, lets_impl& impl,
                                         ERL_NIF_TERM& options, const ERL_NIF_TERM& options_len);

#ifdef __cplusplus
}
#endif

#endif /* LETS_NIF_LIB_H */
