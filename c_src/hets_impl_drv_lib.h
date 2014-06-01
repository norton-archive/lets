// The MIT License
//
// Copyright (C) 2011-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
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

#ifndef HETS_DRV_LIB_H
#define HETS_DRV_LIB_H

#include <string>
#include <vector>

#include "hyperleveldb/db.h"
#include "hyperleveldb/cache.h"
#include "hyperleveldb/slice.h"
#include "hyperleveldb/write_batch.h"
#include "hyperleveldb/filter_policy.h"

#include "hets_impl_drv.h"

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
        REPAIR      = 0x2
    };

    typedef struct
    {
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
        leveldb::DB* db;
        ErlDrvUInt64 db_memory;
        ErlDrvUInt64 db_size;
        std::vector<std::pair<ErlDrvTermData,ErlDrvBinary*> > notify_when_destroyed;
    } lets_impl;

    // prototypes
    extern bool lets_impl_drv_lib_init();

    extern bool lets_init(lets_impl& impl,
                          const char type, const char privacy, const char* name, const size_t namelen);
    extern bool lets_create(lets_impl& impl,
                            const char op);

    extern bool lets_parse_options(lets_impl& impl,
                                   const char* buf, ErlDrvSizeT len);
    extern bool lets_parse_read_options(leveldb::ReadOptions& opts,
                                        const char* buf, ErlDrvSizeT len);
    extern bool lets_parse_write_options(leveldb::WriteOptions& opts,
                                         const char* buf, ErlDrvSizeT len);

    // helpers
    extern int ei_inspect_atom(const char *buf, int *index, char *p);
    extern int ei_inspect_binary(const char *buf, int *index, void **p, long *lenp);

#ifdef __cplusplus
}
#endif

#endif /* HETS_DRV_LIB_H */
