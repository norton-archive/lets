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

#include "lets_impl_drv_lib.h"

#include <ei.h>


#if 0
static bool
return_false(unsigned line) {
    fprintf(stderr, "FALSE %s:%d\n", __FILE__, line);
    return false;
}
#define FALSE return_false(__LINE__)
#else
#define FALSE false
#endif


bool
lets_impl_drv_lib_init()
{
    return true;
}

bool
lets_init(lets_impl& impl,
          const char type, const char privacy, const char* name, const size_t namelen)
{
    impl.type = type;
    impl.privacy = privacy;
    impl.name = new std::string(name, namelen);
    if (!impl.name) {
        return FALSE;
    }

    impl.db_options = leveldb::Options();
    impl.db_read_options = leveldb::ReadOptions();
    impl.db_write_options = leveldb::WriteOptions();

    return true;
}

bool
lets_create(lets_impl& impl,
            const char op)
{
    leveldb::Status status;
    leveldb::DB* db;

    // db
    switch (op) {
    case OPEN:
        status = leveldb::DB::Open(impl.db_options, impl.name->c_str(), &db);
        if (!status.ok()) {
            return FALSE;
        } else {
            impl.db.reset(db);
            // alive
            impl.alive = 1;
        }
        break;
    case DESTROY:
        status = DestroyDB(impl.name->c_str(), impl.db_options);
        if (!status.ok()) {
            return FALSE;
        }
        break;
    case REPAIR:
        status = RepairDB(impl.name->c_str(), impl.db_options);
        if (!status.ok()) {
            return FALSE;
        }
        break;
    case DELETEALL:
        // Prototype delete all objects implementation
        {
            lets_impl::DBPtr impldb = impl.db;
            if (!impl.alive) {
                return FALSE;
            }

            size_t templen = strlen(impl.name->c_str()) + 8;
            char temp[templen];

            snprintf(temp, templen, "%s.XXXXXX", impl.name->c_str());
            if (mkdtemp(temp) == NULL) {
                return false;
            }
            if (rmdir(temp) != 0) {
                return false;
            }
            if (rename(impl.name->c_str(), temp) != 0) {
                return false;
            }

            leveldb::Options db_options = impl.db_options;
            db_options.create_if_missing = true;
            db_options.error_if_exists = true;
            status = leveldb::DB::Open(db_options, impl.name->c_str(), &db);
            if (!status.ok()) {
                if (rename(temp, impl.name->c_str()) != 0) {
                    // TODO - add alert since recovery is not possible
                }
                return false;
            } else {
                lets_impl::DBPtr newdb(db);
                leveldb::Options newdb_options = impl.db_options;
                newdb_options.create_if_missing = false;
                newdb_options.error_if_exists = false;
                impl.db.swap(newdb);
                impldb = impl.db;

                // @TODO cleanup asynchronously
                newdb.reset();
                status = DestroyDB(temp, newdb_options);
                if (!status.ok()) {
                    return false;
                }
            }
        }
        break;
    default:
        return FALSE;
    }

    return true;
}

bool
lets_parse_options(lets_impl& impl,
                   const char* buf, ErlDrvSizeT len)
{
    (void) len;

    int index, ng, items, arity, term_type, size_needed;
    char atom[MAXATOMLEN];

    index = 0;
    ng = ei_decode_list_header(buf, &index, &items);
    if (ng) return FALSE;
    ng = (items < 0);
    if (ng) return FALSE;

    if (!items) return true;

    while (items) {
        ng = ei_get_type(buf, &index, &term_type, &size_needed);
        if (ng) return FALSE;

        switch (term_type) {
        case ERL_ATOM_EXT:
            ng = ei_decode_atom(buf, &index, atom);
            if (ng) return FALSE;
            if (strcmp(atom, "create_if_missing") == 0) {
                impl.db_options.create_if_missing = true;
            } else if (strcmp(atom, "error_if_exists") == 0) {
                impl.db_options.error_if_exists = true;
            } else if (strcmp(atom, "paranoid_checks") == 0) {
                impl.db_options.paranoid_checks = true;
            } else {
                return FALSE;
            }
            break;
        case ERL_SMALL_TUPLE_EXT:
            ng = ei_decode_tuple_header(buf, &index, &arity);
            if (ng) return FALSE;
            ng = (arity != 2);
            if (ng) return FALSE;
            unsigned long val;
            ng = ei_decode_atom(buf, &index, atom);
            if (ng) return FALSE;
            if (strcmp(atom, "create_if_missing") == 0) {
                ng = ei_decode_atom(buf, &index, atom);
                if (ng) return FALSE;
                if (strcmp(atom, "true") == 0) {
                    impl.db_options.create_if_missing = true;
                } else if (strcmp(atom, "false") == 0) {
                    impl.db_options.create_if_missing = false;
                } else {
                    return FALSE;
                }
            } else if (strcmp(atom, "error_if_exists") == 0) {
                ng = ei_decode_atom(buf, &index, atom);
                if (ng) return FALSE;
                if (strcmp(atom, "true") == 0) {
                    impl.db_options.error_if_exists = true;
                } else if (strcmp(atom, "false") == 0) {
                    impl.db_options.error_if_exists = false;
                } else {
                    return FALSE;
                }
            } else if (strcmp(atom, "paranoid_checks") == 0) {
                ng = ei_decode_atom(buf, &index, atom);
                if (ng) return FALSE;
                if (strcmp(atom, "true") == 0) {
                    impl.db_options.paranoid_checks = true;
                } else if (strcmp(atom, "false") == 0) {
                    impl.db_options.paranoid_checks = false;
                } else {
                    return FALSE;
                }
            } else if (strcmp(atom, "write_buffer_size") == 0) {
                ng = ei_decode_ulong(buf, &index, &val);
                if (ng) return FALSE;
                impl.db_options.write_buffer_size = val;
            } else if (strcmp(atom, "max_open_files") == 0) {
                ng = ei_decode_ulong(buf, &index, &val);
                if (ng) return FALSE;
                impl.db_options.max_open_files = val;
            } else if (strcmp(atom, "block_cache_size") == 0) {
                ng = ei_decode_ulong(buf, &index, &val);
                if (ng) return FALSE;
                impl.db_block_cache_size = val;
                delete impl.db_block_cache;
                impl.db_block_cache = leveldb::NewLRUCache(impl.db_block_cache_size);
                impl.db_options.block_cache = impl.db_block_cache;
                if (!impl.db_options.block_cache) {
                    return FALSE;
                }
            } else if (strcmp(atom, "block_size") == 0) {
                ng = ei_decode_ulong(buf, &index, &val);
                if (ng) return FALSE;
                impl.db_options.block_size = val;
            } else if (strcmp(atom, "block_restart_interval") == 0) {
                ng = ei_decode_ulong(buf, &index, &val);
                if (ng) return FALSE;
                impl.db_options.block_restart_interval = val;
            } else if (strcmp(atom, "compression") == 0) {
                ng = ei_decode_atom(buf, &index, atom);
                if (ng) return FALSE;
                if (strcmp(atom, "no") == 0) {
                    impl.db_options.compression = leveldb::kNoCompression;
                } else if (strcmp(atom, "snappy") == 0) {
                    impl.db_options.compression = leveldb::kSnappyCompression;
                } else {
                    return FALSE;
                }
            } else if (strcmp(atom, "filter_policy") == 0) {
                ng = ei_decode_atom(buf, &index, atom);
                if (ng) {
                    ng = ei_decode_tuple_header(buf, &index, &arity);
                    if (ng) return FALSE;
                    ng = (arity != 2);
                    if (ng) return FALSE;
                    ng = ei_decode_atom(buf, &index, atom);
                    if (ng) return FALSE;
                    if (strcmp(atom, "bloom") == 0) {
                        ng = ei_decode_ulong(buf, &index, &val);
                        if (ng) return FALSE;
                        impl.db_filter_policy_bloom_bits_per_key = val;
                        delete impl.db_filter_policy;
                        impl.db_filter_policy = leveldb::NewBloomFilterPolicy(impl.db_filter_policy_bloom_bits_per_key);
                        impl.db_options.filter_policy = impl.db_filter_policy;
                        if (!impl.db_options.filter_policy) {
                            return FALSE;
                        }
                    } else {
                        return FALSE;
                    }
                } else if (strcmp(atom, "no") == 0) {
                    delete impl.db_filter_policy;
                    impl.db_filter_policy = NULL;
                    impl.db_options.filter_policy = NULL;
                } else {
                    return FALSE;
                }
            } else if (strcmp(atom, "async") == 0) {
                ng = ei_decode_atom(buf, &index, atom);
                if (ng) return FALSE;
                if (strcmp(atom, "true") == 0) {
                    impl.async = true;
                } else if (strcmp(atom, "false") == 0) {
                    impl.async = false;
                } else {
                    return FALSE;
                }
            } else {
                return FALSE;
            }
            break;
        default:
            return FALSE;
        }
        items--;
    }

    ng = ei_decode_list_header(buf, &index, &items);
    if (ng) return FALSE;
    ng = (items != 0);
    if (ng) return FALSE;

    return true;
}

bool
lets_parse_read_options(lets_impl& impl,
                        const char* buf, ErlDrvSizeT len)
{
    (void) len;

    int index, ng, items, arity, term_type, size_needed;
    char atom[MAXATOMLEN];

    index = 0;
    ng = ei_decode_list_header(buf, &index, &items);
    if (ng) return FALSE;
    ng = (items < 0);
    if (ng) return FALSE;

    if (!items) return true;

    while (items) {
        ng = ei_get_type(buf, &index, &term_type, &size_needed);
        if (ng) return FALSE;

        switch (term_type) {
        case ERL_ATOM_EXT:
            ng = ei_decode_atom(buf, &index, atom);
            if (ng) return FALSE;
            if (strcmp(atom, "verify_checksums") == 0) {
                impl.db_read_options.verify_checksums = true;
            } else if (strcmp(atom, "fill_cache") == 0) {
                impl.db_read_options.fill_cache = true;
            } else {
                return FALSE;
            }
            break;
        case ERL_SMALL_TUPLE_EXT:
            ng = ei_decode_tuple_header(buf, &index, &arity);
            if (ng) return FALSE;
            ng = (arity != 2);
            if (ng) return FALSE;
            ng = ei_decode_atom(buf, &index, atom);
            if (ng) return FALSE;
            if (strcmp(atom, "verify_checksums") == 0) {
                ng = ei_decode_atom(buf, &index, atom);
                if (ng) return FALSE;
                if (strcmp(atom, "true") == 0) {
                    impl.db_read_options.verify_checksums = true;
                } else if (strcmp(atom, "false") == 0) {
                    impl.db_read_options.verify_checksums = false;
                } else {
                    return FALSE;
                }
            } else if (strcmp(atom, "fill_cache") == 0) {
                ng = ei_decode_atom(buf, &index, atom);
                if (ng) return FALSE;
                if (strcmp(atom, "true") == 0) {
                    impl.db_read_options.fill_cache = true;
                } else if (strcmp(atom, "false") == 0) {
                    impl.db_read_options.fill_cache = false;
                } else {
                    return FALSE;
                }
            } else {
                return FALSE;
            }
            break;
        default:
            return FALSE;
        }
        items--;
    }

    ng = ei_decode_list_header(buf, &index, &items);
    if (ng) return FALSE;
    ng = (items != 0);
    if (ng) return FALSE;

    return true;
}

bool
lets_parse_write_options(lets_impl& impl,
                         const char* buf, ErlDrvSizeT len)
{
    (void) len;

    int index, ng, items, arity, term_type, size_needed;
    char atom[MAXATOMLEN];

    index = 0;
    ng = ei_decode_list_header(buf, &index, &items);
    if (ng) return FALSE;
    ng = (items < 0);
    if (ng) return FALSE;

    if (!items) return true;

    while (items) {
        ng = ei_get_type(buf, &index, &term_type, &size_needed);
        if (ng) return FALSE;

        switch (term_type) {
        case ERL_ATOM_EXT:
            ng = ei_decode_atom(buf, &index, atom);
            if (ng) return FALSE;
            if (strcmp(atom, "sync") == 0) {
                impl.db_write_options.sync = true;
            } else {
                return FALSE;
            }
            break;
        case ERL_SMALL_TUPLE_EXT:
            ng = ei_decode_tuple_header(buf, &index, &arity);
            if (ng) return FALSE;
            ng = (arity != 2);
            if (ng) return FALSE;
            ng = ei_decode_atom(buf, &index, atom);
            if (ng) return FALSE;
            if (strcmp(atom, "sync") == 0) {
                ng = ei_decode_atom(buf, &index, atom);
                if (ng) return FALSE;
                if (strcmp(atom, "true") == 0) {
                    impl.db_write_options.sync = true;
                } else if (strcmp(atom, "false") == 0) {
                    impl.db_write_options.sync = false;
                } else {
                    return FALSE;
                }
            } else {
                return FALSE;
            }
            break;
        default:
            return FALSE;
        }
        items--;
    }

    ng = ei_decode_list_header(buf, &index, &items);
    if (ng) return FALSE;
    ng = (items != 0);
    if (ng) return FALSE;

    return true;
}

//
// KEEP AS PLACEHOLDER UNTIL PATCHES ARE ACCEPTED BY Erlang/OTP TEAM
//

/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1998-2010. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 *

 */

#define get8(s)                                 \
    ((s) += 1,                                  \
     ((unsigned char *)(s))[-1] & 0xff)

#define get16be(s)                              \
    ((s) += 2,                                  \
     (((((unsigned char *)(s))[-2] << 8) |      \
       ((unsigned char *)(s))[-1])) & 0xffff)

#define get32be(s)                              \
    ((s) += 4,                                  \
     ((((unsigned char *)(s))[-4] << 24) |      \
      (((unsigned char *)(s))[-3] << 16) |      \
      (((unsigned char *)(s))[-2] << 8) |       \
      ((unsigned char *)(s))[-1]))

// This function inspects an atom from the binary format.  The p
// parameter is the name of the atom and the name should be
// zero-terminated.  If the name is equal to the atom in binary
// format, returns 0.  Otherwise, return -1.  If name is NULL, no
// comparison is done and returns 0.

int
ei_inspect_atom(const char *buf, int *index, char *p)
{
    const char *s = buf + *index;
    const char *s0 = s;
    int len;

    if (get8(s) != ERL_ATOM_EXT) return -1;

    len = get16be(s);

    if (len > MAXATOMLEN) return -1;

    if (p) {
        if (len != (int) strlen(p)) return -1;
        if (memcmp(p, s, len)) return -1;
    }
    s += len;
    *index += s-s0;

    return 0;
}

// This function inspects a binary from the binary format. The p
// parameter is set to the address of the binary.  The len parameter
// is set to the actual size of the binary.

int
ei_inspect_binary(const char *buf, int *index, void **p, long *lenp)
{
    const char *s = buf + *index;
    const char *s0 = s;
    long len;

    if (get8(s) != ERL_BINARY_EXT) return -1;

    len = get32be(s);
    if (p) *p = (void*) s;
    s += len;

    if (lenp) *lenp = len;
    *index += s-s0;

    return 0;
}
