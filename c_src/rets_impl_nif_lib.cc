// The MIT License
//
// Copyright (C) 2011-2016 by Joseph Wayne Norton <norton@alum.mit.edu>
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

#include "rets_impl_nif_lib.h"


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

ERL_NIF_TERM lets_atom_undefined = 0;
ERL_NIF_TERM lets_atom_true = 0;
ERL_NIF_TERM lets_atom_false = 0;
ERL_NIF_TERM lets_atom_set = 0;
ERL_NIF_TERM lets_atom_ordered_set = 0;
ERL_NIF_TERM lets_atom_private = 0;
ERL_NIF_TERM lets_atom_protected = 0;
ERL_NIF_TERM lets_atom_public = 0;
ERL_NIF_TERM lets_atom_create_if_missing = 0;
ERL_NIF_TERM lets_atom_error_if_exists = 0;
ERL_NIF_TERM lets_atom_paranoid_checks = 0;
ERL_NIF_TERM lets_atom_write_buffer_size = 0;
ERL_NIF_TERM lets_atom_max_open_files = 0;
ERL_NIF_TERM lets_atom_block_cache_size = 0;
ERL_NIF_TERM lets_atom_block_size = 0;
ERL_NIF_TERM lets_atom_block_restart_interval = 0;
ERL_NIF_TERM lets_atom_compression = 0;
ERL_NIF_TERM lets_atom_no = 0;
ERL_NIF_TERM lets_atom_snappy = 0;
ERL_NIF_TERM lets_atom_filter_policy = 0;
ERL_NIF_TERM lets_atom_bloom = 0;
ERL_NIF_TERM lets_atom_async = 0;
ERL_NIF_TERM lets_atom_verify_checksums = 0;
ERL_NIF_TERM lets_atom_fill_cache = 0;
ERL_NIF_TERM lets_atom_sync = 0;
ERL_NIF_TERM lets_atom_end_of_table = 0;
ERL_NIF_TERM lets_atom_when_destroyed = 0;

bool
lets_impl_nif_lib_init(ErlNifEnv* env)
{
    lets_atom_undefined = enif_make_atom(env, "undefined");
    lets_atom_true = enif_make_atom(env, "true");
    lets_atom_false = enif_make_atom(env, "false");
    lets_atom_set = enif_make_atom(env, "set");
    lets_atom_ordered_set = enif_make_atom(env, "ordered_set");
    lets_atom_private = enif_make_atom(env, "private");
    lets_atom_protected = enif_make_atom(env, "protected");
    lets_atom_public = enif_make_atom(env, "public");
    lets_atom_create_if_missing = enif_make_atom(env, "create_if_missing");
    lets_atom_error_if_exists = enif_make_atom(env, "error_if_exists");
    lets_atom_paranoid_checks = enif_make_atom(env, "paranoid_checks");
    lets_atom_write_buffer_size = enif_make_atom(env, "write_buffer_size");
    lets_atom_max_open_files = enif_make_atom(env, "max_open_files");
    lets_atom_block_cache_size = enif_make_atom(env, "block_cache_size");
    lets_atom_block_size = enif_make_atom(env, "block_size");
    lets_atom_block_restart_interval = enif_make_atom(env, "block_restart_interval");
    lets_atom_compression = enif_make_atom(env, "compression");
    lets_atom_no = enif_make_atom(env, "no");
    lets_atom_snappy = enif_make_atom(env, "snappy");
    lets_atom_filter_policy = enif_make_atom(env, "filter_policy");
    lets_atom_bloom = enif_make_atom(env, "bloom");
    lets_atom_async = enif_make_atom(env, "async");
    lets_atom_verify_checksums = enif_make_atom(env, "verify_checksums");
    lets_atom_fill_cache = enif_make_atom(env, "fill_cache");
    lets_atom_sync = enif_make_atom(env, "sync");
    lets_atom_end_of_table = enif_make_atom(env, "$end_of_table");
    lets_atom_when_destroyed = enif_make_atom(env, "when_destroyed");

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

    impl.db_options = rocksdb::Options();
    impl.db_read_options = rocksdb::ReadOptions();
    impl.db_write_options = rocksdb::WriteOptions();
#ifdef ROCKSDB
    impl.db_table_options = rocksdb::BlockBasedTableOptions();
#endif

    return true;
}

bool
lets_create(lets_impl& impl,
            const char op)
{
    rocksdb::Status status;

    // db
    switch (op) {
    case OPEN:
        status = rocksdb::DB::Open(impl.db_options, impl.name->c_str(), &(impl.db));
        if (!status.ok()) {
            return FALSE;
        } else {
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
    default:
        return FALSE;
    }

    return true;
}

bool
lets_parse_options(ErlNifEnv* env, lets_impl& impl,
                   ERL_NIF_TERM& options, const ERL_NIF_TERM& options_len)
{
    (void) options_len;

    ERL_NIF_TERM head, tail;
    int arity;
    const ERL_NIF_TERM* tuple;

    while (enif_get_list_cell(env, options, &head, &tail)) {
        if (enif_is_identical(head, lets_atom_create_if_missing)) {
            impl.db_options.create_if_missing = true;
        } else if (enif_is_identical(head, lets_atom_error_if_exists)) {
            impl.db_options.error_if_exists = true;
        } else if (enif_is_identical(head, lets_atom_paranoid_checks)) {
            impl.db_options.paranoid_checks = true;
        } else if (enif_get_tuple(env, head, &arity, &tuple) && arity == 2) {
            unsigned int val;
            if (enif_is_identical(tuple[0], lets_atom_create_if_missing)) {
                if (enif_is_identical(tuple[1], lets_atom_true)) {
                    impl.db_options.create_if_missing = true;
                } else if (enif_is_identical(tuple[1], lets_atom_false)) {
                    impl.db_options.create_if_missing = false;
                } else {
                    return FALSE;
                }
            } else if (enif_is_identical(tuple[0], lets_atom_error_if_exists)) {
                if (enif_is_identical(tuple[1], lets_atom_true)) {
                    impl.db_options.error_if_exists = true;
                } else if (enif_is_identical(tuple[1], lets_atom_false)) {
                    impl.db_options.error_if_exists = false;
                } else {
                    return FALSE;
                }
            } else if (enif_is_identical(tuple[0], lets_atom_paranoid_checks)) {
                impl.db_options.paranoid_checks = true;
                if (enif_is_identical(tuple[1], lets_atom_true)) {
                    impl.db_options.paranoid_checks = true;
                } else if (enif_is_identical(tuple[1], lets_atom_false)) {
                    impl.db_options.paranoid_checks = false;
                } else {
                    return FALSE;
                }
            } else if (enif_is_identical(tuple[0], lets_atom_write_buffer_size) &&
                       enif_get_uint(env, tuple[1], &val)) {
                impl.db_options.write_buffer_size = val;
            } else if (enif_is_identical(tuple[0], lets_atom_max_open_files) &&
                       enif_get_uint(env, tuple[1], &val)) {
                impl.db_options.max_open_files = val;
            } else if (enif_is_identical(tuple[0], lets_atom_block_cache_size) &&
                       enif_get_uint(env, tuple[1], &val)) {
#ifndef ROCKSDB
                impl.db_block_cache_size = val;
                delete impl.db_block_cache;
                impl.db_block_cache = rocksdb::NewLRUCache(impl.db_block_cache_size);
                impl.db_options.block_cache = impl.db_block_cache;
                if (!impl.db_options.block_cache) {
                    return FALSE;
                }
#endif
            } else if (enif_is_identical(tuple[0], lets_atom_block_size) &&
                       enif_get_uint(env, tuple[1], &val)) {
#ifndef ROCKSDB
                impl.db_options.block_size = val;
#endif
            } else if (enif_is_identical(tuple[0], lets_atom_block_restart_interval) &&
                       enif_get_uint(env, tuple[1], &val)) {
#ifndef ROCKSDB
                impl.db_options.block_restart_interval = val;
#endif
            } else if (enif_is_identical(tuple[0], lets_atom_compression)) {
                if (enif_is_identical(tuple[1], lets_atom_no)) {
                    impl.db_options.compression = rocksdb::kNoCompression;
                } else if (enif_is_identical(tuple[1], lets_atom_snappy)) {
                    impl.db_options.compression = rocksdb::kSnappyCompression;
                } else {
                    return FALSE;
                }
            } else if (enif_is_identical(tuple[0], lets_atom_filter_policy)) {
                if (enif_is_identical(tuple[1], lets_atom_no)) {
                    delete impl.db_filter_policy;
                    impl.db_filter_policy = NULL;
#ifndef ROCKSDB
                    impl.db_options.filter_policy = NULL;
#endif
                } else if (enif_get_tuple(env, tuple[1], &arity, &tuple) && arity == 2) {
                    if (enif_is_identical(tuple[0], lets_atom_bloom) &&
                        enif_get_uint(env, tuple[1], &val)) {
#ifndef ROCKSDB
                        impl.db_filter_policy_bloom_bits_per_key = val;
                        delete impl.db_filter_policy;
                        impl.db_filter_policy = rocksdb::NewBloomFilterPolicy(impl.db_filter_policy_bloom_bits_per_key);
                        impl.db_options.filter_policy = impl.db_filter_policy;
                        if (!impl.db_options.filter_policy) {
                            return FALSE;
                        }
#endif
                    }
                } else {
                    return FALSE;
                }
            } else if (enif_is_identical(tuple[0], lets_atom_async)) {
                if (enif_is_identical(tuple[1], lets_atom_true)) {
                    impl.async = true;
                } else if (enif_is_identical(tuple[1], lets_atom_false)) {
                    impl.async = false;
                } else {
                    return FALSE;
                }
            } else {
                return FALSE;
            }
        } else {
            return FALSE;
        }
        options = tail;
    }

    return true;
}

bool
lets_parse_read_options(ErlNifEnv* env, rocksdb::ReadOptions& opts,
                        ERL_NIF_TERM& options, const ERL_NIF_TERM& options_len)
{
    (void) options_len;

    ERL_NIF_TERM head, tail;
    int arity;
    const ERL_NIF_TERM* tuple;

    // TODO: snapshot
    while (enif_get_list_cell(env, options, &head, &tail)) {
        if (enif_is_identical(head, lets_atom_verify_checksums)) {
            opts.verify_checksums = true;
        } else if (enif_is_identical(head, lets_atom_fill_cache)) {
            opts.fill_cache = true;
        } else if (enif_get_tuple(env, head, &arity, &tuple) && arity == 2) {
            if (enif_is_identical(tuple[0], lets_atom_verify_checksums)) {
                if (enif_is_identical(tuple[1], lets_atom_true)) {
                    opts.verify_checksums = true;
                } else if (enif_is_identical(tuple[1], lets_atom_false)) {
                    opts.verify_checksums = false;
                } else {
                    return FALSE;
                }
            } else if (enif_is_identical(tuple[0], lets_atom_fill_cache)) {
                if (enif_is_identical(tuple[1], lets_atom_true)) {
                    opts.fill_cache = true;
                } else if (enif_is_identical(tuple[1], lets_atom_false)) {
                    opts.fill_cache = false;
                } else {
                    return FALSE;
                }
            } else {
                return FALSE;
            }
        } else {
            return FALSE;
        }
        options = tail;
    }

    return true;
}

bool
lets_parse_write_options(ErlNifEnv* env, rocksdb::WriteOptions& opts,
                         ERL_NIF_TERM& options, const ERL_NIF_TERM& options_len)
{
    (void) options_len;

    ERL_NIF_TERM head, tail;
    int arity;
    const ERL_NIF_TERM* tuple;

    // TODO: snapshot
    while (enif_get_list_cell(env, options, &head, &tail)) {
        if (enif_is_identical(head, lets_atom_sync)) {
            opts.sync = true;
        } else if (enif_get_tuple(env, head, &arity, &tuple) && arity == 2) {
            if (enif_is_identical(tuple[0], lets_atom_sync)) {
                if (enif_is_identical(tuple[1], lets_atom_true)) {
                    opts.sync = true;
                } else if (enif_is_identical(tuple[1], lets_atom_false)) {
                    opts.sync = false;
                } else {
                    return FALSE;
                }
            } else {
                return FALSE;
            }
        } else {
            return FALSE;
        }
        options = tail;
    }

    return true;
}
