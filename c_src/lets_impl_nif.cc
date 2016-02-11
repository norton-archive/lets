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

#include "lets_impl_nif.h"
#include "lets_impl_nif_lib.h"

#include <stdio.h>

static inline ERL_NIF_TERM
make_badarg(ErlNifEnv* env, const leveldb::Status& status, unsigned line=0) {
    if (line) {
        fprintf(stderr, "MAKEBADARG %s:%d %s\n", __FILE__, line, status.ToString().c_str());
    }
    return enif_make_badarg(env);
}

#if 0
#define MAKEBADARG(env, status) make_badarg(env, status, __LINE__)
#else
#define MAKEBADARG(env, status) make_badarg(env, status)
#endif

// NifHandle
typedef struct
{
    lets_impl impl;
} lets_impl_nif_handle;

static ErlNifResourceType* lets_impl_nif_RESOURCE = NULL;
static unsigned lets_impl_nif_RESOURCE_SIZE = sizeof(lets_impl_nif_handle);
static ErlNifFunc nif_funcs[] =
    {
        {"impl_open", 6, lets_impl_nif_open6},
        {"impl_destroy", 6, lets_impl_nif_destroy6},
        {"impl_repair", 6, lets_impl_nif_repair6},
        {"impl_insert", 3, lets_impl_nif_insert3},
        {"impl_insert", 4, lets_impl_nif_insert4},
        {"impl_insert_new", 3, lets_impl_nif_insert_new3},
        {"impl_insert_new", 4, lets_impl_nif_insert_new4},
        {"impl_delete", 2, lets_impl_nif_delete2},
        {"impl_delete", 3, lets_impl_nif_delete3},
        {"impl_delete_all_objects", 2, lets_impl_nif_delete_all_objects2},
        {"impl_lookup", 3, lets_impl_nif_lookup3},
        {"impl_member", 3, lets_impl_nif_member3},
        {"impl_first", 3, lets_impl_nif_first3},
        {"impl_first_iter", 3, lets_impl_nif_first_iter3},
        {"impl_last", 3, lets_impl_nif_last3},
        {"impl_last_iter", 3, lets_impl_nif_last_iter3},
        {"impl_next", 4, lets_impl_nif_next4},
        {"impl_next_iter", 4, lets_impl_nif_next_iter4},
        {"impl_prev", 4, lets_impl_nif_prev4},
        {"impl_prev_iter", 4, lets_impl_nif_prev_iter4},
        {"impl_info_memory", 1, lets_impl_nif_info_memory1},
        {"impl_info_size", 1, lets_impl_nif_info_size1},
        {"impl_notify", 4, lets_impl_nif_notify4},
    };

static void
lets_impl_nif_resource_dtor(ErlNifEnv* env, void* arg)
{
    (void) env;
    lets_impl_nif_handle* h = (lets_impl_nif_handle*) arg;

    // alive
    h->impl.alive = 0;

    // db
    delete h->impl.db;

    // db_block_cache
    delete h->impl.db_block_cache;

    // db_filter_policy
    delete h->impl.db_filter_policy;

    // name
    delete h->impl.name;

    // notify_when_destroyed
    while (h->impl.notify_when_destroyed.size()) {
        ErlNifPid pid = h->impl.notify_when_destroyed.back().first;
        ErlNifEnv* msgenv = h->impl.notify_when_destroyed.back().second.first;
        ERL_NIF_TERM msg = h->impl.notify_when_destroyed.back().second.second;

        if (!enif_send(NULL, &pid, msgenv, msg)) {
            // TODO halt?
        }
        enif_free_env(msgenv);

        h->impl.notify_when_destroyed.pop_back();
    }
}

static int
on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    (void) priv_data;
    (void) load_info;

    if (!lets_impl_nif_lib_init(env)) {
        return -1;
    }

    ErlNifResourceFlags flags = (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
    lets_impl_nif_RESOURCE = enif_open_resource_type(env, NULL,
                                                     "lets_impl_nif_resource",
                                                     &lets_impl_nif_resource_dtor,
                                                     flags, NULL);
    if (lets_impl_nif_RESOURCE == NULL) {
        return -1;
    }

    return 0;
}

ERL_NIF_INIT(lets_impl_nif, nif_funcs, &on_load, NULL, NULL, NULL);

static bool
get_read_options(leveldb::ReadOptions defaults, ErlNifEnv* env, const ERL_NIF_TERM arg, leveldb::ReadOptions& opts)
{
    ERL_NIF_TERM read_options;
    unsigned read_options_len;

    if (enif_is_identical(arg, lets_atom_undefined)) {
        opts = defaults;
        return true;
    }

    if (!enif_get_list_length(env, arg, &read_options_len)) {
        return false;
    }
    read_options = arg;

    if (!lets_parse_read_options(env, opts, read_options, read_options_len)) {
        return false;
    }

    return true;
}

static bool
get_write_options(leveldb::WriteOptions defaults, ErlNifEnv* env, const ERL_NIF_TERM arg, leveldb::WriteOptions& opts)
{
    ERL_NIF_TERM write_options;
    unsigned write_options_len;

    if (enif_is_identical(arg, lets_atom_undefined)) {
        opts = defaults;
        return true;
    }

    if (!enif_get_list_length(env, arg, &write_options_len)) {
        return false;
    }
    write_options = arg;

    if (!lets_parse_write_options(env, opts, write_options, write_options_len)) {
        return false;
    }

    return true;
}

static bool
get_n_option(ErlNifEnv* env, const ERL_NIF_TERM arg, long* n)
{
    if (enif_is_identical(arg, lets_atom_undefined)) {
        *n = -1;
        return true;
    }

    if (!enif_get_long(env, arg, n)) {
        return false;
    }
    if (*n < 0) {
        return false;
    }

    return true;
}

static ERL_NIF_TERM
lets_get_next(ErlNifEnv* env, long n, leveldb::Iterator* it)
{
    ErlNifBinary binary;
    size_t offset = 0;
    int length = 0;
    std::vector<size_t> sizes;
    ERL_NIF_TERM main;
    std::vector<ERL_NIF_TERM> binaries;

    assert(n >= 0);
    assert(it->Valid());

    if (!enif_alloc_binary(it->key().size(), &binary)) {
        return 0;
    }

    sizes.reserve(n);
    while(n > 0 && it->Valid()) {
        size_t len = it->key().size();

        if (!enif_realloc_binary(&binary, offset+len)) {
            enif_release_binary(&binary);
            return 0;
        }

        memcpy(binary.data+offset, it->key().data(), len);
        sizes.push_back(len); offset += len; n--;
        it->Next();
    }

    main = enif_make_binary(env, &binary);
    length = sizes.size();
    binaries.reserve(length);
    offset = n = 0;
    while (n < length) {
        size_t len = sizes[n];
        ERL_NIF_TERM sub = enif_make_sub_binary(env, main, offset, len);
        binaries.push_back(sub);
        offset += len; n++;
    }

    return enif_make_list_from_array(env, binaries.data(), binaries.size());
}

static ERL_NIF_TERM
lets_get_next_iter(ErlNifEnv* env, long n, leveldb::Iterator* it)
{
   ErlNifBinary binary;
    size_t offset = 0;
    int length = 0;
    std::vector<size_t> sizes;
    ERL_NIF_TERM main;
    std::vector<ERL_NIF_TERM> binaries;

    assert(n >= 0);
    assert(it->Valid());

    if (!enif_alloc_binary(it->value().size(), &binary)) {
        return 0;
    }

    sizes.reserve(n);
    while(n > 0 && it->Valid()) {
        size_t len = it->value().size();

        if (!enif_realloc_binary(&binary, offset+len)) {
            enif_release_binary(&binary);
            return 0;
        }

        memcpy(binary.data+offset, it->value().data(), len);
        sizes.push_back(len); offset += len; n--;
        it->Next();
    }

    main = enif_make_binary(env, &binary);
    length = sizes.size();
    binaries.reserve(length);
    offset = n = 0;
    while (n < length) {
        size_t len = sizes[n];
        ERL_NIF_TERM sub = enif_make_sub_binary(env, main, offset, len);
        binaries.push_back(sub);
        offset += len; n++;
    }

    return enif_make_list_from_array(env, binaries.data(), binaries.size());
}

static ERL_NIF_TERM
lets_get_prev(ErlNifEnv* env, long n, leveldb::Iterator* it)
{
    ErlNifBinary binary;
    size_t offset = 0;
    int length = 0;
    std::vector<size_t> sizes;
    ERL_NIF_TERM main;
    std::vector<ERL_NIF_TERM> binaries;

    assert(n >= 0);
    assert(it->Valid());

    if (!enif_alloc_binary(it->key().size(), &binary)) {
        return 0;
    }

    sizes.reserve(n);
    while(n > 0 && it->Valid()) {
        size_t len = it->key().size();

        if (!enif_realloc_binary(&binary, offset+len)) {
            enif_release_binary(&binary);
            return 0;
        }

        memcpy(binary.data+offset, it->key().data(), len);
        sizes.push_back(len); offset += len; n--;
        it->Prev();
    }

    main = enif_make_binary(env, &binary);
    length = sizes.size();
    binaries.reserve(length);
    offset = n = 0;
    while (n < length) {
        size_t len = sizes[n];
        ERL_NIF_TERM sub = enif_make_sub_binary(env, main, offset, len);
        binaries.push_back(sub);
        offset += len; n++;
    }

    return enif_make_list_from_array(env, binaries.data(), binaries.size());
}

static ERL_NIF_TERM
lets_get_prev_iter(ErlNifEnv* env, long n, leveldb::Iterator* it)
{
   ErlNifBinary binary;
    size_t offset = 0;
    int length = 0;
    std::vector<size_t> sizes;
    ERL_NIF_TERM main;
    std::vector<ERL_NIF_TERM> binaries;

    assert(n >= 0);
    assert(it->Valid());

    if (!enif_alloc_binary(it->value().size(), &binary)) {
        return 0;
    }

    sizes.reserve(n);
    while(n > 0 && it->Valid()) {
        size_t len = it->value().size();

        if (!enif_realloc_binary(&binary, offset+len)) {
            enif_release_binary(&binary);
            return 0;
        }

        memcpy(binary.data+offset, it->value().data(), len);
        sizes.push_back(len); offset += len; n--;
        it->Prev();
    }

    main = enif_make_binary(env, &binary);
    length = sizes.size();
    binaries.reserve(length);
    offset = n = 0;
    while (n < length) {
        size_t len = sizes[n];
        ERL_NIF_TERM sub = enif_make_sub_binary(env, main, offset, len);
        binaries.push_back(sub);
        offset += len; n++;
    }

    return enif_make_list_from_array(env, binaries.data(), binaries.size());
}

static ERL_NIF_TERM
db_create6(const char op, ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[], lets_impl_nif_handle** reth)
{
    (void) argc;

    leveldb::Status status;
    char type;
    char privacy;
    ErlNifBinary path;
    ERL_NIF_TERM options;
    unsigned options_len;
    ERL_NIF_TERM read_options;
    unsigned read_options_len;
    ERL_NIF_TERM write_options;
    unsigned write_options_len;

    if (enif_is_identical(argv[0], lets_atom_set)) {
        type = SET;
    } else if (enif_is_identical(argv[0], lets_atom_ordered_set)) {
        type = ORDERED_SET;
    } else {
        return MAKEBADARG(env, status);
    }

    if (enif_is_identical(argv[1], lets_atom_private)) {
        privacy = PRIVATE;
    } else if (enif_is_identical(argv[1], lets_atom_protected)) {
        privacy = PROTECTED;
    } else if (enif_is_identical(argv[1], lets_atom_public)) {
        privacy = PUBLIC;
    } else {
        return MAKEBADARG(env, status);
    }

    if (!enif_inspect_binary(env, argv[2], &path) || !path.size) {
        return MAKEBADARG(env, status);
    }

    if (!enif_get_list_length(env, argv[3], &options_len)) {
        return MAKEBADARG(env, status);
    }
    options = argv[3];

    if (!enif_get_list_length(env, argv[4], &read_options_len)) {
        return MAKEBADARG(env, status);
    }
    read_options = argv[4];

    if (!enif_get_list_length(env, argv[5], &write_options_len)) {
        return MAKEBADARG(env, status);
    }
    write_options = argv[5];

    lets_impl_nif_handle* h = (lets_impl_nif_handle*) enif_alloc_resource(lets_impl_nif_RESOURCE, lets_impl_nif_RESOURCE_SIZE);
    if (!h) {
        return MAKEBADARG(env, status);
    }
    memset(h, 0, lets_impl_nif_RESOURCE_SIZE);

    if (!lets_init(h->impl, type, privacy, (const char*) path.data, path.size)) {
        enif_release_resource(h);
        return MAKEBADARG(env, status);
    }

    if (!lets_parse_options(env, h->impl, options, options_len)) {
        enif_release_resource(h);
        return MAKEBADARG(env, status);
    }
    if (!lets_parse_read_options(env, h->impl.db_read_options, read_options, read_options_len)) {
        enif_release_resource(h);
        return MAKEBADARG(env, status);
    }
    if (!lets_parse_write_options(env, h->impl.db_write_options, write_options, write_options_len)) {
        enif_release_resource(h);
        return MAKEBADARG(env, status);
    }

    if (!lets_create(h->impl, op)) {
        enif_release_resource(h);
        return MAKEBADARG(env, status);
    }

    *reth = h;
    return 0;
}

ERL_NIF_TERM
lets_impl_nif_open6(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    lets_impl_nif_handle* h = NULL;
    ERL_NIF_TERM badarg = db_create6(OPEN, env, argc, argv, &h);

    if (!h) {
        return badarg;
    }

    ERL_NIF_TERM ret = enif_make_resource(env, h);
    enif_release_resource(h);
    return ret;
}

ERL_NIF_TERM
lets_impl_nif_destroy6(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    lets_impl_nif_handle* h = NULL;
    ERL_NIF_TERM badarg = db_create6(DESTROY, env, argc, argv, &h);

    if (!h) {
        return badarg;
    }

    enif_release_resource(h);
    return lets_atom_true;
}

ERL_NIF_TERM
lets_impl_nif_repair6(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    lets_impl_nif_handle* h = NULL;
    ERL_NIF_TERM badarg = db_create6(REPAIR, env, argc, argv, &h);

    if (!h) {
        return badarg;
    }

    enif_release_resource(h);
    return lets_atom_true;
}

ERL_NIF_TERM
lets_impl_nif_insert3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    leveldb::WriteOptions write_options;
    ERL_NIF_TERM list;
    unsigned list_len;
    ERL_NIF_TERM head, tail;
    int arity;
    const ERL_NIF_TERM* tuple;
    ErlNifBinary key;
    ErlNifBinary blob;
    leveldb::WriteBatch batch;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_write_options(h->impl.db_write_options, env, argv[1], write_options)) {
        return MAKEBADARG(env, status);
    }
    if (!enif_get_list_length(env, argv[2], &list_len)) {
        return MAKEBADARG(env, status);
    }
    list = argv[2];

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    while (enif_get_list_cell(env, list, &head, &tail)) {
        if (enif_get_tuple(env, head, &arity, &tuple) && arity == 2) {
            if (!enif_inspect_binary(env, tuple[0], &key)) {
                return MAKEBADARG(env, status);
            }
            if (!enif_inspect_binary(env, tuple[1], &blob)) {
                return MAKEBADARG(env, status);
            }

            leveldb::Slice skey((const char*) key.data, key.size);
            leveldb::Slice sblob((const char*) blob.data, blob.size);
            batch.Put(skey, sblob);
        } else {
            return MAKEBADARG(env, status);
        }
        list = tail;
    }

    status = h->impl.db->Write(write_options, &batch);
    if (!status.ok()) {
        return MAKEBADARG(env, status);
    }

    return lets_atom_true;
}

ERL_NIF_TERM
lets_impl_nif_insert4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    leveldb::WriteOptions write_options;
    ErlNifBinary key;
    ErlNifBinary blob;
    leveldb::WriteBatch batch;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_write_options(h->impl.db_write_options, env, argv[1], write_options)) {
        return MAKEBADARG(env, status);
    }
    if (!enif_inspect_binary(env, argv[2], &key)) {
        return MAKEBADARG(env, status);
    }
    if (!enif_inspect_binary(env, argv[3], &blob)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    leveldb::Slice skey((const char*) key.data, key.size);
    leveldb::Slice sblob((const char*) blob.data, blob.size);
    batch.Put(skey, sblob);

    status = h->impl.db->Write(write_options, &batch);
    if (!status.ok()) {
        return MAKEBADARG(env, status);
    }

    return lets_atom_true;
}

ERL_NIF_TERM
lets_impl_nif_insert_new3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;

    leveldb::Status status;
    // @TODO not supported by leveldb
    return MAKEBADARG(env, status);
}

ERL_NIF_TERM
lets_impl_nif_insert_new4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;

    leveldb::Status status;
    // @TODO not supported by leveldb
    return MAKEBADARG(env, status);
}

ERL_NIF_TERM
lets_impl_nif_delete2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    leveldb::WriteOptions write_options;
    leveldb::WriteBatch batch;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_write_options(h->impl.db_write_options, env, argv[1], write_options)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    // force sync
    write_options.sync = true;

    // alive
    h->impl.alive = 0;

    status = h->impl.db->Write(write_options, &batch);
    if (!status.ok()) {
        return MAKEBADARG(env, status);
    }

    // @TBD This is quite risky ... need to re-consider.
    // delete h->impl.db;
    // h->impl.db = NULL;

    return lets_atom_true;
}

ERL_NIF_TERM
lets_impl_nif_delete3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    leveldb::WriteOptions write_options;
    ErlNifBinary key;
    leveldb::WriteBatch batch;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_write_options(h->impl.db_write_options, env, argv[1], write_options)) {
        return MAKEBADARG(env, status);
    }
    if (!enif_inspect_binary(env, argv[2], &key)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    leveldb::Slice skey((const char*) key.data, key.size);
    batch.Delete(skey);

    status = h->impl.db->Write(write_options, &batch);
    if (!status.ok()) {
        return MAKEBADARG(env, status);
    }

    return lets_atom_true;
}

ERL_NIF_TERM
lets_impl_nif_delete_all_objects2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;
    (void) argv;

    leveldb::Status status;
    // @TODO not supported by leveldb
    return MAKEBADARG(env, status);
}

ERL_NIF_TERM
lets_impl_nif_lookup3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    leveldb::ReadOptions read_options;
    ErlNifBinary key;
    ERL_NIF_TERM ret = 0;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_read_options(h->impl.db_read_options, env, argv[1], read_options)) {
        return MAKEBADARG(env, status);
    }
    if (!enif_inspect_binary(env, argv[2], &key)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    leveldb::Iterator* it = h->impl.db->NewIterator(read_options);
    if (!it) {
        return MAKEBADARG(env, status);
    }

    leveldb::Slice skey((const char*) key.data, key.size);
    it->Seek(skey);
    if (!it->Valid() || it->key().compare(skey) != 0) {
        delete it;
        return lets_atom_end_of_table;
    }

    size_t size = it->value().size();
    unsigned char* b = enif_make_new_binary(env, size, &ret);
    if (!b) {
        delete it;
        return MAKEBADARG(env, status);
    }

    memcpy(b, it->value().data(), size);
    delete it;
    return ret;
}

ERL_NIF_TERM
lets_impl_nif_member3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    leveldb::ReadOptions read_options;
    ErlNifBinary key;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_read_options(h->impl.db_read_options, env, argv[1], read_options)) {
        return MAKEBADARG(env, status);
    }
    if (!enif_inspect_binary(env, argv[2], &key)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    leveldb::Iterator* it = h->impl.db->NewIterator(read_options);
    if (!it) {
        return MAKEBADARG(env, status);
    }

    leveldb::Slice skey((const char*) key.data, key.size);
    it->Seek(skey);
    if (!it->Valid() || it->key().compare(skey) != 0) {
        delete it;
        return lets_atom_false;
    }

    delete it;
    return lets_atom_true;
}

ERL_NIF_TERM
lets_impl_nif_first3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    leveldb::ReadOptions read_options;
    long n;
    ERL_NIF_TERM ret = 0;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_read_options(h->impl.db_read_options, env, argv[1], read_options)) {
        return MAKEBADARG(env, status);
    }
    if (!get_n_option(env, argv[2], &n)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    leveldb::Iterator* it = h->impl.db->NewIterator(read_options);
    if (!it) {
        return MAKEBADARG(env, status);
    }

    it->SeekToFirst();
    if (!it->Valid()) {
        delete it;
        return lets_atom_end_of_table;
    }

    if (n < 0) {
        size_t size = it->key().size();
        unsigned char* k = enif_make_new_binary(env, size, &ret);
        if (!k) {
            delete it;
            return MAKEBADARG(env, status);
        }

        memcpy(k, it->key().data(), size);
    } else {
        ret = lets_get_next(env, n, it);
        if (!ret) {
            delete it;
            return MAKEBADARG(env, status);
        }
    }

    delete it;
    return ret;
}

ERL_NIF_TERM
lets_impl_nif_first_iter3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    leveldb::ReadOptions read_options;
    lets_impl_nif_handle* h;
    long n;
    ERL_NIF_TERM ret = 0;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_read_options(h->impl.db_read_options, env, argv[1], read_options)) {
        return MAKEBADARG(env, status);
    }
    if (!get_n_option(env, argv[2], &n)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    leveldb::Iterator* it = h->impl.db->NewIterator(read_options);
    if (!it) {
        return MAKEBADARG(env, status);
    }

    it->SeekToFirst();
    if (!it->Valid()) {
        delete it;
        return lets_atom_end_of_table;
    }

    if (n < 0) {
        size_t size = it->value().size();
        unsigned char* k = enif_make_new_binary(env, size, &ret);
        if (!k) {
            delete it;
            return MAKEBADARG(env, status);
        }

        memcpy(k, it->value().data(), size);
    } else {
        ret = lets_get_next_iter(env, n, it);
        if (!ret) {
            delete it;
            return MAKEBADARG(env, status);
        }
    }

    delete it;
    return ret;
}

ERL_NIF_TERM
lets_impl_nif_last3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    leveldb::ReadOptions read_options;
    long n;
    ERL_NIF_TERM ret = 0;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_read_options(h->impl.db_read_options, env, argv[1], read_options)) {
        return MAKEBADARG(env, status);
    }
    if (!get_n_option(env, argv[2], &n)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    leveldb::Iterator* it = h->impl.db->NewIterator(read_options);
    if (!it) {
        return MAKEBADARG(env, status);
    }

    it->SeekToLast();
    if (!it->Valid()) {
        delete it;
        return lets_atom_end_of_table;
    }

    if (n < 0) {
        size_t size = it->key().size();
        unsigned char* k = enif_make_new_binary(env, size, &ret);
        if (!k) {
            delete it;
            return MAKEBADARG(env, status);
        }

        memcpy(k, it->key().data(), size);
    } else {
        ret = lets_get_prev(env, n, it);
        if (!ret) {
            delete it;
            return MAKEBADARG(env, status);
        }
    }

    delete it;
    return ret;
}

ERL_NIF_TERM
lets_impl_nif_last_iter3(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    leveldb::ReadOptions read_options;
    long n;
    ERL_NIF_TERM ret = 0;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_read_options(h->impl.db_read_options, env, argv[1], read_options)) {
        return MAKEBADARG(env, status);
    }
    if (!get_n_option(env, argv[2], &n)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    leveldb::Iterator* it = h->impl.db->NewIterator(read_options);
    if (!it) {
        return MAKEBADARG(env, status);
    }

    it->SeekToLast();
    if (!it->Valid()) {
        delete it;
        return lets_atom_end_of_table;
    }

    if (n < 0) {
        size_t size = it->value().size();
        unsigned char* k = enif_make_new_binary(env, size, &ret);
        if (!k) {
            delete it;
            return MAKEBADARG(env, status);
        }

        memcpy(k, it->value().data(), size);
    } else {
        ret = lets_get_prev_iter(env, n, it);
        if (!ret) {
            delete it;
            return MAKEBADARG(env, status);
        }
    }

    delete it;
    return ret;
}

ERL_NIF_TERM
lets_impl_nif_next4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    leveldb::ReadOptions read_options;
    ErlNifBinary key;
    long n;
    ERL_NIF_TERM ret = 0;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_read_options(h->impl.db_read_options, env, argv[1], read_options)) {
        return MAKEBADARG(env, status);
    }
    if (!enif_inspect_binary(env, argv[2], &key)) {
        return MAKEBADARG(env, status);
    }
    if (!get_n_option(env, argv[3], &n)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    leveldb::Iterator* it = h->impl.db->NewIterator(read_options);
    if (!it) {
        return MAKEBADARG(env, status);
    }

    leveldb::Slice skey((const char*) key.data, key.size);
    it->Seek(skey);
    if (!it->Valid()) {
        delete it;
        return lets_atom_end_of_table;
    }

    if (it->key().compare(skey) == 0) {
        it->Next();
        if (!it->Valid()) {
            delete it;
            return lets_atom_end_of_table;
        }
    }

    if (n < 0) {
        size_t size = it->key().size();
        unsigned char* k = enif_make_new_binary(env, size, &ret);
        if (!k) {
            delete it;
            return MAKEBADARG(env, status);
        }

        memcpy(k, it->key().data(), size);
    } else {
        ret = lets_get_next(env, n, it);
        if (!ret) {
            delete it;
            return MAKEBADARG(env, status);
        }
    }

    delete it;
    return ret;
}

ERL_NIF_TERM
lets_impl_nif_next_iter4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    leveldb::ReadOptions read_options;
    ErlNifBinary key;
    long n;
    ERL_NIF_TERM ret = 0;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_read_options(h->impl.db_read_options, env, argv[1], read_options)) {
        return MAKEBADARG(env, status);
    }
    if (!enif_inspect_binary(env, argv[2], &key)) {
        return MAKEBADARG(env, status);
    }
    if (!get_n_option(env, argv[3], &n)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    leveldb::Iterator* it = h->impl.db->NewIterator(read_options);
    if (!it) {
        return MAKEBADARG(env, status);
    }

    leveldb::Slice skey((const char*) key.data, key.size);
    it->Seek(skey);
    if (!it->Valid()) {
        delete it;
        return lets_atom_end_of_table;
    }

    if (it->key().compare(skey) == 0) {
        it->Next();
        if (!it->Valid()) {
            delete it;
            return lets_atom_end_of_table;
        }
    }

    if (n < 0) {
        size_t size = it->value().size();
        unsigned char* k = enif_make_new_binary(env, size, &ret);
        if (!k) {
            delete it;
            return MAKEBADARG(env, status);
        }

        memcpy(k, it->value().data(), size);
    } else {
        ret = lets_get_next_iter(env, n, it);
        if (!ret) {
            delete it;
            return MAKEBADARG(env, status);
        }
    }

    delete it;
    return ret;
}

ERL_NIF_TERM
lets_impl_nif_prev4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    leveldb::ReadOptions read_options;
    ErlNifBinary key;
    long n;
    ERL_NIF_TERM ret = 0;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_read_options(h->impl.db_read_options, env, argv[1], read_options)) {
        return MAKEBADARG(env, status);
    }
    if (!enif_inspect_binary(env, argv[2], &key)) {
        return MAKEBADARG(env, status);
    }
    if (!get_n_option(env, argv[3], &n)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    leveldb::Iterator* it = h->impl.db->NewIterator(read_options);
    if (!it) {
        return MAKEBADARG(env, status);
    }

    leveldb::Slice skey((const char*) key.data, key.size);
    it->Seek(skey);
    if (!it->Valid()) {
        it->SeekToLast();
    } else {
        it->Prev();
    }

    if (!it->Valid()) {
        delete it;
        return lets_atom_end_of_table;
    }

    if (n < 0) {
        size_t size = it->key().size();
        unsigned char* k = enif_make_new_binary(env, size, &ret);
        if (!k) {
            delete it;
            return MAKEBADARG(env, status);
        }

        memcpy(k, it->key().data(), size);
    } else {
        ret = lets_get_prev(env, n, it);
        if (!ret) {
            delete it;
            return MAKEBADARG(env, status);
        }
    }

    delete it;
    return ret;
}

ERL_NIF_TERM
lets_impl_nif_prev_iter4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    leveldb::ReadOptions read_options;
    ErlNifBinary key;
    long n;
    ERL_NIF_TERM ret = 0;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!get_read_options(h->impl.db_read_options, env, argv[1], read_options)) {
        return MAKEBADARG(env, status);
    }
    if (!enif_inspect_binary(env, argv[2], &key)) {
        return MAKEBADARG(env, status);
    }
    if (!get_n_option(env, argv[3], &n)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    leveldb::Iterator* it = h->impl.db->NewIterator(read_options);
    if (!it) {
        return MAKEBADARG(env, status);
    }

    leveldb::Slice skey((const char*) key.data, key.size);
    it->Seek(skey);
    if (!it->Valid()) {
        it->SeekToLast();
    } else {
        it->Prev();
    }

    if (!it->Valid()) {
        delete it;
        return lets_atom_end_of_table;
    }

    if (n < 0) {
        size_t size = it->value().size();
        unsigned char* k = enif_make_new_binary(env, size, &ret);
        if (!k) {
            delete it;
            return MAKEBADARG(env, status);
        }

        memcpy(k, it->value().data(), size);
    } else {
        ret = lets_get_prev_iter(env, n, it);
        if (!ret) {
            delete it;
            return MAKEBADARG(env, status);
        }
    }
    delete it;
    return ret;
}

ERL_NIF_TERM
lets_impl_nif_info_memory1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    // ERL_NIF_TERM info;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    // @TODO implementation
    // info = enif_make_uint64(env, h->impl.db_memory);

    // return info;
    return MAKEBADARG(env, status);
}

ERL_NIF_TERM
lets_impl_nif_info_size1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    // ERL_NIF_TERM info;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        return MAKEBADARG(env, status);
    }

    // @TODO implementation
    // info = enif_make_uint64(env, h->impl.db_size);

    // return info;
    return MAKEBADARG(env, status);
}

ERL_NIF_TERM
lets_impl_nif_notify4(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    (void) argc;

    leveldb::Status status;
    lets_impl_nif_handle* h;
    ErlNifPid pid;
    ErlNifEnv* msgenv;
    ERL_NIF_TERM msg;

    if (!enif_get_resource(env, argv[0], lets_impl_nif_RESOURCE, (void**)&h)) {
        return MAKEBADARG(env, status);
    }
    if (!enif_is_identical(argv[1], lets_atom_when_destroyed)) {
        return MAKEBADARG(env, status);
    }
    if (!enif_get_local_pid(env, argv[2], &pid)) {
        return MAKEBADARG(env, status);
    }
    if (!(msgenv = enif_alloc_env())) {
        return MAKEBADARG(env, status);
    }
    if (!(msg = enif_make_copy(env, argv[3]))) {
        enif_free_env(msgenv);
        return MAKEBADARG(env, status);
    }

    if (!h->impl.alive) {
        enif_free_env(msgenv);
        return MAKEBADARG(env, status);
    }

    h->impl.notify_when_destroyed.push_back(std::make_pair(pid, std::make_pair(msgenv, msg)));

    return lets_atom_true;
}
