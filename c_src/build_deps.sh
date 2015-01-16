#!/bin/bash

# The MIT License
#
# Copyright (C) 2011-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

set -eo pipefail

SNAPPY_VSN=HEAD
LEVELDB_VSN=HEAD
HYPERLEVELDB_VSN=HEAD
ROCKSDB_VSN=HEAD

if [ `basename $PWD` != "c_src" ]; then
    pushd c_src > /dev/null 2>&1
fi

BASEDIR="$PWD"

case "$1" in
    clean)
        rm -f *.o ../priv/lib/*.so
        rm -rf snappy snappy-$SNAPPY_VSN
        rm -rf leveldb leveldb-$LEVELDB_VSN
        rm -rf hyperleveldb hyperleveldb-$HYPERLEVELDB_VSN
        rm -rf rocksdb rocksdb-$ROCKSDB_VSN
        ;;
    get_deps)
        ;;
    update_deps)
        ;;
    *)
        LIBTOOLIZE=libtoolize
        ($LIBTOOLIZE --version) < /dev/null > /dev/null 2>&1 || {
            LIBTOOLIZE=glibtoolize
            ($LIBTOOLIZE --version) < /dev/null > /dev/null 2>&1 || {
                echo
                echo "You must have libtool (& friends) installed to compile LETS."
                echo
                exit -1
            }
        }

        # snappy
        if [ ! -f $BASEDIR/snappy/lib/libsnappy.a ]; then
            (cd $REBAR_DEPS_DIR/snappy && git archive --format=tar --prefix=snappy-$SNAPPY_VSN/ $SNAPPY_VSN) \
                | tar xf -
            (cd snappy-$SNAPPY_VSN && \
                sed -ibak1 '/^AC_ARG_WITH.*$/, /^fi$/d' configure.ac && \
                perl -ibak2 -pe 's/LT_INIT/AM_PROG_AR\nLT_INIT/' configure.ac
            )
            (cd snappy-$SNAPPY_VSN && \
                rm -rf autom4te.cache && \
                $LIBTOOLIZE --copy && \
                aclocal -I m4 && \
                autoheader && \
                automake --add-missing --copy && \
                autoconf)
            (cd snappy-$SNAPPY_VSN && \
                ./configure $CONFFLAGS \
                --enable-static \
                --disable-shared \
                --with-pic \
                --prefix=$BASEDIR/snappy &&  \
                make install)
            rm -f $BASEDIR/snappy/lib/libsnappy.la
        fi
        # leveldb
        if [ ! -f $BASEDIR/leveldb/lib/libleveldb.a ]; then
            (cd $REBAR_DEPS_DIR/leveldb && git archive --format=tar --prefix=leveldb-$LEVELDB_VSN/ $LEVELDB_VSN) \
                | tar xf -
            (cd leveldb-$LEVELDB_VSN && \
                echo "echo \"PLATFORM_CFLAGS+=-fPIC -I$BASEDIR/snappy/include\" >> build_config.mk" >> build_detect_platform &&
                echo "echo \"PLATFORM_CXXFLAGS+=-fPIC -I$BASEDIR/snappy/include\" >> build_config.mk" >> build_detect_platform &&
                echo "echo \"PLATFORM_LDFLAGS+=-L $BASEDIR/snappy/lib -lsnappy\" >> build_config.mk" >> build_detect_platform &&
                make SNAPPY=1 && \
                mkdir -p $BASEDIR/leveldb/include/leveldb && \
                install include/leveldb/*.h $BASEDIR/leveldb/include/leveldb && \
                mkdir -p $BASEDIR/leveldb/lib && \
                install libleveldb.a $BASEDIR/leveldb/lib)
        fi
        # hyperleveldb
        if [ ! -f $BASEDIR/hyperleveldb/lib/libhyperleveldb.a ]; then
            (cd $REBAR_DEPS_DIR/hyperleveldb && git archive --format=tar --prefix=hyperleveldb-$HYPERLEVELDB_VSN/ $HYPERLEVELDB_VSN) \
                | tar xf -
            (cd hyperleveldb-$HYPERLEVELDB_VSN && \
                perl -ibak1 -pe 's/leveldb\.la/leveldb.a/g;' Makefile.am && \
                perl -ibak2 -pe 's/leveldb_la/leveldb_a/g;' Makefile.am && \
                perl -ibak3 -pe 's/lib_LTLIBARIES/lib_LIBRARIES/g;' Makefile.am)
            (cd hyperleveldb-$HYPERLEVELDB_VSN && \
                rm -rf autom4te.cache && \
                $LIBTOOLIZE --copy && \
                aclocal -I m4 && \
                autoheader && \
                automake --add-missing --copy && \
                autoconf)
            (cd hyperleveldb-$HYPERLEVELDB_VSN && \
                env CPPFLAGS="-fPIC -I$BASEDIR/snappy/include $CPPFLAGS" \
                    CFLAGS="-fPIC -I$BASEDIR/snappy/include $CFLAGS" \
                    CXXFLAGS="-fPIC -I$BASEDIR/snappy/include $CXXFLAGS" \
                    LDFLAGS="-L$BASEDIR/snappy/lib -lsnappy $LDFLAGS" \
                    ./configure --enable-static --enable-snappy && \
                make && \
                mkdir -p $BASEDIR/hyperleveldb/include/hyperleveldb && \
                install include/hyperleveldb/*.h $BASEDIR/hyperleveldb/include/hyperleveldb && \
                mkdir -p $BASEDIR/hyperleveldb/lib && \
                install libhyperleveldb.a $BASEDIR/hyperleveldb/lib)
        fi
        # rocksdb
        if [ ! -f $BASEDIR/rocksdb/lib/librocksdb.a ]; then
            (cd $REBAR_DEPS_DIR/rocksdb && git archive --format=tar --prefix=rocksdb-$ROCKSDB_VSN/ $ROCKSDB_VSN) \
                | tar xf -
            (cd rocksdb-$ROCKSDB_VSN && \
                echo "echo \"PLATFORM_CFLAGS+=-DSNAPPY -I$BASEDIR/snappy/include\" >> build_config.mk" >> build_tools/build_detect_platform &&
                echo "echo \"PLATFORM_CXXFLAGS+=-DSNAPPY -I$BASEDIR/snappy/include\" >> build_config.mk" >> build_tools/build_detect_platform &&
                echo "echo \"PLATFORM_LDFLAGS+=-L $BASEDIR/snappy/lib -lsnappy\" >> build_config.mk" >> build_tools/build_detect_platform &&
                make static_lib && \
                mkdir -p $BASEDIR/rocksdb/include/rocksdb && \
                install include/rocksdb/*.h $BASEDIR/rocksdb/include/rocksdb && \
                mkdir -p $BASEDIR/rocksdb/lib && \
                install librocksdb.a $BASEDIR/rocksdb/lib)
        fi
        ;;
esac
