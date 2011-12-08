#!/bin/bash

# The MIT License
#
# Copyright (C) 2011 by Joseph Wayne Norton <norton@alum.mit.edu>
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

set -e
set -o pipefail

SNAPPY_VSN=HEAD
LEVELDB_VSN=HEAD

if [ `basename $PWD` != "c_src" ]; then
    pushd c_src
fi

BASEDIR="$PWD"

case "$1" in
    clean)
        rm -rf snappy snappy-$SNAPPY_VSN
        rm -rf leveldb leveldb-$LEVELDB_VSN
        ;;

    get_deps)
        git clone git://github.com/norton/snappy.git $BASEDIR/snappy_git
        git clone git://github.com/norton/leveldb.git $BASEDIR/leveldb_git
        ;;

    *)
        # snappy
        if [ ! -f $BASEDIR/snappy/lib/libsnappy.a  ]; then
            LIBTOOLIZE=libtoolize
            ($LIBTOOLIZE --version) < /dev/null > /dev/null 2>&1 || {
                LIBTOOLIZE=glibtoolize
                ($LIBTOOLIZE --version) < /dev/null > /dev/null 2>&1 || {
                    echo
                    echo "You must have libtool (& friends) installed to compile Judy."
                    echo
                    exit -1
                }
            }

            (cd $BASEDIR/snappy_git && git archive --format=tar --prefix=snappy-$SNAPPY_VSN/ $SNAPPY_VSN) \
                | tar xf -
            (cd snappy-$SNAPPY_VSN && \
                sed -ibak '/^AC_ARG_WITH.*$/, /^fi$/d' configure.ac
            )
            (cd snappy-$SNAPPY_VSN && \
                rm -rf autom4te.cache && \
                aclocal -I m4 && \
                autoheader && \
                $LIBTOOLIZE --copy && \
                automake --add-missing --copy && \
                autoconf)
            (cd snappy-$SNAPPY_VSN && \
                ./configure $CONFFLAGS \
                --enable-static \
                --disable-shared \
                --with-pic \
                --prefix=$BASEDIR/snappy &&  \
                make install)
        fi
        # leveldb
        if [ ! -f $BASEDIR/leveldb/lib/libleveldb.a  ]; then
            (cd $BASEDIR/leveldb_git && git archive --format=tar --prefix=leveldb-$LEVELDB_VSN/ $LEVELDB_VSN) \
                | tar xf -
            (cd leveldb-$LEVELDB_VSN && \
                echo "echo \"PLATFORM_CFLAGS+=-fPIC -I$BASEDIR/snappy/include\" >> build_config.mk" >> build_detect_platform &&
                echo "echo \"PLATFORM_LDFLAGS+=-L $BASEDIR/snappy/lib -lsnappy\" >> build_config.mk" >> build_detect_platform &&
                make SNAPPY=1 && \
                mkdir -p $BASEDIR/leveldb/include/leveldb && \
                install include/leveldb/*.h $BASEDIR/leveldb/include/leveldb && \
                mkdir -p $BASEDIR/leveldb/lib && \
                install libleveldb.a $BASEDIR/leveldb/lib)
        fi
        ;;
esac
