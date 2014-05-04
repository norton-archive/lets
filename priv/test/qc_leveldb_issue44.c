// %%% The MIT License
// %%%
// %%% Copyright (C) 2011-2014 by Joseph Wayne Norton <norton@alum.mit.edu>
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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "qc_statemc_lets.h"


int main() {
    leveldb_t* Db;
    char g[1] = {'g'};
    size_t glen = 1;
    char nil[0] = {};
    size_t nillen = 0;

    // #1
    Db = open0();

    // #2
    close1(Db);

    // #3
    Db = reopen0();

    // #4
    put2(Db, g, glen, nil, nillen);

    // #5
    close1(Db);

    // #6
    Db = reopen0();

    // #7
    delete2(Db, g, glen);

    // #8
    delete2(Db, nil, nillen);

    // #9
    close1(Db);

    // #10
    Db = reopen0();

    // #11
    delete2(Db, nil, nillen);

    // #12
    close1(Db);

    // #13
    Db = reopen0();

    // #14
    put2(Db, nil, nillen, nil, nillen);

    // #15
    close1(Db);

    // #16
    Db = reopen0();

    // #17
    close1(Db);

    // #18
    Db = reopen0();

    // #19a
    char* bug = NULL;
    size_t buglen = 0;
    bug = last1(Db, &buglen);

    if (buglen == nillen && memcmp(bug, nil, nillen) == 0) {
        fprintf(stderr, "\n\n!!! NO - reappearing ghost key 'g' !!!\n\n");
    }
    assert(buglen == nillen);
    assert(memcmp(bug, nil, nillen) == 0);

    fprintf(stderr, "KEY POINT => sleep for background compaction to finish it's work");
    sleep(1);

    // #19a
    bug = NULL;
    buglen = 0;
    bug = last1(Db, &buglen);

    if (buglen == glen && memcmp(bug, g, glen) == 0) {
        fprintf(stderr, "\n\n!!! YES - reappearing ghost key 'g' !!!\n\n");
    }
    assert(buglen == nillen);
    assert(memcmp(bug, nil, nillen) == 0);

    return 0;
}

/*
Failed!
[{init,{state,qc_statemc_lets,{state,false,false,undefined,[]}}},
 {set,{var,1},{call,qc_leveldb,open,[]}},
 {set,{var,4},{call,qc_leveldb,close,[{var,1}]}},
 {set,{var,5},{call,qc_leveldb,reopen,[]}},
 {set,{var,6},{call,qc_leveldb,put,[{var,5},{obj,<<"a">>,<<>>}]}},
 {set,{var,9},{call,qc_leveldb,close,[{var,5}]}},
 {set,{var,10},{call,qc_leveldb,reopen,[]}},
 {set,{var,11},{call,qc_leveldb,delete,[{var,10},<<"a">>]}},
 {set,{var,14},{call,qc_leveldb,delete,[{var,10},<<>>]}},
 {set,{var,15},{call,qc_leveldb,close,[{var,10}]}},
 {set,{var,19},{call,qc_leveldb,reopen,[]}},
 {set,{var,22},{call,qc_leveldb,delete,[{var,19},<<>>]}},
 {set,{var,26},{call,qc_leveldb,close,[{var,19}]}},
 {set,{var,27},{call,qc_leveldb,reopen,[]}},
 {set,{var,31},{call,qc_leveldb,put,[{var,27},{obj,<<>>,<<>>}]}},
 {set,{var,33},{call,qc_leveldb,close,[{var,27}]}},
 {set,{var,34},{call,qc_leveldb,reopen,[]}},
 {set,{var,36},{call,qc_leveldb,close,[{var,34}]}},
 {set,{var,37},{call,qc_leveldb,reopen,[]}},
 {set,{var,40},{call,qc_leveldb,last,[{var,37}]}}]

COMMANDS:
        "qc_statem-20111029-191936.erl"

HISTORY:
 #1:
        Cmd: {set,{var,1},{call,qc_leveldb,open,[]}}
        Reply: {ptr,{struct,leveldb_t},16833504}
        State: {state,qc_statemc_lets,{state,false,false,undefined,[]}}

 #2:
        Cmd: {set,{var,4},{call,qc_leveldb,close,[{var,1}]}}
        Reply: true
        State: {state,qc_statemc_lets,
                      {state,false,true,{ptr,{struct,leveldb_t},16833504},[]}}

 #3:
        Cmd: {set,{var,5},{call,qc_leveldb,reopen,[]}}
        Reply: {ptr,{struct,leveldb_t},16833616}
        State: {state,qc_statemc_lets,{state,false,true,undefined,[]}}

 #4:
        Cmd: {set,{var,6},{call,qc_leveldb,put,[{var,5},{obj,<<"a">>,<<>>}]}}
        Reply: true
        State: {state,qc_statemc_lets,
                      {state,false,true,{ptr,{struct,leveldb_t},16833616},[]}}

 #5:
        Cmd: {set,{var,9},{call,qc_leveldb,close,[{var,5}]}}
        Reply: true
        State: {state,qc_statemc_lets,
                      {state,false,true,
                             {ptr,{struct,leveldb_t},16833616},
                             [{obj,<<"a">>,<<>>}]}}

 #6:
        Cmd: {set,{var,10},{call,qc_leveldb,reopen,[]}}
        Reply: {ptr,{struct,leveldb_t},16831264}
        State: {state,qc_statemc_lets,
                      {state,false,true,undefined,[{obj,<<"a">>,<<>>}]}}

 #7:
        Cmd: {set,{var,11},{call,qc_leveldb,delete,[{var,10},<<"a">>]}}
        Reply: true
        State: {state,qc_statemc_lets,
                      {state,false,true,
                             {ptr,{struct,leveldb_t},16831264},
                             [{obj,<<"a">>,<<>>}]}}

 #8:
        Cmd: {set,{var,14},{call,qc_leveldb,delete,[{var,10},<<>>]}}
        Reply: true
        State: {state,qc_statemc_lets,
                      {state,false,true,{ptr,{struct,leveldb_t},16831264},[]}}

 #9:
        Cmd: {set,{var,15},{call,qc_leveldb,close,[{var,10}]}}
        Reply: true
        State: {state,qc_statemc_lets,
                      {state,false,true,{ptr,{struct,leveldb_t},16831264},[]}}

 #10:
        Cmd: {set,{var,19},{call,qc_leveldb,reopen,[]}}
        Reply: {ptr,{struct,leveldb_t},16831776}
        State: {state,qc_statemc_lets,{state,false,true,undefined,[]}}

 #11:
        Cmd: {set,{var,22},{call,qc_leveldb,delete,[{var,19},<<>>]}}
        Reply: true
        State: {state,qc_statemc_lets,
                      {state,false,true,{ptr,{struct,leveldb_t},16831776},[]}}

 #12:
        Cmd: {set,{var,26},{call,qc_leveldb,close,[{var,19}]}}
        Reply: true
        State: {state,qc_statemc_lets,
                      {state,false,true,{ptr,{struct,leveldb_t},16831776},[]}}

 #13:
        Cmd: {set,{var,27},{call,qc_leveldb,reopen,[]}}
        Reply: {ptr,{struct,leveldb_t},16831712}
        State: {state,qc_statemc_lets,{state,false,true,undefined,[]}}

 #14:
        Cmd: {set,{var,31},{call,qc_leveldb,put,[{var,27},{obj,<<>>,<<>>}]}}
        Reply: true
        State: {state,qc_statemc_lets,
                      {state,false,true,{ptr,{struct,leveldb_t},16831712},[]}}

 #15:
        Cmd: {set,{var,33},{call,qc_leveldb,close,[{var,27}]}}
        Reply: true
        State: {state,qc_statemc_lets,
                      {state,false,true,
                             {ptr,{struct,leveldb_t},16831712},
                             [{obj,<<>>,<<>>}]}}

 #16:
        Cmd: {set,{var,34},{call,qc_leveldb,reopen,[]}}
        Reply: {ptr,{struct,leveldb_t},16832720}
        State: {state,qc_statemc_lets,
                      {state,false,true,undefined,[{obj,<<>>,<<>>}]}}

 #17:
        Cmd: {set,{var,36},{call,qc_leveldb,close,[{var,34}]}}
        Reply: true
        State: {state,qc_statemc_lets,
                      {state,false,true,
                             {ptr,{struct,leveldb_t},16832720},
                             [{obj,<<>>,<<>>}]}}

 #18:
        Cmd: {set,{var,37},{call,qc_leveldb,reopen,[]}}
        Reply: {ptr,{struct,leveldb_t},16828000}
        State: {state,qc_statemc_lets,
                      {state,false,true,undefined,[{obj,<<>>,<<>>}]}}

 #19:
        Cmd: {set,{var,40},{call,qc_leveldb,last,[{var,37}]}}
        Reply: <<"a">>
        State: {state,qc_statemc_lets,
                      {state,false,true,
                             {ptr,{struct,leveldb_t},16828000},
                             [{obj,<<>>,<<>>}]}}

RESULT:
        {postcondition,false}

STATE:
        {state,qc_statemc_lets,
               {state,false,true,
                      {ptr,{struct,leveldb_t},16828000},
                      [{obj,<<>>,<<>>}]}}

STATE IS SANE:
        true
false
*/
