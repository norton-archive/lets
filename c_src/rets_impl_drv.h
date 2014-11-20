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

#ifndef RETS_DRV_H
#define RETS_DRV_H

#ifdef __cplusplus
extern "C" {
#endif

#include <ei.h>
#include <erl_driver.h>

#if ERL_DRV_EXTENDED_MAJOR_VERSION<2
    typedef int ErlDrvSizeT;
    typedef int ErlDrvSSizeT;
#endif

    // @doc driver init
    DRIVER_INIT(lib_lets_impl_drv);

    // @doc driver callbacks
    int drv_init(void);

    ErlDrvData drv_start(ErlDrvPort port, char* command);
    void drv_stop(ErlDrvData);
    void drv_output(ErlDrvData drv_data, char* buf, ErlDrvSizeT len);
    void drv_ready_async(ErlDrvData, ErlDrvThreadData);

#ifdef __cplusplus
}
#endif

#endif /* RETS_DRV_H */
