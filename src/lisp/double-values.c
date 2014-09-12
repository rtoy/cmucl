/*
 * Some helper functions to return specific double values for use by
 * fdlibm_setexceptions on NetBSD.  Currently, NetBSD doesn't have
 * feraiseexcept so we have to do something special.  See
 * fdlibm_setexceptions.
 */

#include "fdlibm.h"

double
double_zero()
{
    return 0.0;
}

/*
 * Return most-positive-double-float
 */
double
double_huge()
{
    union { int i[2]; double d; } ux;
    ux.i[HIWORD] = 0x7fefffff;
    ux.i[LOWORD] = 0xffffffff;

    return ux.d;
}

/*
 * Return least-positive-double-float
 */
double
double_tiny()
{
    union { int i[2]; double d; } ux;
    ux.i[HIWORD] = 0;
    ux.i[LOWORD] = 1;

    return ux.d;
}
