/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include "fdlibm.h"

/*
 * Wrappers for the special functions
 */

double
lisp_sin(double x)
{
    return fdlibm_sin(x);
}

double
lisp_cos(double x)
{
    return fdlibm_cos(x);
}

double
lisp_tan(double x)
{
    return fdlibm_tan(x);
}

double
lisp_atan(double x)
{
    return fdlibm_atan(x);
}

double
lisp_atan2(double y, double x)
{
    return __ieee754_atan2(y, x);
}

double
lisp_asin(double x)
{
    return __ieee754_asin(x);
}

double
lisp_acos(double x)
{
    return __ieee754_acos(x);
}

double
lisp_sinh(double x)
{
    return __ieee754_sinh(x);
}

double
lisp_cosh(double x)
{
    return __ieee754_cosh(x);
}

double
lisp_tanh(double x)
{
    return fdlibm_tanh(x);
}

double
lisp_asinh(double x)
{
    return fdlibm_asinh(x);
}

double
lisp_acosh(double x)
{
    return __ieee754_acosh(x);
}

double
lisp_atanh(double x)
{
    return __ieee754_atanh(x);
}

double
lisp_exp(double x)
{
    return __ieee754_exp(x);
}

double
lisp_log(double x)
{
    return __ieee754_log(x);
}

double
lisp_log10(double x)
{
    return __ieee754_log10(x);
}

double
lisp_pow(double x, double y)
{
    return __ieee754_pow(x, y);
}

double
lisp_hypot(double x, double y)
{
    return __ieee754_hypot(x, y);
}

double
lisp_log1p(double x)
{
    return fdlibm_log1p(x);
}

double
lisp_expm1(double x)
{
    return fdlibm_expm1(x);
}

double
lisp_scalbn(double x, int n)
{
    return fdlibm_scalbn(x, n);
}
