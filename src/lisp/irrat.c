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

/*
 * Wrappers for the single-float versions
 */
float
lisp_sinf(float x)
{
    return (float) fdlibm_sin((double) x);
}

float
lisp_cosf(float x)
{
    return (float) fdlibm_cos((double) x);
}

float
lisp_tanf(float x)
{
    return (float) fdlibm_tan((double) x);
}

float
lisp_atanf(float x)
{
    return (float) fdlibm_atan((double) x);
}

float
lisp_atan2f(float y, float x)
{
    return (float) __ieee754_atan2((double) y, (double) x);
}

float
lisp_asinf(float x)
{
    return (float) __ieee754_asin((double) x);
}

float
lisp_acosf(float x)
{
    return (float) __ieee754_acos((double) x);
}

float
lisp_sinhf(float x)
{
    return (float) __ieee754_sinh((double) x);
}

float
lisp_coshf(float x)
{
    return (float) __ieee754_cosh((double) x);
}

float
lisp_tanhf(float x)
{
    return (float) fdlibm_tanh((double) x);
}

float
lisp_asinhf(float x)
{
    return (float) fdlibm_asinh((double) x);
}

float
lisp_acoshf(float x)
{
    return (float) __ieee754_acosh((double) x);
}

float
lisp_atanhf(float x)
{
    return (float) __ieee754_atanh((double) x);
}

float
lisp_expf(float x)
{
    return (float) __ieee754_exp((double) x);
}

float
lisp_logf(float x)
{
    return (float) __ieee754_log((double) x);
}

float
lisp_log10f(float x)
{
    return (float) __ieee754_log10((double) x);
}

float
lisp_powf(float x, float y)
{
    return (float) __ieee754_pow((double) x, (double) y);
}

float
lisp_hypotf(float x, float y)
{
    return (float) __ieee754_hypot((double) x, (double) y);
}

float
lisp_log1pf(float x)
{
    return (float) fdlibm_log1p((double) x);
}

float
lisp_expm1f(float x)
{
    return (float) fdlibm_expm1((double) x);
}
