/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include "lisp.h"
#include "internals.h"
#ifdef FEATURE_CORE_MATH
#include <math.h>
extern double cr_sin(double);
extern double cr_cos(double);
extern double cr_tan(double);
extern double cr_atan(double);
extern double cr_atan2(double, double);
extern double cr_asin(double);
extern double cr_acos(double);
extern double cr_sinh(double);
extern double cr_cosh(double);
extern double cr_tanh(double);
extern double cr_asinh(double);
extern double cr_acosh(double);
extern double cr_atanh(double);
extern double cr_exp(double);
extern double cr_log(double);
extern double cr_log10(double);
extern double cr_pow(double, double);
extern double cr_hypot(double, double);
extern double cr_log1p(double);
extern double cr_expm1(double);
#else
#include "fdlibm.h"
#endif


/*
 * Wrappers for the special functions
 */

double
lisp_sin(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_sin(x);
#else    
    return fdlibm_sin(x);
#endif    
}

double
lisp_cos(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_cos(x);
#else    
    return fdlibm_cos(x);
#endif
}

double
lisp_tan(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_tan(x);
#else    
    return fdlibm_tan(x);
#endif
}

double
lisp_atan(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_atan(x);
#else    
    return fdlibm_atan(x);
#endif
}

double
lisp_atan2(double y, double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_atan2(y, x);
#else    
    return __ieee754_atan2(y, x);
#endif
}

double
lisp_asin(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_asin(x);
#else    
    return __ieee754_asin(x);
#endif
}

double
lisp_acos(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_acos(x);
#else    
    return __ieee754_acos(x);
#endif
}

double
lisp_sinh(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_sinh(x);
#else    
    return __ieee754_sinh(x);
#endif
}

double
lisp_cosh(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_cosh(x);
#else    
    return __ieee754_cosh(x);
#endif
}

double
lisp_tanh(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_tanh(x);
#else    
    return fdlibm_tanh(x);
#endif
}

double
lisp_asinh(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_asinh(x);
#else    
    return fdlibm_asinh(x);
#endif
}

double
lisp_acosh(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_acosh(x);
#else    
    return __ieee754_acosh(x);
#endif
}

double
lisp_atanh(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_atanh(x);
#else    
    return __ieee754_atanh(x);
#endif
}

double
lisp_exp(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_exp(x);
#else    
    return __ieee754_exp(x);
#endif
}

double
lisp_log(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_log(x);
#else    
    return __ieee754_log(x);
#endif
}

double
lisp_log10(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_log10(x);
#else    
    return __ieee754_log10(x);
#endif
}

double
lisp_pow(double x, double y)
{
#ifdef FEATURE_CORE_MATH
    return cr_pow(x, y);
#else    
    return __ieee754_pow(x, y);
#endif
}

double
lisp_hypot(double x, double y)
{
#ifdef FEATURE_CORE_MATH
    return cr_hypot(x, y);
#else    
    return __ieee754_hypot(x, y);
#endif
}

double
lisp_log1p(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_log1p(x);
#else    
    return fdlibm_log1p(x);
#endif
}

double
lisp_expm1(double x)
{
#ifdef FEATURE_CORE_MATH
    return cr_expm1(x);
#else    
    return fdlibm_expm1(x);
#endif
}

double
lisp_scalbn(double x, int n)
{
#ifdef FEATURE_CORE_MATH
    return scalbn(x, n);
#else    
    return fdlibm_scalbn(x, n);
#endif
}
