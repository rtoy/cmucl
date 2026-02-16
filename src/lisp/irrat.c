/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include "lisp.h"
#include "internals.h"
#include "fdlibm.h"

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
extern void cr_sincos(double, double *, double *);
#endif


/*
 * Wrappers for the special functions
 */

#define MAYBE_SIGNAL_INVALID(test, val)		\
    if ((test)) {				\
        return fdlibm_setexception(val, FDLIBM_INVALID);	\
    }

#define MAYBE_SIGNAL_OVERFLOW(x)	\
    if (isinf(x)) {	\
	return fdlibm_setexception(x, FDLIBM_OVERFLOW); \
    }

double
lisp_sin(double x)
{
#ifdef FEATURE_CORE_MATH
    MAYBE_SIGNAL_INVALID(isinf(x), x)

    return cr_sin(x);
#else    
    return fdlibm_sin(x);
#endif    
}

double
lisp_cos(double x)
{
#ifdef FEATURE_CORE_MATH
    MAYBE_SIGNAL_INVALID(isinf(x), x)

    return cr_cos(x);
#else    
    return fdlibm_cos(x);
#endif
}

double
lisp_tan(double x)
{
#ifdef FEATURE_CORE_MATH
    MAYBE_SIGNAL_INVALID(isinf(x), x)

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
    MAYBE_SIGNAL_OVERFLOW(x)
	
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
    MAYBE_SIGNAL_OVERFLOW(x)

    return cr_asinh(x);
#else    
    return fdlibm_asinh(x);
#endif
}

double
lisp_acosh(double x)
{
#ifdef FEATURE_CORE_MATH
    MAYBE_SIGNAL_INVALID(x < 1, x)

    MAYBE_SIGNAL_OVERFLOW(x)
    
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
    /*
     * Can't depend on cr_exp to signal underflow.  It seems the
     * underflow has been constant-folded to zero.  Hence, check for
     * underflow here and explicitly signal an underflow.  The
     * constant here is from core-math exp.c.
     */
    if (x <= -0x1.74910d52d3052p+9) {
	return fdlibm_setexception(0.0, FDLIBM_UNDERFLOW);
    }

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
    /*
     * cr_pow when compiled with older versions of gcc or clang can
     * cause failures in the ansi-tests [#469].  Ubuntu 25.10 and Fedora 41
     * (gcc only) are known to have compilers that work well enough
     * that the ansi-tests pass.
     */
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
    return fdlibm_scalbn(x, n);
}

void
lisp_sincos (double x, double *s, double *c)
{
#ifdef FEATURE_CORE_MATH
    cr_sincos(x, s, c);
#else    
    extern void cmucl_sincos(double, double*, double*);

    cmucl_sincos(x, s, c);
#endif
}
