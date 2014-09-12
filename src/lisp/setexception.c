#if defined(__NetBSD__)
/*
 * NetBSD doesn't have fenv.h. At least the version currently being
 * used to build cmucl doesn't.  Assume this also means we don't have
 * feraiseexcept.  So, to generate the desired exceptions, we have to
 * create the appropriate operations to generate the desired
 * exceptions.
 */
#undef HAVE_FERAISEEXCEPT

extern double double_zero();
extern double double_huge();
extern double double_tiny();

#else
#define HAVE_FERAISEEXCEPT
#endif

#ifdef HAVE_FERAISEEXCEPT
#include <fenv.h>
#endif

#include <math.h>
#include <stdio.h>

#include "fdlibm.h"

/*
 * Test if the given number is a quiet NaN
 */

int
isQNaN(double x)
{
    int hx;
    union { int i[2]; double d; } ux;

    ux.d = x;
    hx = ux.i[HIWORD] & 0x7fffffff;

    if (hx >= 0x7ff00000) {
        /*
         * We have some kind of infinity or NaN. Get the (top)
         * mantissa bits. We have a quiet NaN if the most significant
         * bit is 1. The other bits of the mantissa don't matter. We
         * also don't distinguish this from the quiet NaN
         * floating-point indefinite which only has the most
         * significant bit set. These are all considered NaNs for our
         * purposes.
         */
        hx &= 0xfffff;
        
        return hx & 0x80000;
    }

    return 0;
}

/*
 * Signal the floating-point exception of the given |type|, based on
 * the value of |x|.
 */

double
fdlibm_setexception(double x, enum FDLIBM_EXCEPTION type)
{
    double ret;
    
    switch (type) {
      case 0:
          /* Division by zero. Use the sign of x to get the correct
           *  signed infinity
           */
#ifdef HAVE_FERAISEEXCEPT
          feraiseexcept(FE_DIVBYZERO);
          ret = copysign(INFINITY, x);
#else
          ret = copysign(1, x) / double_zero();
#endif
          
          break;
      case 1:
          /* Underflow. Use the sign of x to get a signed zero. */
#ifdef HAVE_FERAISEEXCEPT
          feraiseexcept(FE_UNDERFLOW);
          ret = copysign(0.0, x);
#else
          ret = double_tiny() * double_tiny();;
#endif
          break;
      case 2:
          /* overflow */
#ifdef HAVE_FERAISEEXCEPT
          feraiseexcept(FE_OVERFLOW);
          ret = copysign(INFINITY, x);
#else
          ret = double_huge() * copysign(double_huge(), x);
#endif
          break;
      case 3:
      {
          /* if */

          if (!isQNaN(x)) {
              /*
               * If it's not a quiet NaN, we want to signal an invalid
               * operation. Otherwise, we silently return a NaN.
               */
#ifdef HAVE_FERAISEEXCEPT
              feraiseexcept(FE_INVALID);
#else
              ret = double_zero() / double_zero();
              return ret;
#endif
          }
          /*
           * FIXME: Of the many NaN values that we have, what NaN
           * should we return?
           */
          union { int i[2]; double d; } ux;
          ux.i[HIWORD] = 0x7ff80000;
          ux.i[LOWORD] = 0xdeadbeef;
          
          ret = ux.d;
          
          break;
      }
    }

    return ret;
}
