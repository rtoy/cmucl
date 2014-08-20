#include <fenv.h>
#include <math.h>
#include <stdio.h>

#include "fdlibm.h"

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
          feraiseexcept(FE_DIVBYZERO);
          
          ret = copysign(INFINITY, x);
          break;
      case 1:
          /* Underflow. Use the sign of x to get a signed zero. */
          feraiseexcept(FE_UNDERFLOW);
          ret = copysign(0.0, x);
          break;
      case 2:
          /* overflow */
          feraiseexcept(FE_OVERFLOW);
          ret = copysign(INFINITY, x);
          break;
      case 3:
      {
          /* invalid */
          feraiseexcept(FE_INVALID);
          /*
           * FIXME: Of the many NaN values that we have, what NaN
           * should we return?
           */
          union { int i[2]; double d; } ux;
          ux.i[HIWORD] = 0x7ff00000;
          ux.i[LOWORD] = 0xdeadbeef;
          
          ret = ux.d;
          
          break;
      }
    }

    return ret;
}
