/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include "fdlibm.h"

/*
 * sincos function. Compute sin(x) and cos(x) faster than computing
 * each individually.  Speedup comes from doing the pi argument
 * reduction just once.
 */
void
cmucl_sincos (double x, double *s, double *c)
{
    int ix;
    union { int i[2]; double d; } ux;

    ux.d = x;
    ix = ux.i[HIWORD] & 0x7fffffff;

    /* |x| ~< pi/4 */
    if (ix < 0x3fe921fb) {
        *s = __kernel_sin(x, 0.0, 0);
        *c = __kernel_cos(x, 0.0);
    } else {
        /* Argument reduction needed */
        double y[2];
        int n;
        
        n = __ieee754_rem_pio2(x, y);

        switch (n & 3) {
          case 0:
              *s = __kernel_sin(y[0], y[1], 1);
              *c = __kernel_cos(y[0], y[1]);
              break;
          case 1:
              *s = __kernel_cos(y[0], y[1]);
              *c = - __kernel_sin(y[0], y[1], 1);
              break;
          case 2:
              *s = - __kernel_sin(y[0], y[1], 1);
              *c = - __kernel_cos(y[0], y[1]);
              break;
          case 3:
              *s = - __kernel_cos(y[0], y[1]);
              *c = __kernel_sin(y[0], y[1], 1);
              break;
        }
    }

    return;
}
