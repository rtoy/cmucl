
/* @(#)s_scalbn.c 1.3 95/01/18 */
/*
 * ====================================================
 * Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
 *
 * Developed at SunSoft, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 */

/* 
 * scalbn (double x, int n)
 * scalbn(x,n) returns x* 2**n  computed by  exponent  
 * manipulation rather than by actually performing an 
 * exponentiation or a multiplication.
 */

#include "fdlibm.h"

#ifdef __STDC__
static const double
#else
static double
#endif
two54   =  1.80143985094819840000e+16, /* 0x43500000, 0x00000000 */
twom54  =  5.55111512312578270212e-17, /* 0x3C900000, 0x00000000 */
huge   = 1.0e+300,
tiny   = 1.0e-300;

#ifdef __STDC__
	double fdlibm_scalbn (double x, int n)
#else
	double fdlibm_scalbn (x,n)
	double x; int n;
#endif
{
	int  k,hx,lx;
	union { int i[2]; double d; } ux;

        ux.d = x;
	hx = ux.i[HIWORD];
	lx = ux.i[LOWORD];
        k = (hx&0x7ff00000)>>20;		/* extract exponent */
        if (k==0) {				/* 0 or subnormal x */
            if ((lx|(hx&0x7fffffff))==0) return x; /* +-0 */
	    x *= two54;
            ux.d = x;
	    hx = ux.i[HIWORD];
	    k = ((hx&0x7ff00000)>>20) - 54; 
            if (n< -50000) return fdlibm_setexception(x, FDLIBM_UNDERFLOW);; 	/*underflow*/
	    }
        if (k==0x7ff) return x+x;		/* NaN or Inf */
        k = k+n; 
        if (k >  0x7fe) return fdlibm_setexception(x, FDLIBM_OVERFLOW); /* overflow  */
        if (k > 0) 				/* normal result */
	    {ux.i[HIWORD] = (hx&0x800fffff)|(k<<20); return x;}
        if (k <= -54)
            if (n > 50000) 	/* in case integer overflow in n+k */
              return fdlibm_setexception(x, FDLIBM_OVERFLOW);	/*overflow*/
            else return fdlibm_setexception(x, FDLIBM_UNDERFLOW); 	/*underflow*/
        k += 54;				/* subnormal result */
        ux.i[HIWORD] = (hx&0x800fffff)|(k<<20);
        return x*twom54;
}
