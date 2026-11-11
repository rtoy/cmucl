/*
 * ====================================================
 * Copyright (C) 2004 by Sun Microsystems, Inc. All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 */

/*
 * This is the log2 algorithm pulled out of __ieee754_pow in e_pow.c
 */

/*
 * Method:  Let x =  2   * (1+f)
 *	1. Compute and return log2(x) in two pieces:
 *		log2(x) = w1 + w2,
 *	   where w1 has 53-24 = 29 bit trailing zeros.
 */

#include "fdlibm.h"

static double
bp[] = {1.0, 1.5,},
dp_h[] = { 0.0, 5.84962487220764160156e-01,}, /* 0x3FE2B803, 0x40000000 */
dp_l[] = { 0.0, 1.35003920212974897128e-08,}, /* 0x3E4CFDEB, 0x43CFD006 */
zero    =  0.0,
one	=  1.0,
two53	=  9007199254740992.0,	/* 0x43400000, 0x00000000 */
	/* poly coefs for (3/2)*(log(x)-2s-2/3*s**3 */
L1  =  5.99999999999994648725e-01, /* 0x3FE33333, 0x33333303 */
L2  =  4.28571428578550184252e-01, /* 0x3FDB6DB6, 0xDB6FABFF */
L3  =  3.33333329818377432918e-01, /* 0x3FD55555, 0x518F264D */
L4  =  2.72728123808534006489e-01, /* 0x3FD17460, 0xA91D4101 */
L5  =  2.30660745775561754067e-01, /* 0x3FCD864A, 0x93C9DB65 */
L6  =  2.06975017800338417784e-01, /* 0x3FCA7E28, 0x4A454EEF */
cp    =  9.61796693925975554329e-01, /* 0x3FEEC709, 0xDC3A03FD =2/(3ln2) */
cp_h  =  9.61796700954437255859e-01, /* 0x3FEEC709, 0xE0000000 =(float)cp */
cp_l  = -7.02846165095275826516e-09; /* 0xBE3E2FE0, 0x145B01F5 =tail of cp_h*/

double fdlibm_log2(double x)
{
    double ax;
    int k, hx, lx, ix;
    union { int i[2]; double d; } ux;

    ux.d = x;
    hx = ux.i[HIWORD]; lx = ux.i[LOWORD];
    ix = hx&0x7fffffff;

    ax   = fabs(x);

    /* special value of x */

    if (hx < 0x00100000) {			/* x < 2**-1022  */
        if (((hx&0x7fffffff)|lx)==0) {
            /* log(+-0)=-inf */
            return fdlibm_setexception(-1.0, FDLIBM_DIVIDE_BY_ZERO);
        }
            
        if (hx<0) {
            /* log(-#) = NaN */
            return fdlibm_setexception(x, FDLIBM_INVALID);
        }
    }
        
    {
        double ss,s2,s_h,s_l,t_h,t_l;
        double z_h,z_l,p_h,p_l;
        double r, u, v, t, t1, t2;
        int n, j;
        
        n = 0;
	/* take care subnormal number */
        if(ix<0x00100000) {
            ax *= two53;
            n -= 53;
            ux.d = ax;
            ix = ux.i[HIWORD];
        }
        n  += ((ix)>>20)-0x3ff;
        j  = ix&0x000fffff;
	/* determine interval */
        ix = j|0x3ff00000;		/* normalize ix */
        if(j<=0x3988E) k=0;		/* |x|<sqrt(3/2) */
        else if(j<0xBB67A) k=1;	/* |x|<sqrt(3)   */
        else {k=0;n+=1;ix -= 0x00100000;}
        ux.d = ax;
        ux.i[HIWORD] = ix;
        ax = ux.d;

	/* compute ss = s_h+s_l = (x-1)/(x+1) or (x-1.5)/(x+1.5) */
        u = ax-bp[k];		/* bp[0]=1.0, bp[1]=1.5 */
        v = one/(ax+bp[k]);
        ss = u*v;
        s_h = ss;
        ux.d = s_h;
        ux.i[LOWORD] = 0;
        s_h = ux.d;
	/* t_h=ax+bp[k] High */
        t_h = zero;
        ux.d = t_h;
        ux.i[HIWORD]=((ix>>1)|0x20000000)+0x00080000+(k<<18);
        t_h = ux.d;
        t_l = ax - (t_h-bp[k]);
        s_l = v*((u-s_h*t_h)-s_h*t_l);
	/* compute log(ax) */
        s2 = ss*ss;
        r = s2*s2*(L1+s2*(L2+s2*(L3+s2*(L4+s2*(L5+s2*L6)))));
        r += s_l*(s_h+ss);
        s2  = s_h*s_h;
        t_h = 3.0+s2+r;
        ux.d = t_h;
        ux.i[LOWORD] = 0;
        t_h = ux.d;
        t_l = r-((t_h-3.0)-s2);
	/* u+v = ss*(1+...) */
        u = s_h*t_h;
        v = s_l*t_h+t_l*ss;
	/* 2/(3log2)*(ss+...) */
        p_h = u+v;
        ux.d = p_h;
        ux.i[LOWORD] = 0;
        p_h = ux.d;
        p_l = v-(p_h-u);
        z_h = cp_h*p_h;		/* cp_h+cp_l = 2/(3*log2) */
        z_l = cp_l*p_h+p_l*cp+dp_l[k];
	/* log2(ax) = (ss+..)*2/(3*log2) = n + dp_h + z_h + z_l */
        t = (double)n;
        t1 = (((z_h+z_l)+dp_h[k])+t);
        ux.d = t1;
        ux.i[LOWORD] = 0;
        t1 = ux.d;
        t2 = z_l-(((t1-t)-dp_h[k])-z_h);

        return t1 + t2;
    }
}
