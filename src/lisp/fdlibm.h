
/* @(#)fdlibm.h 1.5 04/04/22 */
/*
 * ====================================================
 * Copyright (C) 2004 by Sun Microsystems, Inc. All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 */

#ifndef FDLIBM_H
#define FDLIBM_H
/* Sometimes it's necessary to define __LITTLE_ENDIAN explicitly
   but these catch some common cases. */

#if defined(i386) || defined(i486) || \
	defined(intel) || defined(x86) || defined(i86pc) || \
	defined(__alpha) || defined(__osf__)
#define __LITTLE_ENDIAN
#endif

#ifdef __LITTLE_ENDIAN
enum { HIWORD = 1, LOWORD = 0 };
#else
enum { HIWORD = 0, LOWORD = 1 };
#endif

/*
 * ANSI/POSIX
 */
extern double fabs(double);
extern double floor(double);
extern double sqrt(double);

/* ieee style elementary functions */
extern int    __ieee754_rem_pio2(double,double*);

/*
 * Functions callable from C, intended to support IEEE arithmetic.
 */
extern double scalbn(double, int);

/* fdlibm kernel function */
extern int    __kernel_rem_pio2(double*,double*,int,int,int,const int*);

extern double __kernel_sin(double x, double y, int iy);
extern double __kernel_cos(double x, double y);
extern double __kernel_tan(double x, double y, int iy);
extern double fdlibm_sin(double x);
extern double fdlibm_cos(double x);
extern double fdlibm_tan(double x);
extern double fdlibm_expm1(double x);
extern double fdlibm_log1p(double x);
extern double fdlibm_atan(double x);
extern double fdlibm_tanh(double x);
extern double fdlibm_asinh(double x);
extern double __ieee754_exp(double x);
extern double __ieee754_log(double x);
extern double __ieee754_atan2(double y, double x);
extern double __ieee754_asin(double x);
extern double __ieee754_acos(double x);
extern double __ieee754_sinh(double x);
extern double __ieee754_cosh(double x);
extern double __ieee754_atanh(double x);
extern double __ieee754_acosh(double x);
extern double __ieee754_log10(double x);
extern double __ieee754_pow(double x, double y);
extern double __ieee754_hypot(double x, double y);
extern double fdlibm_scalbn(double x, int n);
extern double fdlibm_log2(double x);

enum FDLIBM_EXCEPTION {
  FDLIBM_DIVIDE_BY_ZERO,
  FDLIBM_UNDERFLOW,
  FDLIBM_OVERFLOW,
  FDLIBM_INVALID,
  FDLIBM_INEXACT
};

extern double fdlibm_setexception(double x, enum FDLIBM_EXCEPTION);

#endif
