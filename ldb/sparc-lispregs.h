/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/sparc-lispregs.h,v 1.1 1991/05/24 18:45:57 wlott Exp $ */

#ifdef LANGUAGE_ASSEMBLY

#define GREG(num) %g/**/num
#define OREG(num) %o/**/num
#define LREG(num) %l/**/num
#define IREG(num) %i/**/num

#else

#define GREG(num) (num)
#define OREG(num) ((num)+8)
#define LREG(num) ((num)+16)
#define IREG(num) ((num)+24)

#endif

#define NREGS	(32)

#define ZERO	GREG(0)
#define ALLOC	GREG(1)
#define NULLREG	GREG(2)
#define CSP	GREG(3)
#define CFP	GREG(4)
#define BSP	GREG(5)
#define NFP	GREG(6)
#define CFUNC	GREG(7)

#define NL0	OREG(0)
#define NL1	OREG(1)
#define NL2	OREG(2)
#define NL3	OREG(3)
#define NL4	OREG(4)
#define NL5	OREG(5)
#define NSP	OREG(6)
#define NARGS	OREG(7)

#define A0	LREG(0)
#define A1	LREG(1)
#define A2	LREG(2)
#define A3	LREG(3)
#define A4	LREG(4)
#define A5	LREG(5)
#define OCFP	LREG(6)
#define LRA	LREG(7)

#define CNAME	IREG(0)
#define LEXENV	IREG(1)
#define L0	IREG(2)
#define L1	IREG(3)
#define L2	IREG(4)
#define CODE	IREG(5)
#define LIP	IREG(7)

#define REGNAMES \
	"ZERO",		"ALLOC",	"NULL",		"CSP", \
	"CFP",		"BSP",		"NFP",		"CFUNC", \
        "NL0",		"NL1",		"NL2",		"NL3", \
        "NL4",		"NL5",		"NSP",		"NARGS", \
        "A0",		"A1",		"A2",		"A3", \
        "A4",		"A5",		"OCFP",		"LRA", \
        "CNAME",	"LEXENV",	"L0",		"L1", \
        "L2",		"CODE",		"???",		"LIP"
