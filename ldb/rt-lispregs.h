/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/rt-lispregs.h,v 1.1 1991/05/24 18:46:09 wlott Exp $ */

#ifdef LANGUAGE_ASSEMBLY
#define REG(num) r/**/num
#else
#define REG(num) num
#endif

#define NREGS	(16)

#define NARGS	REG(0)
#define NSP	REG(1)
#define NL0	REG(2)
#define OCFP	REG(3)
#define NFP	REG(4)
#define CSP	REG(5)
#define CFP	REG(6)
#define CODE	REG(7)
#define NULLREG	REG(8)
#define CNAME	REG(9)
#define LEXENV	REG(10)
#define LRA	REG(11)
#define A0	REG(12)
#define A1	REG(13)
#define A2	REG(14)
#define LIP	REG(15)

#define REGNAMES \
	"NARGS",	"NSP",		"NL0",		"OCFP", \
	"NFP",		"CSP",		"CFP",		"CODE", \
	"NULL",		"CNAME",	"LEXENV",	"LRA", \
	"A0",		"A1",		"A2",		"LIP"
