/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/lispregs.h,v 1.2 1990/03/29 21:20:22 ch Exp $ */

#ifdef LANGUAGE_ASSEMBLY
#define REG(num) $num
#else
#define REG(num) num

extern char *lisp_register_names[];
#endif

#define ZERO    REG(0)
#define LIP     REG(1)
#define NL0     REG(2)
#define NL1     REG(3)
#define NL2     REG(4)
#define NL3     REG(5)
#define NL4     REG(6)
#define NARGS   REG(7)
#define A0      REG(8)
#define A1      REG(9)
#define A2      REG(10)
#define A3      REG(11)
#define A4      REG(12)
#define A5      REG(13)
#define CNAME   REG(14)
#define LEXENV  REG(15)
#define ARGS    REG(16)
#define OLDCONT REG(17)
#define LRA     REG(18)
#define L0      REG(19)
#define NULLREG REG(20)
#define BSP     REG(21)
#define CONT    REG(22)
#define CSP     REG(23)
#define FLAGS   REG(24)
#define ALLOC   REG(25)
#define L1      REG(28)
#define NSP     REG(29)
#define CODE    REG(30)
#define L2      REG(31)
