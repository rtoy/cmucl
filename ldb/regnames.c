/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/regnames.c,v 1.3 1990/10/23 00:02:53 wlott Exp $ */

#include "lispregs.h"

char *lisp_register_names[] = {
#ifdef mips
	"ZERO",
	"NL3",
	"NL4",
	"FLAGS",
	"NL0",
	"NL1",
	"NL2",
	"NARGS",
	"A0",
	"A1",
	"A2",
	"A3",
	"A4",
	"A5",
	"CNAME",
	"LEXENV",
	"NFP",
	"OCFP",
	"LRA",
	"L0",
	"NULL",
	"BSP",
	"CFP",
	"CSP",
	"L1",
	"ALLOC",
	"K0",
	"K1",
	"L2",
	"NSP",
	"CODE",
	"LIP"
#endif
#ifdef sparc
        "ZERO",
        "ALLOC",
        "NULL",
        "CSP",
        "CFP",
        "BSP",
        "NFP",
        "CFUNC",
        "NL0",
        "NL1",
        "NL2",
        "NL3",
        "NL4",
        "NL5",
        "NSP",
        "NARGS",
        "A0",
        "A1",
        "A2",
        "A3",
        "A4",
        "A5",
        "OCFP",
        "LRA",
        "CNAME",
        "LEXENV",
        "L0",
        "L1",
        "L2",
        "CODE",
        "???",
        "LIP"
#endif
};

		
   
   
