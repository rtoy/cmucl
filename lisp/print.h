/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/print.h,v 1.1 1992/07/28 20:15:19 wlott Exp $ */

#ifndef _PRINT_H_
#define _PRINT_H_

#include "lisp.h"

extern char *lowtag_Names[], *subtype_Names[];

extern void print(lispobj obj);
extern void brief_print(lispobj obj);
extern void reset_printer(void);

#endif
