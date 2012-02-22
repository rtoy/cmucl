/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/print.h,v 1.2 2005/01/13 19:55:00 fgilham Rel $ */

#ifndef _PRINT_H_
#define _PRINT_H_

#include "lisp.h"

extern char *lowtag_Names[], *subtype_Names[];

extern void print(lispobj obj);
extern void brief_print(lispobj obj);
extern void reset_printer(void);

#endif /* _PRINT_H_ */
