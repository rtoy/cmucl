/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/alloc.h,v 1.2.36.1 2009/03/18 15:37:29 rtoy Exp $ */

#ifndef _ALLOC_H_
#define _ALLOC_H_

#include "lisp.h"

extern lispobj alloc_cons(lispobj car, lispobj cdr);
extern lispobj alloc_number(long n);
extern lispobj alloc_string(const char *str);
extern lispobj alloc_sap(void *ptr);

#endif /* _ALLOC_H_ */
