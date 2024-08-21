/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef ALLOC_H
#define ALLOC_H

#include "lisp.h"

extern lispobj alloc_cons(lispobj car, lispobj cdr);
extern lispobj alloc_number(long n);
extern lispobj alloc_string(const char *str);
extern lispobj alloc_sap(void *ptr);

#endif /* ALLOC_H */
