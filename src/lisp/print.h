/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef PRINT_H
#define PRINT_H

#include "lisp.h"

extern char *lowtag_Names[], *subtype_Names[];

extern void print(lispobj obj);
extern void brief_print(lispobj obj);
extern void reset_printer(void);

#endif /* PRINT_H */
