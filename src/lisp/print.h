/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _PRINT_H_
#define _PRINT_H_

#include "lisp.h"

extern char *lowtag_Names[], *subtype_Names[];

extern void print(lispobj obj);
extern void brief_print(lispobj obj);
extern void reset_printer(void);

#endif /* _PRINT_H_ */
