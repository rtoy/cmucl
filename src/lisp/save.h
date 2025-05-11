/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef SAVE_H
#define SAVE_H

#include "core.h"

extern boolean save(char *filename, lispobj initfun, int sse2_mode);

#endif /* SAVE_H */
