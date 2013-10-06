/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _SAVE_H_
#define _SAVE_H_

#include "core.h"

extern boolean save(char *filename, lispobj initfun, int sse2_mode);

#endif /* _SAVE_H_ */
