/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/save.h,v 1.4 2008/12/10 16:16:11 rtoy Rel $
 */

#ifndef _SAVE_H_
#define _SAVE_H_

#include "core.h"

extern boolean save(char *filename, lispobj initfun, int sse2_mode);

#endif /* _SAVE_H_ */
