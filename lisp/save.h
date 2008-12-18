/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/save.h,v 1.3.18.1 2008/12/18 21:50:19 rtoy Exp $
 */

#ifndef _SAVE_H_
#define _SAVE_H_

#include "core.h"

extern boolean save(char *filename, lispobj initfun, int sse2_mode);

#endif /* _SAVE_H_ */
