/*
 * Header file for GC
 *
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/gc.h,v 1.2 1990/03/29 21:18:26 ch Exp $
 */

#if !defined(_INCLUDED_GC_H_)
#define _INCLUDED_GC_H_

#include "lisp.h"

extern void gc_lose();

#define gc_assert(ex) { \
	if (!(ex)) { \
		fprintf(stderr, "GC invariant lost!  File \"%s\", line %d\n", \
		__FILE__, __LINE__); \
		gc_lose(); \
	} \
}

#define pointerp(object) ((object) & 0x01)

#define HEADER_VALUE(object) ((unsigned long) ((object)>>8))
#define CEILING(x,y) (((x) + ((y) - 1)) & (~((y) - 1)))
#define FIXNUM_TO_INT(x) ((x)>>2)
#define INT_TO_FIXNUM(x) ((x)<<2)

#endif
