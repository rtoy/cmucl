/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/purify.h,v 1.1 1992/07/28 20:15:25 wlott Exp $
 */

#if !defined(_PURIFY_H_)
#define _PURIFY_H_

extern int purify(lispobj static_roots, lispobj read_only_roots);

#endif
