/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/dynbind.h,v 1.1 1992/07/28 20:14:24 wlott Exp $ */

#ifndef _DYNBIND_H_
#define _DYNBIND_H_

extern void bind_variable(lispobj symbol, lispobj value);
extern void unbind(void);
extern void unbind_to_here(lispobj *bsp);

#endif
