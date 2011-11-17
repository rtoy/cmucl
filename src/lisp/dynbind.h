/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/dynbind.h,v 1.3 2005/09/15 18:26:51 rtoy Rel $ */

#ifndef _DYNBIND_H_
#define _DYNBIND_H_

extern void bind_variable(lispobj symbol, lispobj value);
extern void unbind(void);
extern void unbind_to_here(lispobj * bsp);

#endif /* _DYNBIND_H_ */
