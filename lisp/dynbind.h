/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/dynbind.h,v 1.2 2005/01/13 19:55:00 fgilham Exp $ */

#ifndef _DYNBIND_H_
#define _DYNBIND_H_

extern void bind_variable(lispobj symbol, lispobj value);
extern void unbind(void);
extern void unbind_to_here(lispobj *bsp);

#endif /* _DYNBIND_H_ */
