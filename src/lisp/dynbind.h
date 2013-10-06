/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _DYNBIND_H_
#define _DYNBIND_H_

extern void bind_variable(lispobj symbol, lispobj value);
extern void unbind(void);
extern void unbind_to_here(lispobj * bsp);

#endif /* _DYNBIND_H_ */
