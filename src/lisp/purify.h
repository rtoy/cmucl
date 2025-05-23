/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef PURIFY_H
#define PURIFY_H

extern int purify(lispobj static_roots, lispobj read_only_roots);

#endif /* PURIFY_H */
