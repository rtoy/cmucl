/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _SEARCH_H_
#define _SEARCH_H_

extern boolean search_for_type(int type, lispobj ** start, int *count);
extern boolean search_for_symbol(char *name, lispobj ** start, int *count);

#endif
