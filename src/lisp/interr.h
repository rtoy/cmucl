/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef INTERR_H
#define INTERR_H

#define crap_out(msg) do { write(2, msg, sizeof(msg)); lose(); } while (0)

extern void lose(char *fmt, ...);
extern void set_lossage_handler(void fun(void));
extern void internal_error(os_context_t * context);

extern void utf16_output(unsigned short int* utf16, int len);
extern lispobj debug_print(lispobj string);

#endif /* INTERR_H */
