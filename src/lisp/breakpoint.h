/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _BREAKPOINT_H_
#define _BREAKPOINT_H_

extern unsigned long breakpoint_install(lispobj code_obj, int pc_offset);
extern void breakpoint_remove(lispobj code_obj, int pc_offset,

			      unsigned long orig_inst);
extern void breakpoint_do_displaced_inst(os_context_t * scp,

					 unsigned long orig_inst);
extern void handle_breakpoint(int signal, int subcode, os_context_t * scp);
extern void *handle_function_end_breakpoint(int signal, int subcode,
					    os_context_t * scp);

#endif /* _BREAKPOINT_H_ */
