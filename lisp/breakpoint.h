/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/breakpoint.h,v 1.1 1992/07/28 20:14:15 wlott Exp $
 */

#ifndef _BREAKPOINT_H_
#define _BREAKPOINT_H_

extern unsigned long breakpoint_install(lispobj code_obj, int pc_offset);
extern void breakpoint_remove(lispobj code_obj, int pc_offset,
			      unsigned long orig_inst);
extern void breakpoint_do_displaced_inst(struct sigcontext *scp,
					 unsigned long orig_inst);
extern void handle_breakpoint(int signal, int subcode, struct sigcontext *scp);
extern void *handle_function_end_breakpoint(int signal, int subcode,
					    struct sigcontext *scp);

#endif
