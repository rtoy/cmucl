/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/arch.h,v 1.6 2002/08/27 22:18:31 moore Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef __ARCH_H__
#define __ARCH_H__

#include "os.h"
#include "signal.h"

extern char *arch_init(void);
extern void arch_skip_instruction(struct sigcontext *scp);
extern boolean arch_pseudo_atomic_atomic(struct sigcontext *scp);
extern void arch_set_pseudo_atomic_interrupted(struct sigcontext *scp);
extern os_vm_address_t arch_get_bad_addr(HANDLER_ARGS);
extern unsigned char *arch_internal_error_arguments(struct sigcontext *scp);
extern unsigned long arch_install_breakpoint(void *pc);
extern void arch_remove_breakpoint(void *pc, unsigned long orig_inst);
extern void arch_install_interrupt_handlers(void);
extern void arch_do_displaced_inst(struct sigcontext *scp,
				   unsigned long orig_inst);
extern lispobj funcall0(lispobj function);
extern lispobj funcall1(lispobj function, lispobj arg0);
extern lispobj funcall2(lispobj function, lispobj arg0, lispobj arg1);
extern lispobj funcall3(lispobj function, lispobj arg0, lispobj arg1,
			lispobj arg2);

extern void fpu_save(void *);
extern void fpu_restore(void *);

extern void arch_make_linkage_entry(long, void *, long);
extern long arch_linkage_entry(unsigned long);
void arch_make_lazy_linkage(long linkage_entry);
long arch_linkage_entry(unsigned long retaddr);

#endif /* __ARCH_H__ */
