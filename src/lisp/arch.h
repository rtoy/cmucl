/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef __ARCH_H__
#define __ARCH_H__

#include "os.h"
#include "signal.h"

extern char *arch_init(fpu_mode_t);

/*
 * Skip over the trap instructions for an error trap and also skip
 * over anly following bytes used to encode information for an
 * error-trap or cerror-trap.  The PC in the context is set to address
 * just past the trap instruction and data bytes (if any).
 */
extern void arch_skip_instruction(os_context_t * scp);

extern boolean arch_pseudo_atomic_atomic(os_context_t * scp);
extern void arch_set_pseudo_atomic_interrupted(os_context_t * scp);
extern os_vm_address_t arch_get_bad_addr(HANDLER_ARGS);
extern unsigned char *arch_internal_error_arguments(os_context_t * scp);

/*
 * Install an architecture-dependent breakpoint instruction at the
 * given PC address.  This also returns the bytes that were
 * overwritten by the breakpoint instruction so that the original
 * instruction can be restored once the breakpoint has been handled.
 */
extern unsigned long arch_install_breakpoint(void *pc);

extern void arch_remove_breakpoint(void *pc, unsigned long orig_inst);
extern void arch_install_interrupt_handlers(void);

/*
 * This is called when we need to continue after a breakpoint.  The
 * original instruction in |orig_inst| is put back.  Then things are
 * set up so that we can run again and after this instruction is run,
 * we trap again so that the original breakpoint can be replaced.  How
 * this is done is architecture-dependent.
 */
extern void arch_do_displaced_inst(os_context_t * scp, unsigned long orig_inst);

extern lispobj funcall0(lispobj function);
extern lispobj funcall1(lispobj function, lispobj arg0);
extern lispobj funcall2(lispobj function, lispobj arg0, lispobj arg1);
extern lispobj funcall3(lispobj function, lispobj arg0, lispobj arg1,
			lispobj arg2);

extern void arch_make_linkage_entry(long, void *, long);
extern long arch_linkage_entry(unsigned long);
void arch_make_lazy_linkage(long linkage_entry);
long arch_linkage_entry(unsigned long retaddr);


extern void fpu_save(void *);
extern void fpu_restore(void *);
extern void sse_save(void *);
extern void sse_restore(void *);
extern void save_fpu_state(void*);
extern void restore_fpu_state(void*);

/*
 * Set to non-zero to enable debug prints for debugging the sigill and
 * sigtrap handlers and for debugging breakpoints.
 */
extern unsigned int debug_handlers;

#if defined(i386) || defined(__x86_64)
#include "x86-arch.h"
#endif

#if defined(DARWIN) && defined(__ppc__)
#include "ppc-arch.h"
#endif

#if defined(sparc)
#include "sparc-arch.h"
#endif
#endif /* __ARCH_H__ */
