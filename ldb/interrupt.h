/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/interrupt.h,v 1.3 1991/09/27 10:32:30 wlott Exp $ */

#if !defined(_INCLUDE_INTERRUPT_H_)
#define _INCLUDE_INTERRUPT_H_

#include <signal.h>

#define MAX_INTERRUPTS (4096)

extern struct sigcontext *lisp_interrupt_contexts[MAX_INTERRUPTS];

#ifdef SUNOS
#define SIGHDLRTYPE void
#else
#define SIGHDLRTYPE int
#endif

union interrupt_handler {
	lispobj lisp;
	SIGHDLRTYPE (*c)();
};

extern SIGHDLRTYPE interrupt_handle_now();
extern void interrupt_handle_pending();
extern void interrupt_internal_error();

extern union interrupt_handler interrupt_handlers[NSIG];

#define BLOCKABLE (sigmask(SIGHUP) | sigmask(SIGINT) | \
		   sigmask(SIGQUIT) | sigmask(SIGPIPE) | \
		   sigmask(SIGALRM) | sigmask(SIGURG) | \
		   sigmask(SIGTSTP) | sigmask(SIGCHLD) | \
		   sigmask(SIGIO) | sigmask(SIGXCPU) | \
		   sigmask(SIGXFSZ) | sigmask(SIGVTALRM) | \
		   sigmask(SIGPROF) | sigmask(SIGWINCH) | \
		   sigmask(SIGUSR1) | sigmask(SIGUSR2))

#endif
