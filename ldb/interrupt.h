/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/interrupt.h,v 1.1 1990/03/29 21:23:00 ch Exp $ */

#if !defined(_INCLUDE_INTERRUPT_H_)
#define _INCLUDE_INTERRUPT_H_

#include <signal.h>

#define MAX_INTERRUPTS (4096)

extern struct sigcontext *lisp_interrupt_contexts[MAX_INTERRUPTS];

union interrupt_handler {
	lispobj lisp;
	int (*c)();
};

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
