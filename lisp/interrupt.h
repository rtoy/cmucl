/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/interrupt.h,v 1.3 1993/07/27 15:08:44 hallgren Exp $ */

#if !defined(_INCLUDE_INTERRUPT_H_)
#define _INCLUDE_INTERRUPT_H_

#include <signal.h>

#define MAX_INTERRUPTS (4096)

extern struct sigcontext *lisp_interrupt_contexts[MAX_INTERRUPTS];

union interrupt_handler {
	lispobj lisp;
	void (*c)(int signal, int code, struct sigcontext *scp);
};

extern void interrupt_init(void);
extern void fake_foreign_function_call(struct sigcontext *context);
extern void undo_fake_foreign_function_call(struct sigcontext *context);
extern void interrupt_handle_now(int signal, int code, struct sigcontext *scp);
extern void interrupt_handle_pending(struct sigcontext *scp);
extern void interrupt_internal_error(int signal, int code,
				     struct sigcontext *scp,
				     boolean continuable);
extern boolean interrupt_maybe_gc(int sig, int code, struct sigcontext *scp);
extern void interrupt_install_low_level_handler
    (int signal,
     void handler(int signal, int code, struct sigcontext *scp));
extern unsigned long install_handler(int signal,
				     void handler(int signal, int code,
						  struct sigcontext *handler));

extern union interrupt_handler interrupt_handlers[NSIG];

#ifdef hpux
#define BLOCKABLE (sigmask(SIGHUP) | sigmask(SIGINT) | \
		   sigmask(SIGQUIT) | sigmask(SIGPIPE) | \
		   sigmask(SIGALRM) | sigmask(SIGURG) | \
		   sigmask(SIGTSTP) | sigmask(SIGCHLD) | \
		   sigmask(SIGIO) | sigmask(SIGVTALRM) | \
		   sigmask(SIGPROF) | sigmask(SIGWINCH) | \
		   sigmask(SIGUSR1) | sigmask(SIGUSR2))
#else
#define BLOCKABLE (sigmask(SIGHUP) | sigmask(SIGINT) | \
		   sigmask(SIGQUIT) | sigmask(SIGPIPE) | \
		   sigmask(SIGALRM) | sigmask(SIGURG) | \
		   sigmask(SIGTSTP) | sigmask(SIGCHLD) | \
		   sigmask(SIGIO) | sigmask(SIGXCPU) | \
                   sigmask(SIGXFSZ) | sigmask(SIGVTALRM) | \
		   sigmask(SIGPROF) | sigmask(SIGWINCH) | \
		   sigmask(SIGUSR1) | sigmask(SIGUSR2))
#endif
#endif
