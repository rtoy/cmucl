/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/interrupt.h,v 1.5 1997/11/19 02:41:55 dtc Exp $ */

#if !defined(_INCLUDE_INTERRUPT_H_)
#define _INCLUDE_INTERRUPT_H_

#include <signal.h>

#define MAX_INTERRUPTS (4096)

extern struct sigcontext *lisp_interrupt_contexts[MAX_INTERRUPTS];

union interrupt_handler {
	lispobj lisp;
	void (*c)(HANDLER_ARGS);
};

#ifdef i386
extern void set_maybe_gc_pending(void);
#endif

extern void interrupt_init(void);
extern void fake_foreign_function_call(struct sigcontext *context);
extern void undo_fake_foreign_function_call(struct sigcontext *context);
extern void interrupt_handle_now(HANDLER_ARGS);
extern void interrupt_handle_pending(struct sigcontext *scp);
extern void interrupt_internal_error(HANDLER_ARGS, boolean continuable);
extern boolean interrupt_maybe_gc(HANDLER_ARGS);
extern void interrupt_install_low_level_handler
    (int signal,
     void handler(HANDLER_ARGS));
extern unsigned long install_handler(int signal,
				     void handler(HANDLER_ARGS));

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
#ifdef POSIX_SIGS
#define FILLBLOCKSET(s) (sigaddset(s,SIGHUP), sigaddset(s,SIGINT), \
		   sigaddset(s,SIGQUIT), sigaddset(s,SIGPIPE), \
		   sigaddset(s,SIGALRM), sigaddset(s,SIGURG), \
		   sigaddset(s,SIGTSTP), sigaddset(s,SIGCHLD), \
		   sigaddset(s,SIGIO), sigaddset(s,SIGXCPU), \
                   sigaddset(s,SIGXFSZ), sigaddset(s,SIGVTALRM), \
		   sigaddset(s,SIGPROF), sigaddset(s,SIGWINCH), \
		   sigaddset(s,SIGUSR1), sigaddset(s,SIGUSR2))
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
#endif
