/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef INTERRUPT_H
#define INTERRUPT_H

#include <signal.h>

#define MAX_INTERRUPTS (4096)

extern os_context_t *lisp_interrupt_contexts[MAX_INTERRUPTS];

union interrupt_handler {
    lispobj lisp;
    void (*c) (HANDLER_ARGS);
};

extern void interrupt_init(void);
extern void fake_foreign_function_call(os_context_t * context);
extern void undo_fake_foreign_function_call(os_context_t * context);
extern void interrupt_handle_now(HANDLER_ARGS);
extern void interrupt_handle_pending(os_context_t * scp);
extern void interrupt_internal_error(HANDLER_ARGS, boolean continuable);
extern boolean interrupt_maybe_gc(HANDLER_ARGS);
extern void interrupt_install_low_level_handler

    (int signal, void handler(HANDLER_ARGS));
extern unsigned long install_handler(int signal, void handler(HANDLER_ARGS));

extern void build_fake_control_stack_frame(os_context_t * context);
extern void interrupt_handle_space_overflow(lispobj error,

					    os_context_t * context);

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
#define FILLBLOCKSET(s) (sigaddset(s,SIGHUP), sigaddset(s,SIGINT), \
		   sigaddset(s,SIGQUIT), sigaddset(s,SIGPIPE), \
		   sigaddset(s,SIGALRM), sigaddset(s,SIGURG), \
		   sigaddset(s,SIGTSTP), sigaddset(s,SIGCHLD), \
		   sigaddset(s,SIGIO), sigaddset(s,SIGXCPU), \
                   sigaddset(s,SIGXFSZ), sigaddset(s,SIGVTALRM), \
		   sigaddset(s,SIGPROF), sigaddset(s,SIGWINCH), \
		   sigaddset(s,SIGUSR1), sigaddset(s,SIGUSR2))
#endif

#endif /* INTERRUPT_H */
