/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/interrupt.c,v 1.2 1990/03/28 22:51:10 ch Exp $ */

/* Interrupt handing magic. */

#include <signal.h>
#include <mips/cpu.h>

#include "lisp.h"
#include "ldb.h"
#include "globals.h"
#include "lispregs.h"

#define MAX_INTERRUPTS (4096)

struct sigcontext interrupt_contexts[MAX_INTERRUPTS];

static union handler {
	lispobj lisp;
	int (*c)();
} Handler[NSIG];
      
static int pending_signal, pending_code, pending_mask;

#define BLOCKABLE (sigmask(SIGHUP) | sigmask(SIGINT) | \
		   sigmask(SIGQUIT) | sigmask(SIGPIPE) | \
		   sigmask(SIGALRM) | sigmask(SIGURG) | \
		   sigmask(SIGTSTP) | sigmask(SIGCHLD) | \
		   sigmask(SIGIO) | sigmask(SIGXCPU) | \
		   sigmask(SIGXFSZ) | sigmask(SIGVTALRM) | \
		   sigmask(SIGPROF) | sigmask(SIGWINCH) | \
		   sigmask(SIGUSR1) | sigmask(SIGUSR2))


static handle_now(signal, code, context)
int signal, code;
struct sigcontext *context;
{
	int were_in_lisp;
	union handler handler = Handler[signal];
	lispobj args[6];	/* Six is the minimum */
	lispobj callname, function;

	were_in_lisp = !foreign_function_call_active;

	if (were_in_lisp) {
		/* Get current LISP state from context */
		current_dynamic_space_free_pointer =
			(lispobj *) context->sc_regs[ALLOC];
		current_control_stack_pointer =
			(lispobj *) context->sc_regs[CSP];
		current_binding_stack_pointer =
			(lispobj *) context->sc_regs[BSP];
		current_flags_register = context->sc_regs[FLAGS]|(1<<flag_Atomic);

		/* Restore the GP */
		set_global_pointer(saved_global_pointer);

		foreign_function_call_active = 1;
	}

	if (LowtagOf(handler.lisp) == type_EvenFixnum ||
	    LowtagOf(handler.lisp) == type_OddFixnum)
		(*handler.c)(signal, code, context);
	else {
		args[0] = fixnum(signal);
		args[1] = fixnum(code);
		args[2] = alloc_sap(context);
		callname = handler.lisp;
		if (LowtagOf(callname) == type_FunctionPointer)
			function = callname;
		else
			function = ((struct symbol *)PTR(callname))->function;
		call_into_lisp(callname, function, args, 3);
	}

	if (were_in_lisp) {
		sigblock(BLOCKABLE);
		foreign_function_call_active = 0;

		/* Put the dynamic space free pointer back into the context. */
		context->sc_regs[ALLOC] =
			(unsigned long) current_dynamic_space_free_pointer;
	}
}

static maybe_now_maybe_later(signal, code, context)
int signal, code;
struct sigcontext *context;
{
	if ((!foreign_function_call_active) &&
	    (context->sc_regs[FLAGS] & (1<<flag_Atomic))) {
		pending_signal = signal;
		pending_code = code;
		pending_mask = context->sc_mask;
		context->sc_mask |= BLOCKABLE;
		context->sc_regs[FLAGS] |= (1<<flag_Interrupted);
	} else
		handle_now(signal, code, context);
}

static segv_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
#if 0
	if (bogus_page == guard_page) {
		unprotext(guard_page);
		if ((!foreign_function_call_active) &&
		    (context->sc_regs[FLAGS] & (1<<flag_Atomic))) {
			pending_signal = signal;
			pending_code = code;
			pending_mask = context->sc_mask;
			context->sc_mask |= BLOCKABLE;
			context->sc_regs[FLAGS] |= (1<<flag_Interrupted);
		}
		/* ### Fix this */
		SetSymbolValue(GC_TRIGGER_HIT, T);
	}
	else
#endif
		handle_now(signal, code, context);
}

static trap_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
	if (code == trap_PendingInterrupt) {
		signal = pending_signal;
		code = pending_code;
		context->sc_mask = pending_mask;
		pending_signal = 0;
		pending_code = 0;
		pending_mask = 0;
	}
	handle_now(signal, code, context);
}

static fpe_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
	switch (signal) {
        case EXC_OV:
		/* integer overflow.  Make a bignum. */
		/* For now, drop through. */

        default:
		handle_now(signal, code, context);
	}
}

void install_handler(signal, handler)
int signal;
union handler handler;
{
	struct sigvec sv;

	if (sigmask(signal)&BLOCKABLE)
		sv.sv_handler = maybe_now_maybe_later;
	else if (signal == SIGSEGV)
		sv.sv_handler = segv_handler;
	else if (signal == SIGTRAP)
		sv.sv_handler = trap_handler;
	else if (signal == SIGFPE)
		sv.sv_handler = fpe_handler;
	else
		sv.sv_handler = handle_now;
	sv.sv_mask = BLOCKABLE;
	sv.sv_flags = 0;

	Handler[signal] = handler;

	sigvec(signal, &sv, NULL);
}
