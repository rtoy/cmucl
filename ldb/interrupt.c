/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/interrupt.c,v 1.5 1990/05/26 01:20:17 ch Exp $ */

/* Interrupt handing magic. */

#include <signal.h>
#include <mips/cpu.h>

#include "lisp.h"
#include "ldb.h"
#include "globals.h"
#include "lispregs.h"
#include "interrupt.h"

struct sigcontext *lisp_interrupt_contexts[MAX_INTERRUPTS];

union interrupt_handler interrupt_handlers[NSIG];
      
static int pending_signal, pending_code, pending_mask;


static handle_now(signal, code, context)
int signal, code;
struct sigcontext *context;
{
	int were_in_lisp;
	union interrupt_handler handler;
	lispobj *args;
	lispobj callname, function;

	handler = interrupt_handlers[signal];
	were_in_lisp = !foreign_function_call_active;

	if (were_in_lisp) {
		int context_index;

		/* Get current LISP state from context */
		current_dynamic_space_free_pointer =
			(lispobj *) context->sc_regs[ALLOC];
		current_binding_stack_pointer =
			(lispobj *) context->sc_regs[BSP];
		current_flags_register = context->sc_regs[FLAGS]|(1<<flag_Atomic);

                /* Build a fake stack frame */
                current_control_frame_pointer =
                        (lispobj *) context->sc_regs[CSP];
                current_control_stack_pointer = 
                        current_control_frame_pointer + 8;
                current_control_frame_pointer[0] =
                        context->sc_regs[CONT];
                current_control_frame_pointer[1] = 
                        context->sc_regs[CODE];

		/* Restore the GP */
		set_global_pointer(saved_global_pointer);

		/* Do dynamic binding of the active interrupt context index
		   and save the context in the context array. */
		context_index = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;
		
		if (context_index >= MAX_INTERRUPTS) {
			fprintf("Maximum number (%d) of interrupts exceeded.  Exiting.\n",
				MAX_INTERRUPTS);
			exit(1);
		}

		bind_variable(FREE_INTERRUPT_CONTEXT_INDEX,
			      fixnum(context_index + 1));

		lisp_interrupt_contexts[context_index] = context;

		/* No longer in Lisp now. */
		foreign_function_call_active = 1;
	}

	if (LowtagOf(handler.lisp) == type_EvenFixnum ||
	    LowtagOf(handler.lisp) == type_OddFixnum)
		(*handler.c)(signal, code, context);
	else {
                args = current_control_stack_pointer;
                current_control_stack_pointer += 3;
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
		int context_index;

		/* Block all blockable signals */
		sigblock(BLOCKABLE);

		/* Going back into lisp. */
		foreign_function_call_active = 0;

		/* Undo dynamic binding. */
		/* ### Do I really need to unbind_to_here()? */
		unbind();

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

void install_handler(signal, handler)
int signal;
union interrupt_handler handler;
{
	struct sigvec sv;

	if (sigmask(signal)&BLOCKABLE)
		sv.sv_handler = maybe_now_maybe_later;
	else if (signal == SIGTRAP)
		sv.sv_handler = trap_handler;
	else
		sv.sv_handler = handle_now;
	sv.sv_mask = BLOCKABLE;
	sv.sv_flags = 0;

	interrupt_handlers[signal] = handler;

	sigvec(signal, &sv, NULL);
}

void unistall_handler(signal)
int signal;
{

}

interrupt_init()
{
	int i;

	for (i = 0; i < NSIG; i++)
		interrupt_handlers[i].lisp = (lispobj) fixnum(0);
}
