/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/interrupt.c,v 1.32 1992/03/08 18:42:36 wlott Exp $ */

/* Interrupt handing magic. */

#include <stdio.h>

#include <signal.h>
#ifdef mips
#include <mips/cpu.h>
#endif

#include "ldb.h"

#include "os.h"
#include "arch.h"
#include "lisp.h"
#include "globals.h"
#include "lispregs.h"
#include "interrupt.h"
#include "validate.h"

#define FIXNUM_VALUE(lispobj) (((int)lispobj)>>2)

boolean internal_errors_enabled = 0;

struct sigcontext *lisp_interrupt_contexts[MAX_INTERRUPTS];

union interrupt_handler interrupt_handlers[NSIG];
SIGHDLRTYPE (*interrupt_low_level_handlers[NSIG])() = {0};

static int pending_signal = 0, pending_code = 0, pending_mask = 0;
static boolean maybe_gc_pending = FALSE;


/****************************************************************\
* Utility routines used by various signal handlers.              *
\****************************************************************/

void fake_foreign_function_call(context)
     struct sigcontext *context;
{
    int context_index;
    lispobj oldcont;
    
    /* Get current LISP state from context */
#ifndef ibmrt
    current_dynamic_space_free_pointer = (lispobj *) context->sc_regs[ALLOC];
    current_binding_stack_pointer = (lispobj *) context->sc_regs[BSP];
#endif
#ifdef mips
    current_flags_register = context->sc_regs[FLAGS]|(1<<flag_Atomic);
#endif
    
    /* Build a fake stack frame */
    current_control_frame_pointer = (lispobj *) context->sc_regs[CSP];
    if ((lispobj *)context->sc_regs[CFP]==current_control_frame_pointer) {
        /* There is a small window during call where the callee's frame */
        /* isn't built yet. */
        if (LowtagOf(context->sc_regs[CODE]) == type_FunctionPointer) {
            /* We have called, but not built the new frame, so
               build it for them. */
            current_control_frame_pointer[0] = context->sc_regs[OCFP];
            current_control_frame_pointer[1] = context->sc_regs[LRA];
            current_control_frame_pointer += 8;
            /* Build our frame on top of it. */
            oldcont = (lispobj)context->sc_regs[CFP];
        }
        else {
            /* We haven't yet called, build our frame as if the
               partial frame wasn't there. */
            oldcont = (lispobj)context->sc_regs[OCFP];
        }
    }
    /* ### We can't tell if we are still in the caller if it had to
       allocate the stack frame due to stack arguments. */
    /* ### Can anything strange happen during return? */
    else
        /* Normal case. */
        oldcont = (lispobj)context->sc_regs[CFP];
    
    current_control_stack_pointer = current_control_frame_pointer + 8;

    current_control_frame_pointer[0] = oldcont;
    current_control_frame_pointer[1] = NIL;
    current_control_frame_pointer[2] = (lispobj)context->sc_regs[CODE];
    
#ifdef mips
    /* Restore the GP */
    set_global_pointer(saved_global_pointer);
#endif
    
    /* Do dynamic binding of the active interrupt context index
       and save the context in the context array. */
    context_index = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;
    
    if (context_index >= MAX_INTERRUPTS) {
        fprintf("Maximum number (%d) of interrupts exceeded.  Exiting.\n",
                MAX_INTERRUPTS);
        exit(1);
    }
    
    bind_variable(FREE_INTERRUPT_CONTEXT_INDEX, fixnum(context_index + 1));
    
    lisp_interrupt_contexts[context_index] = context;
    
    /* No longer in Lisp now. */
    foreign_function_call_active = 1;
}

void undo_fake_foreign_function_call(context)
     struct sigcontext *context;
{
    /* Block all blockable signals */
    sigblock(BLOCKABLE);
    
    /* Going back into lisp. */
    foreign_function_call_active = 0;
    
    /* Undo dynamic binding. */
    /* ### Do I really need to unbind_to_here()? */
    unbind();
    
#ifndef ibmrt
    /* Put the dynamic space free pointer back into the context. */
    context->sc_regs[ALLOC] =
        (unsigned long) current_dynamic_space_free_pointer;
#endif
}

#define call_maybe_gc() \
    funcall_sym(MAYBE_GC, current_control_stack_pointer, 0)

void interrupt_internal_error(signal, code, context, continuable)
     struct sigcontext *context;
     boolean continuable;
{
    lispobj *args;

    if (internal_errors_enabled) {
	sigsetmask(context->sc_mask);
	fake_foreign_function_call(context);
	args = current_control_stack_pointer;
	current_control_stack_pointer += 2;
	args[0] = alloc_sap(context);
	if (continuable)
	    args[1] = T;
	else
	    args[1] = NIL;
	funcall_sym(INTERNAL_ERROR, args, 2);
	undo_fake_foreign_function_call(context);
	if (continuable)
	    arch_skip_instruction(context);
    }
    else
	interrupt_handle_now(signal, code, context);
}

void interrupt_handle_pending(context)
     struct sigcontext *context;
{
    if (foreign_function_call_active)
	crap_out("Oh no, got a PendingInterrupt while foreign function call was active.\n");

#ifdef mips
    context->sc_regs[FLAGS] &= ~(1<<flag_Interrupted);
#else
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, 0);
#endif
    SetSymbolValue(INTERRUPT_PENDING, NIL);
    if (maybe_gc_pending) {
	maybe_gc_pending = FALSE;
	fake_foreign_function_call(context);
	call_maybe_gc();
	undo_fake_foreign_function_call(context);
    }
    if (pending_signal) {
	int signal, code;

	signal = pending_signal;
	code = pending_code;
	pending_signal = 0;
	pending_code = 0;
	interrupt_handle_now(signal, code, context);
    }
    context->sc_mask = pending_mask;
    pending_mask = 0;
    arch_skip_instruction(context);
}


/****************************************************************\
* interrupt_handle_now, maybe_now_maybe_later                    *
*    the two main signal handlers.                               *
\****************************************************************/

SIGHDLRTYPE interrupt_handle_now(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    int were_in_lisp;
    union interrupt_handler handler;
    lispobj *args;
    lispobj function;
    
    handler = interrupt_handlers[signal];

    if(handler.c==SIG_IGN)
	return;

    were_in_lisp = !foreign_function_call_active;
    if (were_in_lisp)
        fake_foreign_function_call(context);
    
    /* Allow signals again. */
    sigsetmask(context->sc_mask);

    if(handler.c==SIG_DFL){
	/* This can happen if someone tries to ignore or default on of the */
	/* signals we need for runtime support. */
	fprintf(stderr,"interrupt_handle_now: No handler for signal %d?\n",signal);
	printf("[exit debugger to continue]\n");
	ldb_monitor();
    }
    else if (LowtagOf(handler.lisp) == type_EvenFixnum ||
        LowtagOf(handler.lisp) == type_OddFixnum)
        (*handler.c)(signal, code, context);
    else {
        args = current_control_stack_pointer;
        current_control_stack_pointer += 3;
        args[0] = fixnum(signal);
        args[1] = fixnum(code);
        args[2] = alloc_sap(context);
	function = handler.lisp;
        call_into_lisp(function, function, args, 3);
    }
    
    if (were_in_lisp)
        undo_fake_foreign_function_call(context);
}

static SIGHDLRTYPE maybe_now_maybe_later(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    if (SymbolValue(INTERRUPTS_ENABLED) == NIL) {
        pending_signal = signal;
        pending_code = code;
        pending_mask = context->sc_mask;
        context->sc_mask |= BLOCKABLE;
        SetSymbolValue(INTERRUPT_PENDING, T);
    } else if ((!foreign_function_call_active)
#ifdef mips
               && (context->sc_regs[FLAGS] & (1<<flag_Atomic))
#else
	       && (SymbolValue(PSEUDO_ATOMIC_ATOMIC))
#endif               
               ) {
        pending_signal = signal;
        pending_code = code;
        pending_mask = context->sc_mask;
        context->sc_mask |= BLOCKABLE;
#ifdef mips
        context->sc_regs[FLAGS] |= (1<<flag_Interrupted);
#else
	SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, T);
#endif
    } else
        interrupt_handle_now(signal, code, context);
}

/****************************************************************\
* Stuff to detect and handle hitting the gc trigger.             *
\****************************************************************/

#ifndef ibmrt
static boolean gc_trigger_hit(context)
     struct sigcontext *context;
{

    if (current_auto_gc_trigger == NULL)
	return FALSE;
    else{
	lispobj *badaddr=(lispobj *)arch_get_bad_addr(context);

	return (badaddr >= current_auto_gc_trigger &&
		badaddr < current_dynamic_space + DYNAMIC_SPACE_SIZE);
    }
}
#endif


boolean interrupt_maybe_gc(context)
struct sigcontext *context;
{
    if (!foreign_function_call_active
#ifndef ibmrt
		  && gc_trigger_hit(context)
#endif
	) {
#ifdef mips
	set_global_pointer(saved_global_pointer);
#endif
#ifndef ibmrt
	clear_auto_gc_trigger();
#endif

	if (
#ifdef mips
	    context->sc_regs[FLAGS] & (1<<flag_Atomic)
#else
	    SymbolValue(PSEUDO_ATOMIC_ATOMIC)
#endif
	    ) {
	    maybe_gc_pending = TRUE;
	    if (pending_signal == 0) {
		pending_mask = context->sc_mask;
		context->sc_mask |= BLOCKABLE;
	    }
#ifdef mips
	    context->sc_regs[FLAGS] |= (1<<flag_Interrupted);
#else
	    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, T);
#endif
	}
	else {
	    fake_foreign_function_call(context);
	    call_maybe_gc();
	    undo_fake_foreign_function_call(context);
	}

	return TRUE;
    }else
	return FALSE;
}

/****************************************************************\
* Noise to install handlers.                                     *
\****************************************************************/

void interrupt_install_low_level_handler(signal,handler)
int signal;
SIGHDLRTYPE (*handler)();
{
    struct sigvec sv;

    sv.sv_handler=handler;
    sv.sv_mask=BLOCKABLE;
    sv.sv_flags=0;

    sigvec(signal,&sv,NULL);

    interrupt_low_level_handlers[signal]=(handler==SIG_DFL ? 0 : handler);
}

unsigned long install_handler(signal, handler)
int signal;
SIGHDLRTYPE (*handler)();
{
    struct sigvec sv;
    int oldmask;
    union interrupt_handler oldhandler;

    oldmask = sigblock(sigmask(signal));

    if(interrupt_low_level_handlers[signal]==0){
	if(handler==SIG_DFL || handler==SIG_IGN)
	    sv.sv_handler=handler;
	else if (sigmask(signal)&BLOCKABLE)
	    sv.sv_handler = maybe_now_maybe_later;
	else
	    sv.sv_handler = interrupt_handle_now;

	sv.sv_mask = BLOCKABLE;
	sv.sv_flags = 0;

	sigvec(signal, &sv, NULL);
    }

    oldhandler = interrupt_handlers[signal];
    interrupt_handlers[signal].c = handler;

    sigsetmask(oldmask);

    return (unsigned long)oldhandler.lisp;
}

interrupt_init()
{
    int i;

    for (i = 0; i < NSIG; i++)
        interrupt_handlers[i].c = SIG_DFL;
}
