/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/interrupt.c,v 1.4 1994/07/05 16:08:03 hallgren Exp $ */

/* Interrupt handing magic. */

#include <stdio.h>

#include <signal.h>
#ifdef mach
#ifdef mips
#include <mips/cpu.h>
#endif
#endif

#include "lisp.h"
#include "internals.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "lispregs.h"
#include "interrupt.h"
#include "validate.h"
#include "monitor.h"
#include "gc.h"
#include "alloc.h"
#include "dynbind.h"
#include "interr.h"

boolean internal_errors_enabled = 0;

struct sigcontext *lisp_interrupt_contexts[MAX_INTERRUPTS];

union interrupt_handler interrupt_handlers[NSIG];
void (*interrupt_low_level_handlers[NSIG])(int signal, int code,
					   struct sigcontext *scp) = {0};

static int pending_signal = 0, pending_code = 0, pending_mask = 0;
static boolean maybe_gc_pending = FALSE;


/****************************************************************\
* Utility routines used by various signal handlers.              *
\****************************************************************/

void fake_foreign_function_call(struct sigcontext *context)
{
    int context_index;
    lispobj oldcont;
    
    /* Get current LISP state from context */
#ifdef reg_ALLOC
    current_dynamic_space_free_pointer = (lispobj *)SC_REG(context, reg_ALLOC);
#ifdef alpha
    if((long) current_dynamic_space_free_pointer & 1) {
      printf("Dead in fake_foriegn_function-call, context = %x\n",context);
      lose("");
    }
#endif
#endif
#ifdef reg_BSP
    current_binding_stack_pointer = (lispobj *)SC_REG(context, reg_BSP);
#endif
    
#ifndef i386
    /* Build a fake stack frame */
    current_control_frame_pointer = (lispobj *)SC_REG(context, reg_CSP);
    if ((lispobj *)SC_REG(context, reg_CFP)==current_control_frame_pointer) {
        /* There is a small window during call where the callee's frame */
        /* isn't built yet. */
        if (LowtagOf(SC_REG(context, reg_CODE)) == type_FunctionPointer) {
            /* We have called, but not built the new frame, so
               build it for them. */
            current_control_frame_pointer[0] = SC_REG(context, reg_OCFP);
            current_control_frame_pointer[1] = SC_REG(context, reg_LRA);
            current_control_frame_pointer += 8;
            /* Build our frame on top of it. */
            oldcont = (lispobj)SC_REG(context, reg_CFP);
        }
        else {
            /* We haven't yet called, build our frame as if the
               partial frame wasn't there. */
            oldcont = (lispobj)SC_REG(context, reg_OCFP);
        }
    }
    /* ### We can't tell if we are still in the caller if it had to
       reg_ALLOCate the stack frame due to stack arguments. */
    /* ### Can anything strange happen during return? */
    else
        /* Normal case. */
        oldcont = (lispobj)SC_REG(context, reg_CFP);
    
    current_control_stack_pointer = current_control_frame_pointer + 8;

    current_control_frame_pointer[0] = oldcont;
    current_control_frame_pointer[1] = NIL;
    current_control_frame_pointer[2] = (lispobj)SC_REG(context, reg_CODE);
#endif
    
    /* Do dynamic binding of the active interrupt context index
       and save the context in the context array. */
    context_index = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX)>>2;
    
    if (context_index >= MAX_INTERRUPTS) {
        fprintf(stderr,
		"Maximum number (%d) of interrupts exceeded.  Exiting.\n",
                MAX_INTERRUPTS);
        exit(1);
    }
    
    bind_variable(FREE_INTERRUPT_CONTEXT_INDEX,
		  make_fixnum(context_index + 1));
    
    lisp_interrupt_contexts[context_index] = context;
    
    /* No longer in Lisp now. */
    foreign_function_call_active = 1;
}

void undo_fake_foreign_function_call(struct sigcontext *context)
{
    /* Block all blockable signals */
    sigblock(BLOCKABLE);
    
    /* Going back into lisp. */
    foreign_function_call_active = 0;
    
    /* Undo dynamic binding. */
    /* ### Do I really need to unbind_to_here()? */
    unbind();
    
#ifdef reg_ALLOC
    /* Put the dynamic space free pointer back into the context. */
    SC_REG(context, reg_ALLOC) =
        (unsigned long) current_dynamic_space_free_pointer;
#endif
}

void interrupt_internal_error(int signal, int code, struct sigcontext *context,
			      boolean continuable)
{
    sigsetmask(context->sc_mask);
    fake_foreign_function_call(context);
    if (internal_errors_enabled)
	funcall2(SymbolFunction(INTERNAL_ERROR), alloc_sap(context),
		 continuable ? T : NIL);
    else
	internal_error(context);
    undo_fake_foreign_function_call(context);
    if (continuable)
	arch_skip_instruction(context);
}

void interrupt_handle_pending(struct sigcontext *context)
{
    boolean were_in_lisp = !foreign_function_call_active;

    SetSymbolValue(INTERRUPT_PENDING, NIL);

    if (maybe_gc_pending) {
	maybe_gc_pending = FALSE;
	if (were_in_lisp)
	    fake_foreign_function_call(context);
	funcall0(SymbolFunction(MAYBE_GC));
	if (were_in_lisp)
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
}


/****************************************************************\
* interrupt_handle_now, maybe_now_maybe_later                    *
*    the two main signal handlers.                               *
\****************************************************************/

void interrupt_handle_now(int signal, int code, struct sigcontext *context)
{
    int were_in_lisp;
    union interrupt_handler handler;
    
    handler = interrupt_handlers[signal];

    if(handler.c==SIG_IGN)
	return;

    were_in_lisp = !foreign_function_call_active;
    if (were_in_lisp)
        fake_foreign_function_call(context);
    
    /* Allow signals again. */
    sigsetmask(context->sc_mask);

    if (handler.c==SIG_DFL)
	/* This can happen if someone tries to ignore or default on of the */
	/* signals we need for runtime support, and the runtime support */
	/* decides to pass on it.  */
	lose("interrupt_handle_now: No handler for signal %d?\n", signal);
    else if (LowtagOf(handler.lisp) == type_FunctionPointer)
	funcall3(handler.lisp, make_fixnum(signal), make_fixnum(code),
	         alloc_sap(context));
    else
        (*handler.c)(signal, code, context);
    
    if (were_in_lisp)
        undo_fake_foreign_function_call(context);
}

static void maybe_now_maybe_later(int signal, int code,
				  struct sigcontext *context)
{
    if (SymbolValue(INTERRUPTS_ENABLED) == NIL) {
        pending_signal = signal;
        pending_code = code;
        pending_mask = context->sc_mask;
        context->sc_mask |= BLOCKABLE;
        SetSymbolValue(INTERRUPT_PENDING, T);
    } else if ((!foreign_function_call_active)
	       && arch_pseudo_atomic_atomic(context)) {
        pending_signal = signal;
        pending_code = code;
        pending_mask = context->sc_mask;
        context->sc_mask |= BLOCKABLE;
	arch_set_pseudo_atomic_interrupted(context);
    } else
        interrupt_handle_now(signal, code, context);
}

/****************************************************************\
* Stuff to detect and handle hitting the gc trigger.             *
\****************************************************************/

#ifndef INTERNAL_GC_TRIGGER
static boolean gc_trigger_hit(int signal, int code, struct sigcontext *context)
{
    if (current_auto_gc_trigger == NULL)
	return FALSE;
    else{
	lispobj *badaddr=(lispobj *)arch_get_bad_addr(signal, code, context);

	return (badaddr >= current_auto_gc_trigger &&
		badaddr < current_dynamic_space + DYNAMIC_SPACE_SIZE);
    }
}
#endif


boolean interrupt_maybe_gc(int signal, int code, struct sigcontext *context)
{
    if (!foreign_function_call_active
#ifndef INTERNAL_GC_TRIGGER
		  && gc_trigger_hit(signal, code, context)
#endif
	) {
#ifndef INTERNAL_GC_TRIGGER
	clear_auto_gc_trigger();
#endif

	if (arch_pseudo_atomic_atomic(context)) {
	    maybe_gc_pending = TRUE;
	    if (pending_signal == 0) {
		pending_mask = context->sc_mask;
		context->sc_mask |= BLOCKABLE;
	    }
	    arch_set_pseudo_atomic_interrupted(context);
	}
	else {
	    fake_foreign_function_call(context);
	    funcall0(SymbolFunction(MAYBE_GC));
	    undo_fake_foreign_function_call(context);
	}

	return TRUE;
    }else
	return FALSE;
}

/****************************************************************\
* Noise to install handlers.                                     *
\****************************************************************/

void interrupt_install_low_level_handler
    (int signal,
     void handler(int signal, int code, struct sigcontext *handler))
{
    struct sigvec sv;

    sv.sv_handler=handler;
    sv.sv_mask=BLOCKABLE;
    sv.sv_flags=0;

    sigvec(signal,&sv,NULL);

    interrupt_low_level_handlers[signal]=(handler==SIG_DFL ? 0 : handler);
}

unsigned long install_handler(int signal,
			      void handler(int signal, int code,
					   struct sigcontext *handler))
{
    struct sigvec sv;
    int oldmask;
    union interrupt_handler oldhandler;

    oldmask = sigblock(sigmask(signal));

    if(interrupt_low_level_handlers[signal]==0){
	if(handler==SIG_DFL || handler==SIG_IGN)
	    sv.sv_handler = handler;
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

void interrupt_init(void)
{
    int i;

    for (i = 0; i < NSIG; i++)
        interrupt_handlers[i].c = SIG_DFL;
}
