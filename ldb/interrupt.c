/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/interrupt.c,v 1.11 1990/10/22 12:38:51 wlott Exp $ */

/* Interrupt handing magic. */

#include <signal.h>
#ifdef mips
#include <mips/cpu.h>
#endif

#include "lisp.h"
#include "ldb.h"
#include "globals.h"
#include "lispregs.h"
#include "interrupt.h"
#include "validate.h"

#define crap_out(msg) do { write(2, msg, sizeof(msg)); abort(); } while (0)

struct sigcontext *lisp_interrupt_contexts[MAX_INTERRUPTS];

union interrupt_handler interrupt_handlers[NSIG];
      
static int pending_signal = 0, pending_code = 0, pending_mask = 0;
static boolean maybe_gc_pending = FALSE;

static void fake_foreign_function_call(context)
     struct sigcontext *context;
{
    int context_index;
    lispobj oldcont;
    
    /* Get current LISP state from context */
    current_dynamic_space_free_pointer = (lispobj *) context->sc_regs[ALLOC];
    current_binding_stack_pointer = (lispobj *) context->sc_regs[BSP];
    current_flags_register = context->sc_regs[FLAGS]|(1<<flag_Atomic);
    
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

static void undo_fake_foreign_function_call(context)
     struct sigcontext *context;
{
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

static handle_now(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    int were_in_lisp;
    union interrupt_handler handler;
    lispobj *args;
    lispobj callname, function;
    long mask;
    
    handler = interrupt_handlers[signal];
    were_in_lisp = !foreign_function_call_active;
    
    if (were_in_lisp)
        fake_foreign_function_call(context);
    
    /* Allow signals again. */
    sigsetmask(context->sc_mask);
    
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
    
    if (were_in_lisp)
        undo_fake_foreign_function_call(context);
}

static maybe_now_maybe_later(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    if (SymbolValue(INTERRUPTS_ENABLED) == NIL) {
        pending_signal = signal;
        pending_code = code;
        pending_mask = context->sc_mask;
        context->sc_mask |= BLOCKABLE;
        SetSymbolValue(INTERRUPT_PENDING, T);
    } else if ((!foreign_function_call_active) &&
               (context->sc_regs[FLAGS] & (1<<flag_Atomic))) {
        pending_signal = signal;
        pending_code = code;
        pending_mask = context->sc_mask;
        context->sc_mask |= BLOCKABLE;
        context->sc_regs[FLAGS] |= (1<<flag_Interrupted);
    } else
        handle_now(signal, code, context);
}

static void skip_instruction(context)
     struct sigcontext *context;
{
    /* Skip the offending instruction */
    if (context->sc_cause & CAUSE_BD)
        emulate_branch(context, *(unsigned long *)context->sc_pc);
    else
        context->sc_pc += 4;
}

#define call_maybe_gc() \
    call_into_lisp(MAYBE_GC, SymbolFunction(MAYBE_GC), \
                   current_control_stack_pointer, 0)

static trap_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    if (code == trap_PendingInterrupt) {
        if (foreign_function_call_active)
            crap_out("Oh no, got a PendingInterrupt while foreign function call was active.\n");

        context->sc_regs[FLAGS] &= ~(1<<flag_Interrupted);
        SetSymbolValue(INTERRUPT_PENDING, NIL);
        if (maybe_gc_pending) {
            maybe_gc_pending = FALSE;
            fake_foreign_function_call(context);
            call_maybe_gc();
            undo_fake_foreign_function_call(context);
        }
        if (pending_signal) {
            signal = pending_signal;
            code = pending_code;
            context->sc_mask = pending_mask;
            pending_signal = 0;
            pending_code = 0;
            pending_mask = 0;
            handle_now(signal, code, context);
        }
        skip_instruction(context);
    }
    else
        handle_now(signal, code, context);
}

#ifdef mips
static sigfpe_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    unsigned long bad_inst;
    unsigned int op, rs, rt, rd, funct, dest;
    int immed;
    long result;

    if (context->sc_cause & CAUSE_BD)
        bad_inst = *(unsigned long *)(context->sc_pc + 4);
    else
        bad_inst = *(unsigned long *)(context->sc_pc);

    op = (bad_inst >> 26) & 0x3f;
    rs = (bad_inst >> 21) & 0x1f;
    rt = (bad_inst >> 16) & 0x1f;
    rd = (bad_inst >> 11) & 0x1f;
    funct = bad_inst & 0x3f;
    immed = (((int)(bad_inst & 0xffff)) << 16) >> 16;

#define FIXNUM_VALUE(lispobj) (((int)lispobj)>>2)

    switch (op) {
        case 0x0: /* SPECIAL */
            switch (funct) {
                case 0x20: /* ADD */
                    result = FIXNUM_VALUE(context->sc_regs[rs]) + FIXNUM_VALUE(context->sc_regs[rt]);
                    dest = rd;
                    break;

                case 0x22: /* SUB */
                    result = FIXNUM_VALUE(context->sc_regs[rs]) - FIXNUM_VALUE(context->sc_regs[rt]);
                    dest = rd;
                    break;

                default:
                    dest = 32;
                    break;
            }
            break;

        case 0x8: /* ADDI */
            result = FIXNUM_VALUE(context->sc_regs[rs]) + (immed>>2);
            dest = rt;
            break;

        default:
            dest = 32;
            break;
    }

    if (dest < 32) {
        set_global_pointer(saved_global_pointer);
        current_dynamic_space_free_pointer =
            (lispobj *) context->sc_regs[ALLOC];

        context->sc_regs[dest] = alloc_number(result);

	context->sc_regs[ALLOC] =
	    (unsigned long) current_dynamic_space_free_pointer;

        skip_instruction(context);

    }
    else
        handle_now(signal, code, context);
}
#undef FIXNUM_VALUE


static sigbus_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    lispobj *badaddr;

    badaddr = (lispobj *)context->sc_badvaddr;

    if (!foreign_function_call_active && badaddr >= current_dynamic_space && badaddr < current_dynamic_space + DYNAMIC_SPACE_SIZE) {
        set_global_pointer(saved_global_pointer);
        clear_auto_gc_trigger();

        if (context->sc_regs[FLAGS] & (1<<flag_Atomic)) {
            maybe_gc_pending = TRUE;
            context->sc_mask |= BLOCKABLE;
            context->sc_regs[FLAGS] |= (1<<flag_Interrupted);
        }
        else {
            fake_foreign_function_call(context);
            call_maybe_gc();
            undo_fake_foreign_function_call(context);
        }
    }
    else
        handle_now(signal, code, context);
}
#endif

unsigned long install_handler(signal, handler)
int signal;
union interrupt_handler handler;
{
    struct sigvec sv;
    int oldmask;
    union interrupt_handler oldhandler;

    if (signal == SIGTRAP)
        sv.sv_handler = trap_handler;
#ifdef mips
    else if (signal == SIGFPE)
        sv.sv_handler = sigfpe_handler;
    else if (signal == SIGBUS)
        sv.sv_handler = sigbus_handler;
#endif
    else if (handler.c == SIG_DFL || handler.c == SIG_IGN)
        sv.sv_handler = handler.c;
    else if (sigmask(signal)&BLOCKABLE)
        sv.sv_handler = maybe_now_maybe_later;
    else
        sv.sv_handler = handle_now;
    sv.sv_mask = BLOCKABLE;
    sv.sv_flags = 0;

    oldmask = sigblock(sigmask(signal));

    oldhandler = interrupt_handlers[signal];
    interrupt_handlers[signal] = handler;
    sigvec(signal, &sv, NULL);

    sigsetmask(oldmask);

    return (unsigned long)oldhandler.lisp;
}

interrupt_init()
{
    int i;

    for (i = 0; i < NSIG; i++)
        interrupt_handlers[i].c = SIG_DFL;
}
