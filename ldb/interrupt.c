/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/interrupt.c,v 1.23 1991/02/16 01:00:26 wlott Exp $ */

/* Interrupt handing magic. */

#include <signal.h>
#ifdef mips
#include <mips/cpu.h>
#endif
#ifdef sparc
#include <machine/trap.h>
#endif

#include "lisp.h"
#include "ldb.h"
#include "globals.h"
#include "lispregs.h"
#include "interrupt.h"
#include "validate.h"

#define crap_out(msg) do { write(2, msg, sizeof(msg)); abort(); } while (0)

#define FIXNUM_VALUE(lispobj) (((int)lispobj)>>2)

boolean internal_errors_enabled = 0;

struct sigcontext *lisp_interrupt_contexts[MAX_INTERRUPTS];

union interrupt_handler interrupt_handlers[NSIG];
      
static int pending_signal = 0, pending_code = 0, pending_mask = 0;
static boolean maybe_gc_pending = FALSE;


/****************************************************************\
* Utility routines used by various signal handlers.              *
\****************************************************************/

static void fake_foreign_function_call(context)
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
    
#ifndef ibmrt
    /* Put the dynamic space free pointer back into the context. */
    context->sc_regs[ALLOC] =
        (unsigned long) current_dynamic_space_free_pointer;
#endif
}

#define call_maybe_gc() \
    call_into_lisp(MAYBE_GC, SymbolFunction(MAYBE_GC), \
                   current_control_stack_pointer, 0)

static void skip_instruction(context)
     struct sigcontext *context;
{
#ifdef mips
    /* Skip the offending instruction */
    if (context->sc_cause & CAUSE_BD)
        emulate_branch(context, *(unsigned long *)context->sc_pc);
    else
        context->sc_pc += 4;
#endif
#ifdef sparc
    context->sc_pc = context->sc_npc;
    context->sc_npc += 4;
#endif
}

static void internal_error(signal, code, context, continuable)
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
	call_into_lisp(INTERNAL_ERROR, SymbolFunction(INTERNAL_ERROR),
		       args, 2);
	undo_fake_foreign_function_call(context);
	if (continuable)
	    skip_instruction(context);
    }
    else
	handle_now(signal, code, context);
}

static void handle_pending_interrupt(context)
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
	handle_now(signal, code, context);
    }
    context->sc_mask = pending_mask;
    pending_mask = 0;
    skip_instruction(context);
}


/****************************************************************\
* handle_now, maybe_now_maybe_later                              *
*    the two main signal handlers.                               *
\****************************************************************/

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
    
    if (handler.c == SIG_DFL || handler.c == SIG_IGN)
	/* This can happen if someone tries to ignore or default on of the */
	/* signals we need for runtime support. */
	return;

    if (were_in_lisp)
        fake_foreign_function_call(context);
    
    /* Allow signals again. */
    sigsetmask(context->sc_mask);

    write(1, "Calling handler.\n", 17);

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
        handle_now(signal, code, context);
}

/****************************************************************\
* PMAX specific signal handlers.                                 *
\****************************************************************/

#ifdef mips
static sigtrap_handler(signal, code, context)
     int signal, code;
     struct sigcontext *context;
{
    switch (code) {
      case trap_PendingInterrupt:
	handle_pending_interrupt(context);
	break;

      case trap_Halt:
	crap_out("%primitive halt called; the party is over.\n");

      case trap_Error:
      case trap_Cerror:
	internal_error(signal, code, context, code==trap_Cerror);
	break;

      default:
	handle_now(signal, code, context);
	break;
    }
}

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
#endif

/****************************************************************\
* SPARC specific signal handlers.                                *
\****************************************************************/

#ifdef sparc
static sigill_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    int badinst;

    if (code == T_UNIMP_INSTR) {
	int trap;

	trap = *(unsigned long *)(context->sc_pc) & 0x3fffff;

	switch (trap) {
	  case trap_PendingInterrupt:
	    handle_pending_interrupt(context);
	    break;

	  case trap_Halt:
	    crap_out("%primitive halt called; the party is over.\n");

	  case trap_Error:
	  case trap_Cerror:
	    internal_error(signal, code, context, trap == trap_Cerror);
	    break;

	  default:
	    handle_now(signal, code, context);
	    break;
	}
    }
    else if (code >= T_SOFTWARE_TRAP + 16 & code < T_SOFTWARE_TRAP + 32)
	internal_error(signal, code, context, FALSE);
    else
	handle_now(signal, code, context);
}

static sigemt_handler(signal, code, context)
     int signal, code;
     struct sigcontext *context;
{
    unsigned long badinst;
    boolean subtract, immed;
    int rd, rs1, op1, rs2, op2, result;

    badinst = *(unsigned long *)context->sc_pc;
    if ((badinst >> 30) != 2 || ((badinst >> 20) & 0x1f) != 0x11) {
	/* It wasn't a tagged add.  Pass the signal into lisp. */
	handle_now(signal, code, context);
	return;
    }
	
    /* Extract the parts of the inst. */
    subtract = badinst & (1<<19);
    rs1 = (badinst>>14) & 0x1f;
    op1 = context->sc_regs[rs1];
    if ((op1 & 3) != 0) {
	/* The first arg wan't a fixnum. */
	internal_error(signal, code, context, FALSE);
	return;
    }

    if (immed = badinst & (1<<13)) {
	op2 = badinst & 0x1fff;
	if (op2 & (1<<12))
	    op2 |= -1<<13;
    }
    else {
	rs2 = badinst & 0x1f;
	op2 = context->sc_regs[rs2];
    }

    if ((op2 & 3) != 0) {
	/* The second arg wan't a fixnum. */
	internal_error(signal, code, context, FALSE);
	return;
    }

    rd = (badinst>>25) & 0x1f;
    if (rd != 0) {
	/* Don't bother computing the result unless we are going to use it. */
	if (subtract)
	    result = (op1>>2) - (op2>>2);
	else
	    result = (op1>>2) + (op2>>2);

        current_dynamic_space_free_pointer =
            (lispobj *) context->sc_regs[ALLOC];

	context->sc_regs[rd] = alloc_number(result);

	context->sc_regs[ALLOC] =
	    (unsigned long) current_dynamic_space_free_pointer;
    }
    skip_instruction(context);
}
#endif

/****************************************************************\
* Stuff to detect and handle hitting the gc trigger.             *
\****************************************************************/

static boolean gc_trigger_hit(context)
     struct sigcontext *context;
{
    lispobj *badaddr;
#ifdef sparc
    unsigned long badinst;
    int rs1;
#endif

    if (current_auto_gc_trigger == NULL)
	return FALSE;

#ifdef mips
    /* Finding the bad address on the mips is easy. */
    badaddr = (lispobj *)context->sc_badvaddr;
#endif

#ifdef sparc
    /* On the sparc, we have to decode the instruction. */

    /* Make sure it's not the pc thats bogus, and that it was lisp code */
    /* that caused the fault. */
    if ((context->sc_pc & 3) != 0 ||
	((context->sc_pc < READ_ONLY_SPACE_START ||
	  context->sc_pc >= READ_ONLY_SPACE_START+READ_ONLY_SPACE_SIZE) &&
	 ((lispobj *)context->sc_pc < current_dynamic_space &&
	  (lispobj *)context->sc_pc >=
	      current_dynamic_space + DYNAMIC_SPACE_SIZE)))
	return FALSE;

    badinst = *(unsigned long *)context->sc_pc;

    if ((badinst >> 30) != 3)
	/* All load/store instructions have op = 11 (binary) */
	return FALSE;

    rs1 = (badinst>>14)&0x1f;

    if (badinst & (1<<13)) {
	/* r[rs1] + simm(13) */
	int simm13 = badinst & 0x1fff;

	if (simm13 & (1<<12))
	    simm13 |= -1<<13;

	badaddr = (lispobj *)(context->sc_regs[rs1] + simm13);
    }
    else {
	/* r[rs1] + r[rs2] */
	int rs2 = badinst & 0x1f;

	badaddr = (lispobj *)(context->sc_regs[rs1] + context->sc_regs[rs2]);
    }
#endif

#ifdef ibmrt
    /* ### Don't know where to look on the RT. */
    badaddr = NULL;
#endif

    return (badaddr >= current_auto_gc_trigger &&
	    badaddr < current_dynamic_space + DYNAMIC_SPACE_SIZE);
}

static sigbus_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    if (!foreign_function_call_active && gc_trigger_hit(context)) {
#ifdef mips
	set_global_pointer(saved_global_pointer);
#endif
	clear_auto_gc_trigger();

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
    }
    else
	handle_now(signal, code, context);
}

/****************************************************************\
* Noise to install handlers.                                     *
\****************************************************************/

unsigned long install_handler(signal, handler)
int signal;
int (*handler)();
{
    struct sigvec sv;
    int oldmask;
    union interrupt_handler oldhandler;

#ifdef mips
    if (signal == SIGTRAP)
        sv.sv_handler = sigtrap_handler;
    else if (signal == SIGFPE)
        sv.sv_handler = sigfpe_handler;
    else if (signal == SIGBUS || signal == SIGSEGV)
        sv.sv_handler = sigbus_handler;
#endif
#ifdef sparc
    if (signal == SIGILL)
	sv.sv_handler = sigill_handler;
    else if (signal == SIGEMT)
	sv.sv_handler = sigemt_handler;
    else if (signal == SIGBUS)
        sv.sv_handler = sigbus_handler;
#endif
#ifdef ibmrt
    if (0)
	;
#endif
    else if (handler == SIG_DFL || handler == SIG_IGN)
        sv.sv_handler = handler;
    else if (sigmask(signal)&BLOCKABLE)
        sv.sv_handler = maybe_now_maybe_later;
    else
        sv.sv_handler = handle_now;
    sv.sv_mask = BLOCKABLE;
    sv.sv_flags = 0;

    oldmask = sigblock(sigmask(signal));

    oldhandler = interrupt_handlers[signal];
    interrupt_handlers[signal].c = handler;
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
