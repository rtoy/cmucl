/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/interrupt.c,v 1.1 1990/02/24 19:37:17 wlott Exp $ */
#include <signal.h>
#include <mips/cpu.h>

#include "lisp.h"
#include "ldb.h"
#include "lispregs.h"

/* Interrupt handing magic. */

static union handler {
    lispobj lisp;
    int (*c)();
} Handler[NSIG];
      
static int pending_signal, pending_code, pending_mask;

#define BLOCKABLE (sigmask(SIGHUP)|sigmask(SIGINT)|sigmask(SIGQUIT)|sigmask(SIGPIPE)|sigmask(SIGALRM)|sigmask(SIGURG)|sigmask(SIGTSTP)|sigmask(SIGCHLD)|sigmask(SIGIO)|sigmask(SIGXCPU)|sigmask(SIGXFSZ)|sigmask(SIGVTALRM)|sigmask(SIGPROF)|sigmask(SIGWINCH)|sigmask(SIGUSR1)|sigmask(SIGUSR2))


static handle_now(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    boolean were_in_lisp = (SymbolValue(FOREIGN_FUNCTION_CALL_ACTIVE) == NIL);
    union handler handler = Handler[signal];
    lispobj args[6]; /* Six is the minimum */
    lispobj callname, function;

    if (were_in_lisp) {
        /* Get LISP state from context */
        SetSymbolValue(SAVED_ALLOCATION_POINTER, context->sc_regs[ALLOC]);
        SetSymbolValue(SAVED_CONTROL_STACK_POINTER, context->sc_regs[CSP]);
        SetSymbolValue(SAVED_BINDING_STACK_POINTER, context->sc_regs[BSP]);
        SetSymbolValue(SAVED_FLAGS_REGISTER, context->sc_regs[FLAGS]|(1<<flag_Atomic));

        /* Restore the GP */
        set_global_pointer(SymbolValue(SAVED_GLOBAL_POINTER));

        /* Push context pointer on control stack. */
        /* ### */

        SetSymbolValue(FOREIGN_FUNCTION_CALL_ACTIVE, T);
    }

    if (LowtagOf(handler.lisp) == type_EvenFixnum || LowtagOf(handler.lisp) == type_OddFixnum)
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
        SetSymbolValue(FOREIGN_FUNCTION_CALL_ACTIVE, NIL);

        /* Put ALLOC back into context. */
        context->sc_regs[ALLOC] = SymbolValue(SAVED_ALLOCATION_POINTER);
    }
}

static maybe_now_maybe_later(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    if (SymbolValue(FOREIGN_FUNCTION_CALL_ACTIVE) == NIL && context->sc_regs[FLAGS] & (1<<flag_Atomic)) {
        pending_signal = signal;
        pending_code = code;
        pending_mask = context->sc_mask;
        context->sc_mask |= BLOCKABLE;
        context->sc_regs[FLAGS] |= (1<<flag_Interrupted);
    }
    else
        handle_now(signal, code, context);
}

static segv_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
#if 0
    if (bogus_page == guard_page) {
        unprotext(guard_page);
        if (SymbolValue(FOREIGN_FUNCTION_CALL_ACTIVE) == NIL && context->sc_regs[FLAGS] & (1<<flag_Atomic)) {
            pending_signal = signal;
            pending_code = code;
            pending_mask = context->sc_mask;
            context->sc_mask |= BLOCKABLE;
            context->sc_regs[FLAGS] |= (1<<flag_Interrupted);
        }
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
