/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/test.c,v 1.1 1990/02/24 19:37:31 wlott Exp $ */
/* Extra random routines for testing stuff. */

#include <signal.h>
#include <mips/cpu.h>

#include "lisp.h"
#include "ldb.h"

static char *signames[] = {
    "<Unused>", "SIGHUP", "SIGINT", "SIGQUIT", "SIGILL", "SIGTRAP",
    "SIGIOT", "SIGEMT", "SIGFPE", "SIGKILL", "SIGBUS", "SIGSEGV",
    "SIGSYS", "SIGPIPE", "SIGALRM", "SIGTERM", "SIGURG", "SIGSTOP",
    "SIGTSTP", "SIGCONT", "SIGCHLD", "SIGTTIN", "SIGTTOU", "SIGIO",
    "SIGXCPU", "SIGXFSZ", "SIGVTALRM", "SIGPROF", "SIGWINCH",
    "SIGUSR1", "SIGUSR2"
};


signal_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    int mask;
    unsigned long bad_inst;

    printf("Hit with %s, code = %d, context = 0x%x\n", signames[signal], code, context);

    if (context->sc_cause & CAUSE_BD)
        bad_inst = *(unsigned long *)(context->sc_pc + 4);
    else
        bad_inst = *(unsigned long *)(context->sc_pc);

    if ((bad_inst >> 26) == 0 && (bad_inst & 0x3f) == 0xd) {
        printf("Hit a break.  Use ``exit'' to continue.\n");
        if (context->sc_cause & CAUSE_BD)
            emulate_branch(context, *(unsigned long *)context->sc_pc);
        else
            context->sc_pc += 4;
    }

    mask = sigsetmask(0);

    monitor();

    sigsetmask(mask);
}

test_init()
{
    extern int throw_to_top(), throw_to_monitor();

    install_handler(SIGINT, signal_handler);
    install_handler(SIGQUIT, throw_to_top);
    install_handler(SIGTRAP, signal_handler);
}


cacheflush()
{
    /* This is supposed to be defined, but is not. */
}
