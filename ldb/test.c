/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/test.c,v 1.3 1990/03/19 14:21:02 wlott Exp $ */
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

static char *errors[] = ERRORS;


signal_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    int mask;
    unsigned long *ptr, bad_inst;
    char *cptr;

    printf("Hit with %s, code = %d, context = 0x%x\n", signames[signal], code, context);

    if (context->sc_cause & CAUSE_BD)
        ptr = (unsigned long *)(context->sc_pc + 4);
    else
        ptr = (unsigned long *)(context->sc_pc);
    bad_inst = *ptr;

    if ((bad_inst >> 26) == 0 && (bad_inst & 0x3f) == 0xd) {
        /* It was a break. */
        switch (code) {
            case trap_Halt:
                printf("%primitive halt called; the party is over.\n");
                break;

            case trap_PendingInterrupt:
                printf("Pending interrupt trap? This should not happen.\n");
                break;

            case trap_Error:
            case trap_Cerror:
                cptr = (char *)(ptr+1);
                printf("Error: %s\n", errors[*cptr]);
                while (*++cptr != 0)
                    printf("    R%d: 0x%x\n", *cptr, context->sc_regs[*cptr]);
                if (code == trap_Cerror) {
                    printf("Hit a break.  Use ``exit'' to continue.\n");
                    if (context->sc_cause & CAUSE_BD)
                        emulate_branch(context, *(unsigned long *)context->sc_pc);
                    else
                        context->sc_pc += 4;
                }
                break;

            default:
                printf("Unknown trap type.\n");
                break;
        }
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


lispobj print_three_nums(num1, num2, num3)
long num1, num2, num3;
{
    printf("%d\t%d\t%d\n", num1>>2, num2>>2, num3>>2);
    return NIL;
}
