/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/test.c,v 1.5 1990/05/26 01:23:30 ch Exp $ */
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



#define FIXNUM_VALUE(lispobj) (((int)lispobj)>>2)

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
		    signal_handler(signal, code, context);
		    return;
            }
            break;

        case 0x8: /* ADDI */
            result = FIXNUM_VALUE(context->sc_regs[rs]) + (immed>>2);
            dest = rt;
            break;

        default:
	    signal_handler(signal, code, context);
	    return;
    }

    context->sc_regs[dest] = alloc_number(result);

    /* Skip the offending instruction */
    if (context->sc_cause & CAUSE_BD)
        emulate_branch(context, *(unsigned long *)context->sc_pc);
    else
        context->sc_pc += 4;
}



static sigsegv_handler(signal, code, context)
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
}



test_init()
{
    extern int throw_to_top(), throw_to_monitor();

    install_handler(SIGINT, signal_handler);
    install_handler(SIGQUIT, throw_to_top);
    install_handler(SIGTRAP, signal_handler);
    install_handler(SIGFPE, sigfpe_handler);
}


cacheflush()
{
    /* This is supposed to be defined, but is not. */
}


lispobj debug_print(string)
lispobj string;
{
    printf("%s\n", ((struct vector *)PTR(string))->data);

    return NIL;
}
