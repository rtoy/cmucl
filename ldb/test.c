/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/test.c,v 1.8 1990/09/08 10:59:34 wlott Exp $ */
/* Extra random routines for testing stuff. */

#include <signal.h>
#ifdef mips
#include <mips/cpu.h>
#endif

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
    unsigned char *cptr;

    printf("Hit with %s, code = %d, context = 0x%x\n", signames[signal], code, context);

#ifdef mips
    if (context->sc_cause & CAUSE_BD)
        ptr = (unsigned long *)(context->sc_pc + 4);
    else
        ptr = (unsigned long *)(context->sc_pc);
    bad_inst = *ptr;

    if ((bad_inst >> 26) == 0 && (bad_inst & 0x3f) == 0xd) {
        /* It was a break. */
        switch ((bad_inst >> 16) & 0x3ff) {
            case trap_Halt:
                printf("%primitive halt called; the party is over.\n");
                break;

            case trap_PendingInterrupt:
                printf("Pending interrupt trap? This should not happen.\n");
                break;

            case trap_Error:
            case trap_Cerror:
                cptr = (unsigned char *)(ptr+1);
                if (*cptr == 0xff) {
                    int len, scoffset, sc, offset, ch;

                    cptr++;
                    len = *cptr++;
                    printf("Error: %s\n", errors[*cptr++]);
                    len--;
                    while (len > 0) {
                        scoffset = *cptr++;
                        len--;
                        if (scoffset == 253) {
                            scoffset = *cptr++;
                            len--;
                        }
                        else if (scoffset == 254) {
                            scoffset = cptr[0] + cptr[1]*256;
                            cptr += 2;
                            len -= 2;
                        }
                        else if (scoffset == 255) {
                            scoffset = cptr[0] + (cptr[1]<<8) +
                                (cptr[2]<<16) + (cptr[3]<<24);
                            cptr += 4;
                            len -= 4;
                        }
                        sc = scoffset & 0x1f;
                        offset = scoffset >> 5;

                        printf("    SC: %d, Offset: %d", sc, offset);
                        switch (sc) {
                          case 16: /* Any reg */
                          case 17: /* Descriptor reg */
                            putchar('\t');
                            brief_print(context->sc_regs[offset]);
                            break;
                          case 18: /* base char reg */
                            ch = context->sc_regs[offset];
                            switch (ch) {
                              case '\n': printf("\t'\\n'\n"); break;
                              case '\b': printf("\t'\\b'\n"); break;
                              case '\t': printf("\t'\\t'\n"); break;
                              case '\r': printf("\t'\\r'\n"); break;
                              default:
                                if (ch < 32 || ch > 127)
                                    printf("\\%03o", ch);
                                else
                                    printf("\t'%c'\n", ch);
                                break;
                            }
                            break;
                          case 19: /* sap reg */
                            printf("\t0x%08x\n", context->sc_regs[offset]);
                            break;
                          case 20: /* signed reg */
                            printf("\t%ld\n", context->sc_regs[offset]);
                            break;
                          case 21: /* unsigned reg */
                            printf("\t%lu\n", context->sc_regs[offset]);
                            break;
                          case 22: /* non-descr reg */
                          case 23: /* interior reg */
                            printf("\t???\n");
                            break;
                          case 24: /* single-float reg */
                            printf("\t%g\n",
                                   *(float *)&context->sc_fpregs[offset]);
                            break;
                          case 25: /* double-float reg */
                            printf("\t%g\n",
                                   *(double *)&context->sc_fpregs[offset]);
                            break;
                          default:
                            printf("\t???\n");
                            break;
                        }
                    }
                }
                else {
                    printf("Error: %s\n", errors[*cptr]);
                    while (*++cptr != 0)
                        printf("    R%d: 0x%x\n", *cptr,
                               context->sc_regs[*cptr]);
                }
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
#endif

    mask = sigsetmask(0);

    ldb_monitor();

    sigsetmask(mask);
}



#define FIXNUM_VALUE(lispobj) (((int)lispobj)>>2)

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
#endif



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
    extern int throw_to_monitor();

    install_handler(SIGINT, signal_handler);
    install_handler(SIGTRAP, signal_handler);
#ifdef mips
    install_handler(SIGFPE, sigfpe_handler);
#endif
}


#ifdef mips
cacheflush()
{
    /* This is supposed to be defined, but is not. */
}
#endif

lispobj debug_print(string)
lispobj string;
{
    printf("%s\n", ((struct vector *)PTR(string))->data);

    return NIL;
}
