/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/test.c,v 1.9 1990/10/13 04:50:03 wlott Exp $ */
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



test_init()
{
    install_handler(SIGINT, signal_handler);
    install_handler(SIGTRAP, signal_handler);
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
