/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/test.c,v 1.11 1991/02/16 01:01:36 wlott Exp $ */
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

#ifdef mips
#define any_reg_sc 16
#define descriptor_reg_sc 17
#define base_char_reg_sc 18
#define sap_reg_sc 19
#define signed_reg_sc 20
#define unsigned_reg_sc 21
#define non_descr_reg_sc 22
#define interior_reg_sc 23
#define single_float_reg_sc 24
#define double_float_reg_sc 25
#endif

#ifdef sparc
#define any_reg_sc 11
#define descriptor_reg_sc 12
#define base_char_reg_sc 13
#define sap_reg_sc 14
#define signed_reg_sc 15
#define unsigned_reg_sc 16
#define non_descr_reg_sc 17
#define interior_reg_sc 18
#define single_float_reg_sc 19
#define double_float_reg_sc 20
#endif

#ifdef ibmrt
#define any_reg_sc 10
#define descriptor_reg_sc 11
#define base_char_reg_sc 12
#define sap_reg_sc 13
#define signed_reg_sc 14
#define unsigned_reg_sc 15
#define non_descr_reg_sc 16
#define word_pointer_reg_sc 17
#define interior_reg_sc 18
#define single_68881_reg_sc 19
#define double_68881_reg_sc 20
#define single_fpa_reg_sc 21
#define double_fpa_reg_sc 22
#define single_afpa_reg_sc 23
#define double_afpa_reg_sc 24
#endif

signal_handler(signal, code, context)
int signal, code;
struct sigcontext *context;
{
    int mask;
    unsigned long *ptr, bad_inst;
    unsigned char *cptr;
    int len, scoffset, sc, offset, ch;

    printf("Hit with %s, code = %d, context = 0x%x\n", signames[signal], code, context);

#ifdef mips
    if (context->sc_cause & CAUSE_BD)
        ptr = (unsigned long *)(context->sc_pc + 4);
    else
        ptr = (unsigned long *)(context->sc_pc);
#else
    ptr = (unsigned long *)(context->sc_pc);
#endif

    bad_inst = *ptr;

#ifdef mips
    if ((bad_inst >> 26) == 0 && (bad_inst & 0x3f) == 0xd) {
        /* It was a break. */
        switch ((bad_inst >> 16) & 0x3ff) {
#if 0
        }
    }
#endif
#endif
#ifdef sparc
    if ((bad_inst & 0xc1c00000) == 0) {
        switch (bad_inst & 0x3fffff) {
#if 0
        }
    }
#endif
#endif
#ifdef ibmrt
    if ((bad_inst & 0xffff0000) == 0xcc700000) {
	switch (bad_inst & 0xffff) {
#if 0
	}
    }
#endif
#endif
#if !defined(mips) && !defined(sparc) && !defined(ibmrt)
    if (0) {
	switch (0) {
#endif
            case trap_Halt:
                printf("%primitive halt called; the party is over.\n");
                break;

            case trap_PendingInterrupt:
                printf("Pending interrupt trap? This should not happen.\n");
                break;

            case trap_Error:
            case trap_Cerror:
                cptr = (unsigned char *)(ptr+1);
                if (*cptr == 0xff)
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
                      case any_reg_sc:
                      case descriptor_reg_sc:
                        putchar('\t');
                        brief_print(context->sc_regs[offset]);
                        break;
                      case base_char_reg_sc:
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
                      case sap_reg_sc:
#ifdef ibmrt
		      case word_pointer_reg_sc:
#endif
                        printf("\t0x%08x\n", context->sc_regs[offset]);
                        break;
                      case signed_reg_sc:
                        printf("\t%ld\n", context->sc_regs[offset]);
                        break;
                      case unsigned_reg_sc:
                        printf("\t%lu\n", context->sc_regs[offset]);
                        break;
                      case non_descr_reg_sc:
                      case interior_reg_sc:
                        printf("\t???\n");
                        break;
#ifndef ibmrt
                      case single_float_reg_sc:
                        printf("\t%g\n",
                               *(float *)&context->sc_fpregs[offset]);
                        break;
                      case double_float_reg_sc:
                        printf("\t%g\n",
                               *(double *)&context->sc_fpregs[offset]);
                        break;
#endif
                      default:
                        printf("\t???\n");
                        break;
                    }
                }
                if (code == trap_Cerror) {
                    printf("Hit a break.  Use ``exit'' to continue.\n");
#ifdef mips
                    if (context->sc_cause & CAUSE_BD)
                        emulate_branch(context, *(unsigned long *)context->sc_pc);
                    else
                        context->sc_pc += 4;
#endif
#ifdef sparc
                    context->sc_pc = context->sc_npc;
                    context->sc_npc = context->sc_npc + 4;
#endif
                }
                break;

            default:
                printf("Unknown trap type.\n");
                break;
        }
    }

    mask = sigsetmask(0);

    ldb_monitor();

    sigsetmask(mask);
}



test_init()
{
    install_handler(SIGINT, signal_handler);
#if defined(mips)||defined(ibmrt)
    install_handler(SIGTRAP, signal_handler);
#endif
#ifdef sparc
    install_handler(SIGILL, signal_handler);
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
