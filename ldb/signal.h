#ifndef _SIGNAL_H_
#define _SIGNAL_H_

#include </usr/include/signal.h>

#ifdef sparc

/* We need to fake the existance of a reasonable sigcontext */
struct lisp_sigcontext {
    /* This part is identical to the real sigcontext. */
    int     sc_onstack;		/* sigstack state to restore */
    int     sc_mask;		/* signal mask to restore */
    int     sc_sp;                  /* sp to restore */
    int     sc_pc;                  /* pc to retore */
    int     sc_npc;                 /* next pc to restore */
    int     sc_psr;                 /* psr to restore */
    int     sc_g1;                  /* register that must be restored */
    int     sc_o0;

    /* And this is the part we have added. */
    unsigned int sc_regs[32];
    unsigned int sc_fpregs[32];
};

#define sigcontext lisp_sigcontext

#endif sparc


#endif _SIGNAL_H_
