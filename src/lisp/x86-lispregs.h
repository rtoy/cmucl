/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef X86_LISPREGS_H
#define X86_LISPREGS_H

/* These register names and offsets correspond to definitions
 * in compiler/x86/vm.lisp. They map into accessors in the
 * os dependent <machine/signal.h> structure via the sc_reg
 * os dependent function.
 */

#define NREGS	(8)

#ifdef LANGUAGE_ASSEMBLY
#define REG(num) $ ## num
#else
#define REG(num) num
#endif

#define reg_EAX REG( 0)
#define reg_ECX REG( 2)
#define reg_EDX REG( 4)
#define reg_EBX REG( 6)
#define reg_ESP REG( 8)
#define reg_EBP REG(10)
#define reg_ESI REG(12)
#define reg_EDI REG(14)
#define reg_EFL REG(16)

#define reg_SP reg_ESP
#define reg_FP reg_EBP
#define reg_NARGS reg_ECX

#define REGNAMES "EAX", "ECX", "EDX", "EBX", "ESP", "EBP", "ESI", "EDI"

/* These registers can contain lisp object pointers */
#define BOXED_REGISTERS {\
  reg_EAX, reg_ECX, reg_EDX, reg_EBX, reg_ESI, reg_EDI \
  }

/* N is offset in storage class (SC) as defined in vm.lisp.
 * Ordering in sigcontext is probably os dependent so let
 * xxx-os.c handle it.
 */

#define SC_REG(scp, offset) (*os_sigcontext_reg(scp, offset))
#define SC_PC(scp) (*os_sigcontext_pc(scp))
#define SC_SP(scp) SC_REG(scp, reg_ESP)

#if defined(DARWIN)
#if __DARWIN_UNIX03
/* For 10.5 */
#define SC_EFLAGS(sc) ((sc)->uc_mcontext->__ss.__eflags)
#else
/* For 10.4 */
#define SC_EFLAGS(sc) ((sc)->uc_mcontext->ss.eflags)
#endif
#elif defined(__linux__)
#define SC_EFLAGS(sc) SC_REG(sc, reg_EFL)
#elif defined(__NetBSD__)
#define SC_EFLAGS(sc) ((sc)->uc_mcontext.__gregs[_REG_EFL])
#elif defined(SOLARIS)
/*
 * Solaris/x86 has access the the eflags value, but this doesn't
 * currently work.  It seems that when we set the EFLAG to enable
 * single-stepping, we never actually step the new instruction but we
 * stop at exactly the same place.  This is not how it works on other
 * OSes where we do step an instruction.  I (rtoy) do not know why.
 * Some more work needs to be done in x86-arch.c to make this work.
 * But the default code there works fine on Solaris/x86.
 */
/* #define SC_EFLAGS(sc) ((sc)->uc_mcontext.gregs[EFL])*/
#endif

#endif /* X86_LISPREGS_H */
