/* x86-lispregs.h -*- Mode: C; -*-
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/amd64-lispregs.h,v 1.1 2004/05/18 22:06:04 cwang Exp $
 */

/* These register names and offsets correspond to definitions
 * in compiler/amd64/vm.lisp. They map into accessors in the
 * os dependent <machine/signal.h> structure via the sc_reg
 * os dependent function.
 */

#define NREGS	(8)

#ifdef LANGUAGE_ASSEMBLY
#define REG(num) $ ## num
#else
#define REG(num) num
#endif

#define reg_RAX REG( 0)
#define reg_RCX REG( 2)
#define reg_RDX REG( 4)
#define reg_RBX REG( 6)
#define reg_RSP REG( 8)
#define reg_RBP REG(10)
#define reg_RSI REG(12)
#define reg_RDI REG(14)

#define reg_SP reg_RSP
#define reg_FP reg_RBP

#define REGNAMES "RAX", "RCX", "RDX", "RBX", "RSP", "RBP", "RSI", "RDI"

/* These registers can contain lisp object pointers */
#define BOXED_REGISTERS {\
  reg_RAX, reg_RCX, reg_RDX, reg_RBX, reg_RSI, reg_RDI \
  }

/* N is offset in storage class (SC) as defined in vm.lisp.
 * Ordering in sigcontext is probably os dependent so let
 * xxx-os.c handle it.
 */

#define SC_REG(sc, n) sc_reg(sc,n)
#define SC_PC(sc) ((sc)->sc_pc)

