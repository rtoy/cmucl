/*
 * This code was written as part of the CMUCL project and has been
 * placed in the public domain.
 */
#ifndef ARM64_LISPREGS_H
#define ARM64_LISPREGS_H

#ifdef LANGUAGE_ASSEMBLY
#define REG(num) x ## num
#else
#define REG(num) (num)
#endif

#define NREGS 32

/*
 * Non-descriptor (C argument/scratch) registers: X0-X7.
 * Caller-saved in both the C ABI and Lisp; hold raw untagged values.
 */
#define reg_NL0    REG(0)   /* X0  - C arg 0 / return value */
#define reg_NL1    REG(1)   /* X1  - C arg 1 */
#define reg_NL2    REG(2)   /* X2  - C arg 2 */
#define reg_NL3    REG(3)   /* X3  - C arg 3 */
#define reg_NL4    REG(4)   /* X4  - C arg 4 */
#define reg_NL5    REG(5)   /* X5  - C arg 5 */
#define reg_NL6    REG(6)   /* X6  - C arg 6 */
#define reg_NL7    REG(7)   /* X7  - C arg 7 */

/* Runtime state registers (non-descriptor range). */
#define reg_NARGS  REG(8)   /* X8  - number of arguments */
#define reg_CFUNC  REG(9)   /* X9  - C function address */
#define reg_NFP    REG(10)  /* X10 - number-stack frame pointer */
#define reg_BSP    REG(11)  /* X11 - binding stack pointer */
#define reg_CFP    REG(12)  /* X12 - control frame pointer */
#define reg_CSP    REG(13)  /* X13 - control stack pointer */
#define reg_ALLOC  REG(14)  /* X14 - allocation pointer */
#define reg_NULL   REG(15)  /* X15 - NIL / null register */

/* Code and call-target registers. */
#define reg_CODE   REG(16)  /* X16 - current code object (C: ip0) */
#define reg_FDEFN  REG(17)  /* X17 - function definition (C: ip1) */

/* Descriptor (Lisp object) registers. */
#define reg_CNAME  REG(18)  /* X18 - called name */
#define reg_LEXENV REG(19)  /* X19 - lexical environment */
#define reg_OCFP   REG(20)  /* X20 - old control frame pointer */
#define reg_LRA    REG(21)  /* X21 - lisp return address */

/* Argument registers (descriptor). */
#define reg_A0     REG(22)  /* X22 - argument 0 */
#define reg_A1     REG(23)  /* X23 - argument 1 */
#define reg_A2     REG(24)  /* X24 - argument 2 */
#define reg_A3     REG(25)  /* X25 - argument 3 */

/* Local (descriptor) registers. */
#define reg_L0     REG(26)  /* X26 - local 0 */
#define reg_L1     REG(27)  /* X27 - local 1 */
#define reg_L2     REG(28)  /* X28 - local 2 */

/* Interior pointer and ABI registers. */
#define reg_LIP    REG(29)  /* X29 - lisp interior pointer (C ABI FP, reclaimed) */
#define reg_LR     REG(30)  /* X30 - hardware link register */

/*
 * X31 is context-dependent in AArch64:
 *   - In data-processing encodings:    XZR (zero register), reads as 0.
 *   - In load/store base and SP-arith: SP  (stack pointer).
 */
#define reg_NSP    REG(31)  /* X31/SP - native stack pointer / zero register */

#define REGNAMES \
  "NL0", "NL1", "NL2", "NL3", "NL4", "NL5", "NL6", "NL7",   \
  "NARGS", "CFUNC", "NFP", "BSP", "CFP", "CSP", "ALLOC", "NULL", \
  "CODE", "FDEFN", "CNAME", "LEXENV", "OCFP", "LRA",          \
  "A0", "A1", "A2", "A3",                                      \
  "L0", "L1", "L2",                                            \
  "LIP", "LR", "ZR/SP"

#define BOXED_REGISTERS { \
  reg_A0, reg_A1, reg_A2, reg_A3, \
  reg_CNAME, reg_LEXENV, reg_OCFP, reg_LRA, reg_CODE, reg_FDEFN \
}

#define SC_REG(scp, reg) (*os_sigcontext_reg(scp, reg))
#define SC_PC(scp)       (*os_sigcontext_pc(scp))
#define SC_SP(scp)       SC_REG(scp, reg_CSP)

#endif /* ARM64_LISPREGS_H */
