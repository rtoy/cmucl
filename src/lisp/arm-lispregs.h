/*
 * This code was written as part of the CMUCL project and has been
 * placed in the public domain.
 */

#ifndef ARM_LISPREGS_H
#define ARM_LISPREGS_H

#ifdef LANGUAGE_ASSEMBLY
#define REG(num) r ## num
#else
#define REG(num) (num)
#endif

#define NREGS 16

/*
 * Define all the Lisp registers appropriately for assembly and C.
 */

#define reg_NL0 REG(0)
#define reg_OCFP REG(1)
#define reg_NARGS REG(2)
#define reg_NFP REG(3)
#define reg_CODE REG(4)
#define reg_A0 REG(5)
#define reg_A1 REG(6)
#define reg_A2 REG(7)
#define reg_LRA REG(8)
#define reg_NULL REG(9)
#define reg_LEXENV REG(10)
#define reg_CNAME REG(11)
#define reg_CFP REG(12)
#define reg_CSP REG(13)
#define reg_LIP REG(14)
#define reg_PC REG(15)

#define REGNAMES \
  "NL0", "OCFP", "NARGS", "NFP", "CODE", "A0", "A1", "A2", \
  "LRA", "NULL", "LEXENV", "CNAME", "CFP", "CSP", "LIP", "PC"

#define BOXED_REGISTERS { \
  reg_A0, reg_A1, reg_A2, reg_CNAME, reg_LRA, reg_LEXENV \
}

#define SC_REG(scp, reg) (*os_sigcontext_reg(scp, reg))
#define SC_PC(scp) (*os_sigcontext_reg(scp, reg_PC))
#define SC_SP(scp) SC_REG(scp, reg_CSP)

#endif /* ARM_LISPREGS_H */
