/*
 * The arm-specific bits for Linux-os.c
 */

#include <ucontext.h>

void
os_init0(const char *argv[], const char *envp[])
{
    /* Nothing needed on ARM */
}

unsigned long *
os_sigcontext_reg(ucontext_t *scp, int offset)
{
#define reg(num) \
    case num: \
        return (unsigned long *) &scp->uc_mcontext.arm_r ## num
    switch (offset) {
      case 0:
          return (unsigned long *) &scp->uc_mcontext.arm_r0;
      case 1:
          return (unsigned long *) &scp->uc_mcontext.arm_r1;
      case 2:
          return (unsigned long *) &scp->uc_mcontext.arm_r2;
      case 3:
          return (unsigned long *) &scp->uc_mcontext.arm_r3;
      case 4:
          return (unsigned long *) &scp->uc_mcontext.arm_r4;
      case 5:
          return (unsigned long *) &scp->uc_mcontext.arm_r5;
      case 6:
          return (unsigned long *) &scp->uc_mcontext.arm_r6;
      case 7:
          return (unsigned long *) &scp->uc_mcontext.arm_r7;
      case 8:
          return (unsigned long *) &scp->uc_mcontext.arm_r8;
      case 9:
          return (unsigned long *) &scp->uc_mcontext.arm_r9;
      case 10:
          return (unsigned long *) &scp->uc_mcontext.arm_r10;
      case 11:
          return (unsigned long *) &scp->uc_mcontext.arm_fp;
      case 12:
          return (unsigned long *) &scp->uc_mcontext.arm_ip;
      case 13:
          return (unsigned long *) &scp->uc_mcontext.arm_sp;
      case 14:
          return (unsigned long *) &scp->uc_mcontext.arm_lr;
      case 15:
          return (unsigned long *) &scp->uc_mcontext.arm_pc;
      default:
          return NULL:
    }
}

unsigned long *
os_sigcontext_pc(ucontext_t *scp)
{
    return os_sigcontext_reg(scp, reg_PC);
}

unsigned char *
os_sigcontext_fpu_reg(ucontext_t *scp, int offset)
{
    return NULL;
}

unsigned int
os_sigcontext_fpu_modes(ucontext_t *scp)
{
    return 0;
}
#endif

