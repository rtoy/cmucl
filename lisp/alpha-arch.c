#include <stdio.h>

#include "lisp.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "internals.h"
#include "arch.h"
#include "lispregs.h"
#include "signal.h"
#include "alloc.h"
#include "interrupt.h"
#include "interr.h"
#include "breakpoint.h"

extern char call_into_lisp_LRA[], call_into_lisp_end[];

char *arch_init(void)
{
  if(mmap((os_vm_address_t) call_into_lisp_LRA_page,OS_VM_DEFAULT_PAGESIZE,
	  OS_VM_PROT_ALL,MAP_PRIVATE|MAP_ANONYMOUS|MAP_FIXED,-1,0)
     == (os_vm_address_t) -1)
    perror("mmap");
  bcopy(call_into_lisp_LRA,call_into_lisp_LRA_page,OS_VM_DEFAULT_PAGESIZE);
  os_flush_icache(call_into_lisp_LRA_page,OS_VM_DEFAULT_PAGESIZE);
  return NULL;
}

os_vm_address_t arch_get_bad_addr(int sig, int code, struct sigcontext *scp)
{
  unsigned int badinst;

  if((scp->sc_pc & 3) != 0) return NULL;

  if( (scp->sc_pc < READ_ONLY_SPACE_START ||
       scp->sc_pc >= READ_ONLY_SPACE_START+READ_ONLY_SPACE_SIZE) && 
      ((lispobj *)scp->sc_pc < current_dynamic_space ||
       (lispobj *)scp->sc_pc >= current_dynamic_space + DYNAMIC_SPACE_SIZE))
    return NULL;

  badinst = *(unsigned int *)scp->sc_pc;

  if((badinst>>27)!=0x16) return NULL;

  return scp->sc_regs[(badinst>>16)&0x1f] + (badinst&0xffff);
}

void arch_skip_instruction(scp)
struct sigcontext *scp;
{
}

unsigned char *arch_internal_error_arguments(struct sigcontext *scp)
{
  return (unsigned char *)(scp->sc_pc+4);
}

boolean arch_pseudo_atomic_atomic(struct sigcontext *scp)
{
  return (scp->sc_regs[reg_ALLOC] & 1);
}

#define PSEUDO_ATOMIC_INTERRUPTED_BIAS 0x7f000000

void arch_set_pseudo_atomic_interrupted(struct sigcontext *scp)
{
  scp->sc_regs[reg_ALLOC] |= 2;
}

unsigned long arch_install_breakpoint(void *pc)
{
}

void arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
}

static unsigned long *skipped_break_addr, displaced_after_inst;
static int orig_sigmask;

void arch_do_displaced_inst(struct sigcontext *scp,
				   unsigned long orig_inst)
{
}

static void sigtrap_handler(int signal, int code, struct sigcontext *scp)
{
    /* Don't disallow recursive breakpoint traps.  Otherwise, we can't */
    /* use debugger breakpoints anywhere in here. */
    sigsetmask(scp->sc_mask);

    code=*(u32 *)(scp->sc_pc);

    switch (code) {
      case trap_PendingInterrupt:
	arch_skip_instruction(scp);
	interrupt_handle_pending(scp);
	break;

      case trap_Halt:
	fake_foreign_function_call(scp);
	lose("%%primitive halt called; the party is over.\n");

      case trap_Error:
      case trap_Cerror:
	interrupt_internal_error(signal, code, scp, code==trap_Cerror);
	break;

      case trap_Breakpoint:
	handle_breakpoint(signal, code, scp);
	break;

      case trap_FunctionEndBreakpoint:
	scp->sc_pc = (int)handle_function_end_breakpoint(signal, code, scp);
	break;
/*
      case trap_AfterBreakpoint:
	*skipped_break_addr = (trap_Breakpoint << 16) | 0xd;
	os_flush_icache((os_vm_address_t)skipped_break_addr,
			sizeof(unsigned long));
	skipped_break_addr = NULL;
	*(unsigned long *)scp->sc_pc = displaced_after_inst;
	os_flush_icache((os_vm_address_t)scp->sc_pc, sizeof(unsigned long));
	scp->sc_mask = orig_sigmask;
	break;
*/
      default:
	interrupt_handle_now(signal, code, scp);
	break;
    }
}

#define FIXNUM_VALUE(lispobj) (((int)lispobj)>>2)

static void sigfpe_handler(int signal, int code, struct sigcontext *scp)
{
}

void arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGILL,sigtrap_handler);
    interrupt_install_low_level_handler(SIGTRAP,sigtrap_handler);
    interrupt_install_low_level_handler(SIGFPE,sigfpe_handler);
}

extern lispobj call_into_lisp(lispobj fun, lispobj *args, int nargs);

lispobj funcall0(lispobj function)
{
    lispobj *args = current_control_stack_pointer;

    return call_into_lisp(function, args, 0);
}

lispobj funcall1(lispobj function, lispobj arg0)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 1;
    args[0] = arg0;

    return call_into_lisp(function, args, 1);
}

lispobj funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 2;
    args[0] = arg0;
    args[1] = arg1;

    return call_into_lisp(function, args, 2);
}

lispobj funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj *args = current_control_stack_pointer;

    current_control_stack_pointer += 3;
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;

    return call_into_lisp(function, args, 3);
}


/* This is apparently called by emulate_branch, but isn't defined.  So */
/* just do nothing and hope it works... */

void cacheflush(void)
{
}
