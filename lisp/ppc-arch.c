/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/ppc-arch.c,v 1.2.2.1 2005/02/12 16:14:15 rtoy Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>

#include "arch.h"
#include "lisp.h"
#include "internals.h"
#include "globals.h"
#include "validate.h"
#include "os.h"
#include "lispregs.h"
#include "signal.h"
#include "interrupt.h"
#include "interr.h"

  /* The header files may not define PT_DAR/PT_DSISR.  This definition
     is correct for all versions of ppc linux >= 2.0.30

     As of DR2.1u4, MkLinux doesn't pass these registers to signal
     handlers correctly; a patch is necessary in order to (partially)
     correct this.

     Even with the patch, the DSISR may not have its 'write' bit set
     correctly (it tends not to be set if the fault was caused by
     something other than a protection violation.)
     
     Caveat callers.  */

#ifndef PT_DAR
#define PT_DAR		41
#endif

#ifndef PT_DSISR
#define PT_DSISR	42
#endif

char * arch_init(void)
{
  return "lisp.core";
}

os_vm_address_t 
arch_get_bad_addr(HANDLER_ARGS)
{
  unsigned long badinstr, pc = SC_PC(context);
  int instclass;
  os_vm_address_t addr;


  /* Make sure it's not the pc thats bogus, and that it was lisp code */
  /* that caused the fault. */
  if ((pc & 3) != 0 ||
      ((pc < READ_ONLY_SPACE_START ||
	pc >= READ_ONLY_SPACE_START+READ_ONLY_SPACE_SIZE) &&
       ((lispobj *)pc < current_dynamic_space &&
	(lispobj *)pc >= current_dynamic_space + DYNAMIC_SPACE_SIZE)))
    return 0;
  

  addr = (os_vm_address_t) SC_REG(context, PT_DAR);
  return addr;
}
      

void 
arch_skip_instruction(os_context_t *context)
{
  /* Skip the offending instruction */
  SC_PC(context) += 4;
}

unsigned char *
arch_internal_error_arguments(os_context_t *scp)
{
  return (unsigned char *)(SC_PC(scp)+4);
}

boolean 
arch_pseudo_atomic_atomic(os_context_t *scp)
{
  return (SC_REG(scp, reg_ALLOC) & 4);
}

#define PSEUDO_ATOMIC_INTERRUPTED_BIAS 0x7f000000

void 
arch_set_pseudo_atomic_interrupted(os_context_t *scp)
{
  SC_REG(scp, reg_ALLOC) |= 1;
}

unsigned long 
arch_install_breakpoint(void *pc)
{
  unsigned long *ptr = (unsigned long *)pc;
  unsigned long result = *ptr;
  *ptr = (3<<26) | (5 << 21) | trap_Breakpoint;
  os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));
  return result;
}

void 
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
  *(unsigned long *)pc = orig_inst;
  os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));
}

static unsigned long *skipped_break_addr, displaced_after_inst;
static sigset_t orig_sigmask;

void 
arch_do_displaced_inst(os_context_t *scp, unsigned long orig_inst)
{
  unsigned long *pc = (unsigned long *)SC_PC(scp);

  orig_sigmask = scp->uc_sigmask;
  sigemptyset(&scp->uc_sigmask);
  FILLBLOCKSET(&scp->uc_sigmask);

  *pc = orig_inst;
  os_flush_icache((os_vm_address_t) pc, sizeof(unsigned long));
  skipped_break_addr = pc;
}

static void 
sigill_handler(HANDLER_ARGS)
{
  int badinst;
  int opcode;
  HANDLER_GET_CONTEXT
    
    SAVE_CONTEXT();

  sigprocmask(SIG_SETMASK, &context->uc_sigmask, 0);
  opcode = *((int *) SC_PC(context));

  /* twnei reg_NL3,0 - check for deferred interrupt */

  if (opcode == ((3 << 26) | (0x18 << 21) | (reg_NL3 << 16))) {
    /* Clear the pseudo-atomic-interrupted bit */
    SC_REG(context, reg_ALLOC) &= ~1;
    arch_skip_instruction(context);
    interrupt_handle_pending(context);
#ifdef DARWIN
    /* Work around G5 bug; fix courtesy gbyers via chandler */
    sigreturn(context);
#endif
    return;
  }

  if ((opcode >> 16) == ((3 << 10) | (6 << 5))) {
    /* twllei reg_ZERO,N will always trap if reg_ZERO = 0 */
    int trap = opcode & 0x1f, extra = (opcode >> 5) & 0x1f;
    
    switch (trap) {
    case trap_Halt:
      fake_foreign_function_call(context);
      lose("%%primitive halt called; the party is over.\n");
      
    case trap_Error:
    case trap_Cerror:
      interrupt_internal_error(signal, code, context, trap == trap_Cerror);
      break;

    case trap_PendingInterrupt:
      arch_skip_instruction(context);
      interrupt_handle_pending(context);
      break;

    case trap_Breakpoint:
      handle_breakpoint(signal, code, context);
      break;
      
    case trap_FunctionEndBreakpoint:
      SC_PC(context)=(int)handle_function_end_breakpoint(signal, code, context);
      break;

    case trap_AfterBreakpoint:
      *skipped_break_addr = trap_Breakpoint;
      skipped_break_addr = NULL;
      *(unsigned long *)SC_PC(context) = displaced_after_inst;
      context->uc_sigmask = orig_sigmask;
      
      os_flush_icache((os_vm_address_t) SC_PC(context),
		      sizeof(unsigned long));
      break;

    default:
      interrupt_handle_now(signal, code, context);
      break;
    }
#ifdef DARWIN
    /* Work around G5 bug; fix courtesy gbyers via chandler */
    sigreturn(context);
#endif
    return;
  }
  if (((opcode >> 26) == 3) && (((opcode >> 21) & 31) == 24)) {
    interrupt_internal_error(signal, code, context, 0);
#ifdef DARWIN
    /* Work around G5 bug; fix courtesy gbyers via chandler */
    sigreturn(context);
#endif
    return;
  }

  interrupt_handle_now(signal, code, context);
#ifdef DARWIN
  /* Work around G5 bug; fix courtesy gbyers via chandler */
  sigreturn(context);
#endif
}


void arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGILL,sigill_handler);
    interrupt_install_low_level_handler(SIGTRAP,sigill_handler);
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

void
ppc_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
  os_vm_address_t end = (os_vm_address_t) ((int)(address+length+(32-1)) &~(32-1));
  extern void ppc_flush_cache_line(os_vm_address_t);

  while (address < end) {
    ppc_flush_cache_line(address);
    address += 32;
  }
}

#ifdef LINKAGE_TABLE
/* Linkage tables for PowerPC
 *
 * Linkage entry size is 16, because we need at least 4 instructions to
 * implement a jump.
 */

/*
 * This had better match lisp::target-foreign-linkage-entry-size in
 * ppco/parms.lisp!  Each entry is 6 instructions long, so at least
 * 24 bytes.
 */
#ifndef LinkageEntrySize
#define LinkageEntrySize (8*4)
#endif

/*
 * Define the registers to use in the linkage jump table. Can be the
 * same. Some care must be exercised when choosing these. It has to be
 * a register that is not otherwise being used. reg_NFP is a good
 * choice. call_into_c trashes reg_NFP without preserving it, so we can
 * trash it in the linkage jump table.
 */
#define LINKAGE_TEMP_REG        reg_NFP
#define LINKAGE_ADDR_REG        reg_A0

/*
 * Insert the necessary jump instructions at the given address.
 */
void
arch_make_jump_entry(void* reloc_addr, void *target_addr)
{
  /*
   * Make JMP to function entry.
   *
   * The instruction sequence is:
   *
   *        addis temp, 0, (hi part of reloc)
   *        ori   temp, temp, (lo part of reloc)
   *        addis addr, 0, (hi part of addr)
   *        ori   addr, addr, (low part of addr)
   *        mtctr addr
   *        bctr
   *        
   */
  int* inst_ptr;
  unsigned long hi;                   /* Top 16 bits of address */
  unsigned long lo;                   /* Low 16 bits of address */
  unsigned int inst;

  inst_ptr = (int*) reloc_addr;

  /*
   * Split the target address into hi and lo parts for the addis/ori
   * instructions.
   */
  hi = (unsigned long) reloc_addr;
  lo = hi & 0xffff;
  hi >>= 16;

  /*
   * addis 3, 0, (hi part)
   */
  inst = (15 << 26) | (LINKAGE_ADDR_REG << 21) | (0 << 16) | hi;
  *inst_ptr++ = inst;

  /*
   * ori 3, 3, (lo part)
   */

  inst = (24 << 26) | (LINKAGE_ADDR_REG << 21) | (LINKAGE_ADDR_REG << 16) | lo;
  *inst_ptr++ = inst;
      
  /*
   * Split the target address into hi and lo parts for the addis/ori
   * instructions.
   */

  hi = (unsigned long) target_addr;
  lo = hi & 0xffff;
  hi >>= 16;

  /*
   * addis 13, 0, (hi part)
   */
      
  inst = (15 << 26) | (LINKAGE_TEMP_REG << 21) | (0 << 16) | hi;
  *inst_ptr++ = inst;

  /*
   * ori 13, 13, (lo part)
   */

  inst = (24 << 26) | (LINKAGE_TEMP_REG << 21) | (LINKAGE_TEMP_REG << 16) | lo;
  *inst_ptr++ = inst;
  
  /*
   * mtctr 13
   */

  inst = (31 << 26) | (LINKAGE_TEMP_REG << 21) | (9 << 16) | (467 << 1);
  *inst_ptr++ = inst;

  /*
   * bctr
   */

  inst = (19 << 26) | (20 << 21) | (528 << 1);
  *inst_ptr++ = inst;


  *inst_ptr++ = inst;
  
  os_flush_icache((os_vm_address_t) reloc_addr, (char*) inst_ptr - (char*) reloc_addr);
}

void arch_make_linkage_entry(long linkage_entry, void *target_addr, long type)
{
  int *reloc_addr = (int *)(FOREIGN_LINKAGE_SPACE_START
                            + linkage_entry * LinkageEntrySize);

  if (type == 1)
    {			/* code reference */
      arch_make_jump_entry(reloc_addr, target_addr);
    }
  else if (type == 2)
    {
      *(unsigned long *)reloc_addr = (unsigned long)target_addr;
    }
}

/* Make a the entry a jump to resolve_linkage_tramp. */

extern void resolve_linkage_tramp(void);

void arch_make_lazy_linkage(long linkage_entry)
{
  arch_make_linkage_entry(linkage_entry, (void*) resolve_linkage_tramp, 1);
}

/* Get linkage entry.  We're given the return address which should be
   the address of the jmpl instruction (2nd word) of the linkage
   entry.  Figure out which entry this address belong to. */

long arch_linkage_entry(unsigned long retaddr)
{
  return (retaddr - (FOREIGN_LINKAGE_SPACE_START))
    / LinkageEntrySize;
}
#endif
