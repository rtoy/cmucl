/* x86-arch.c -*- Mode: C; comment-column: 40 -*-
 *
 * $header: $ 
 *
 */

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

#define DPRINTF(test,e){if(test) fprintf e ;}

#define BREAKPOINT_INST 0xcc	/* INT3 */

unsigned long  fast_random_state = 1;

char *
arch_init(void)
{
  return "lisp.core";
}

os_vm_address_t 
arch_get_bad_addr(HANDLER_ARGS)
{
#ifdef __linux__
  GET_CONTEXT
#endif

  unsigned int badinst;

  if((context->sc_pc & 3) != 0) return NULL;

  if( (context->sc_pc < READ_ONLY_SPACE_START ||
       context->sc_pc >= READ_ONLY_SPACE_START+READ_ONLY_SPACE_SIZE) && 
      ((lispobj *)context->sc_pc < current_dynamic_space ||
       (lispobj *)context->sc_pc >= current_dynamic_space + DYNAMIC_SPACE_SIZE))
    return NULL;

  badinst = *(unsigned int *)context->sc_pc;
#ifdef fixme
  if((badinst>>27)!=0x16) return NULL;
  return (os_vm_address_t)(context->sc_regs[(badinst>>16)&0x1f]+(badinst&0xffff));
#else
  return NULL;
#endif
}

void arch_skip_instruction(context)
struct sigcontext *context;
{
  /* Assuming we get here via an INT3 xxx instruction, the PC now
   * points to the interrupt code (lisp value) so we just move past
   * it. Skip the code, then if the code if an error-trap or
   * Cerror-trap then skip the data bytes that follow. */
  int vlen,code;

  DPRINTF(0,(stderr,"[arch_skip_inst at %x>]\n", context->sc_pc));

  /* Get and skip the lisp error code. */
  code = *(char*)context->sc_pc++;
  switch (code)
    {
    case trap_Error:
    case trap_Cerror:
      /* Lisp error arg vector length */
      vlen = *(char*)context->sc_pc++;
      /* Skip lisp error arg data bytes */
      while(vlen-- > 0) 
	(char*)context->sc_pc++;
      /* Align 2 */
      /* (char*)context->sc_pc += 0x3;
      context->sc_pc &= ~0x03; */
      break;

    case trap_Breakpoint:		/* Not tested */
    case trap_FunctionEndBreakpoint:	/* not tested */
      break;

    case trap_PendingInterrupt:
    case trap_Halt:
      /* Only needed to skip the Code. */
      break;

    default:
      DPRINTF(1,(stderr,"[arch_skip_inst invalid code %d\n]\n",code));
      break;
    }

  DPRINTF(0,(stderr,"[arch_skip_inst resuming at %x>]\n", context->sc_pc));
}

unsigned char *
arch_internal_error_arguments(struct sigcontext *context)
{
  return (unsigned char *)(context->sc_pc+1);
}

boolean 
arch_pseudo_atomic_atomic(struct sigcontext *context)
{
#ifdef fixme
  return (context->sc_regs[reg_ALLOC] & 1);
#else
  return 0;
#endif
}

#define PSEUDO_ATOMIC_INTERRUPTED_BIAS 0x7f000000

void 
arch_set_pseudo_atomic_interrupted(struct sigcontext *context)
{
#ifdef fixme
  context->sc_regs[reg_ALLOC] |= 2;
#endif
}

/* This stuff seems to get called for TRACE and debug activity */
unsigned long 
arch_install_breakpoint(void *pc)
{
  char*ptr = (char*)pc;
  unsigned long result = *(unsigned long*)ptr;
  *ptr++ = BREAKPOINT_INST;		/* x86 INT3       */
  *ptr++ = trap_Breakpoint;		/* Lisp trap code */
  *ptr++ = 1;				/* vector length  */
  *ptr++ = 0;				/* junk data      */
  
  os_flush_icache((os_vm_address_t)pc, sizeof(unsigned long));

  return result;
}

void 
arch_remove_breakpoint(void *pc, unsigned long orig_inst)
{
  unsigned int *ptr=(unsigned int*)pc;
  *ptr = orig_inst;
  os_flush_icache((os_vm_address_t)pc, sizeof(unsigned long));
}

static unsigned int *skipped_break_addr, displaced_after_inst,
     after_breakpoint;
static int orig_sigmask;

unsigned int
emulate_branch(struct sigcontext *context,unsigned long orig_inst)
{
  /* This has to be invented for the X86. Maybe figure out how
   * to use the trace trap. Shouldn't be hard. Gdb does it.
   */
  int next_pc = context->sc_pc;
  return next_pc;
}

#ifdef __linux__
_syscall1(int,sigreturn,struct sigcontext *,context)
#endif


void 
arch_do_displaced_inst(struct sigcontext *context,
				   unsigned long orig_inst)
{
  /* Ditto the above for X86.
   */
  unsigned int *pc = (unsigned int*)context->sc_pc;
  unsigned int *next_pc;
  unsigned int next_inst;
  DPRINTF(0,(stderr,"[arch_do_displaced_inst %x %x NOT YET!]\n",
	     context,orig_inst));
  sigreturn(context);
}

#define AfterBreakpoint 100	/* pfw - what is this?? */

void 
sigtrap_handler(HANDLER_ARGS)
{
#ifdef __linux__
  GET_CONTEXT
#endif

#ifdef __linux__
    __setfpucw(contextstruct.fpstate->cw);
#endif

  /* Don't disallow recursive breakpoint traps.  Otherwise, we can't */
  /* use debugger breakpoints anywhere in here. */
  /*fprintf(stderr,"x86sigtrap: %8x %x\n", context->sc_pc, *(char*)(context->sc_pc-1));
   */
  DPRINTF(0,(stderr,"sigtrap(%d %d %x)\n",signal,code,context));
  sigsetmask(context->sc_mask);
  SAVE_CONTEXT();
  /* this is just for info in case monitor wants to print an approx */
  current_control_stack_pointer = (unsigned long*)context->sc_sp;
 /* On entry %eip points just after the INT3 byte and aims at the
  * 'kind' value (eg trap_Cerror). For error-trap and Cerror-trap a
  * number of bytes will follow, the first is the length of the byte
  * arguments to follow.  */
  if( *(unsigned char*)(context->sc_pc-1) == BREAKPOINT_INST)
    {
      if(after_breakpoint) code = AfterBreakpoint; /* ?? */
      else code = (int)*(char*)context->sc_pc;
    } 
  switch (code)
    {
    case trap_PendingInterrupt:
      DPRINTF(0,(stderr,"<trap Pending Interrupt.>\n"));
      arch_skip_instruction(context);
      interrupt_handle_pending(context);
      break;
      
    case trap_Halt:
      fake_foreign_function_call(context);
      lose("%%primitive halt called; the party is over.\n");
      undo_fake_foreign_function_call(context);
      arch_skip_instruction(context);
      break;
      
    case trap_Error:
    case trap_Cerror:
      DPRINTF(0,(stderr,"<trap Error %d>\n",code));
#ifdef __linux__
      interrupt_internal_error(signal,contextstruct, code==trap_Cerror);
#else
      interrupt_internal_error(signal, code, context, code==trap_Cerror);
#endif
      break;
      
    case trap_Breakpoint:		/* Not tested */
      (char*)context->sc_pc -= 1;
      handle_breakpoint(signal, code, context);
      break;
      
    case trap_FunctionEndBreakpoint:	/* not tested */
      (char*)context->sc_pc -= 1;
      context->sc_pc = (int)handle_function_end_breakpoint(signal, code, context);
      break;
      
    case AfterBreakpoint:		/* not tested */
      DPRINTF(1,(stderr,"[C--AfterBreakpoint NOT TESTED]\n"));
      (char*)context->sc_pc -= 1;
      *skipped_break_addr = BREAKPOINT_INST;
      os_flush_icache((os_vm_address_t)skipped_break_addr,
		      sizeof(unsigned long));
      skipped_break_addr = NULL;
      *(unsigned int *)context->sc_pc = displaced_after_inst;
      os_flush_icache((os_vm_address_t)context->sc_pc, sizeof(unsigned long));
      context->sc_mask = orig_sigmask;
      after_breakpoint=NULL;
      break;
      
    default:
      DPRINTF(0,(stderr,"[C--trap default %d %d %x]\n",signal,code,context));
#ifdef __linux__
      interrupt_handle_now(signal,contextstruct);
#else
      interrupt_handle_now(signal, code, context);
#endif
      break;
    }
}

#define FIXNUM_VALUE(lispobj) (((int)lispobj)>>2)

extern void first_handler();
void 
arch_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGILL ,sigtrap_handler);
    interrupt_install_low_level_handler(SIGTRAP,sigtrap_handler);
}


extern lispobj
call_into_lisp(lispobj fun, lispobj *args, int nargs);

/* These next four functions are an interface to the 
 * Lisp call-in facility. Since this is C we can know
 * nothing about the calling environment. The control
 * stack might be the C stack if called from the monitor
 * or the Lisp stack if called as a result of an interrupt
 * or maybe even a separate stack. The args are most likely
 * on that stack but could be in registers depending on
 * what the compiler likes. So I try to package up the
 * args into a portable vector and let the assembly language
 * call-in function figure it out.
 */
lispobj 
funcall0(lispobj function)
{
    lispobj *args = NULL;

    return call_into_lisp(function, args, 0);
}

lispobj
funcall1(lispobj function, lispobj arg0)
{
    lispobj args[1];
    args[0] = arg0;
    return call_into_lisp(function, args, 1);
}

lispobj
funcall2(lispobj function, lispobj arg0, lispobj arg1)
{
    lispobj args[2];
    args[0] = arg0;
    args[1] = arg1;
    return call_into_lisp(function, args, 2);
}

lispobj
funcall3(lispobj function, lispobj arg0, lispobj arg1, lispobj arg2)
{
    lispobj args[3];
    args[0] = arg0;
    args[1] = arg1;
    args[2] = arg2;
    return call_into_lisp(function, args, 3);
}
