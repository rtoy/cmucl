/*
 * FreeBSD-os.c. Maybe could be just BSD-os.c
 * From osf1-os.c,v 1.1 94/03/27 15:30:51 hallgren Exp $
 *
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 * This is the OSF1 version.  By Sean Hallgren.
 * Much hacked by Paul Werkowski
 *
 */

#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/param.h>
#include <signal.h>
#include <sys/user.h>
#include <sys/ptrace.h>
#include <sys/wait.h>
#include <sys/param.h>
#include <sys/file.h>
#include <sys/proc.h>
#include <errno.h>
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "lispregs.h"
#include "internals.h"

#include "x86-validate.h"
vm_size_t os_vm_page_size;
#define DPRINTF(t,a) {if(t)fprintf a;}

#define HAVE_SIGVEC		/* defined  - now obsolete */
#define HAVE_SIGACTION		/* replacement for SIGVEC          */
/* SIGSTKSZ == 40Kb */
#define SIG_STACK_SIZE (SIGSTKSZ/sizeof(double))
/* make sure the stack is 8 byte aligned */
#if defined USE_SIG_STACK
static double estack_buf[SIG_STACK_SIZE];
#endif

void
os_init()
{

#if defined USE_SIG_STACK
  static struct sigaltstack estack;
  estack.ss_base = (char*)estack_buf; /* this should be ss_sp */
  estack.ss_size = SIGSTKSZ;
  estack.ss_flags = 0;
  if (sigaltstack(&estack, 0) < 0)
    perror("sigaltstack");
#endif
  os_vm_page_size=OS_VM_DEFAULT_PAGESIZE;
}

int
sc_reg(struct sigcontext *c, int offset)
{
  switch(offset)
    {
    case  0: return c->sc_eax;
    case  2: return c->sc_ecx;
    case  4: return c->sc_edx;
    case  6: return c->sc_ebx;
    case  8: return c->sc_esp;
    case 10: return c->sc_ebp;
    case 12: return c->sc_esi;
    case 14: return c->sc_edi;
    }
  return 0;
}
void
os_save_context()
{
  /* Called from interrupt handlers so C stuff knows things set in Lisp
   */
}
void
os_set_context()
{
}

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
  int flags = MAP_PRIVATE|MAP_ANONYMOUS;

  if(addr) flags|=MAP_FIXED;
  else flags|=MAP_VARIABLE;

  DPRINTF(0,(stderr,"os_validate %x %d => ",addr,len));
  if((addr=mmap(addr,len,OS_VM_PROT_ALL,flags,-1,0)) == (os_vm_address_t) -1)
    {
      perror("mmap");
      return NULL;
    }
  DPRINTF(0,(stderr,"%x\n",addr));

  return addr;
}

void
os_invalidate(os_vm_address_t addr, os_vm_size_t len)
{
  DPRINTF(0,(stderr,"os_invalidate %x %d\n",addr,len));
  if(munmap(addr,len) == -1)
    perror("munmap");
}

os_vm_address_t
os_map(int fd, int offset, os_vm_address_t addr, os_vm_size_t len)
{
  if((addr=mmap(addr,len,OS_VM_PROT_ALL,MAP_PRIVATE|MAP_FILE|MAP_FIXED,fd,
		(off_t) offset)) == (os_vm_address_t) -1)
    perror("mmap");
  
  return addr;
}

void
os_flush_icache(os_vm_address_t address, os_vm_size_t length)
{
}

void
os_protect(os_vm_address_t address, os_vm_size_t length, os_vm_prot_t prot)
{
  if(mprotect(address, length, prot) == -1)
    perror("mprotect");
}

static boolean
in_range_p(os_vm_address_t a, lispobj sbeg, size_t slen)
{
  char* beg = (char*)sbeg;
  char* end = (char*)sbeg + slen;
  char* adr = (char*)a;
  return (adr >= beg && adr < end);
}
boolean
valid_addr(os_vm_address_t addr)
{
  int ret;
  os_vm_address_t newaddr;
  newaddr=os_trunc_to_page(addr);

  if(   in_range_p(addr, READ_ONLY_SPACE_START, READ_ONLY_SPACE_SIZE)
     || in_range_p(addr, STATIC_SPACE_START   , STATIC_SPACE_SIZE   )
     || in_range_p(addr, DYNAMIC_0_SPACE_START, DYNAMIC_SPACE_SIZE  )
     || in_range_p(addr, DYNAMIC_1_SPACE_START, DYNAMIC_SPACE_SIZE  )
     || in_range_p(addr, CONTROL_STACK_START  , CONTROL_STACK_SIZE  )
     || in_range_p(addr, BINDING_STACK_START  , BINDING_STACK_SIZE  ))
    return TRUE;
  return FALSE;
}


static void
sigbus_handler(int signal, int code, struct sigcontext *context)
{
  DPRINTF(0,(stderr,"sigbus:\n"));
#if defined NOTYET
  if(!interrupt_maybe_gc(signal, code, context))
#endif
    interrupt_handle_now(signal, code, context);
}
static void
sigsegv_handler(int signal, int code, struct sigcontext *context)
{
  DPRINTF(0,(stderr,"os_sigsegv\n"));
#if defined NOTYET
  if(!interrupt_maybe_gc(signal, code, context))
#endif
    interrupt_handle_now(signal, code, context);
}

void 
os_install_interrupt_handlers(void)
{
  interrupt_install_low_level_handler(SIGSEGV,sigsegv_handler);
  interrupt_install_low_level_handler(SIGBUS,sigbus_handler);
}


/* All this is needed to get the floating-point status register
 * that was stuffed in process context on a SIGFPE. We need it
 * to determine what kind of condition occured. This code also
 * sets up the possibility of defining some local structs to
 * make up for lack of sigcontext registers and have a low level
 * SIGFPE handler dummy something up.
 */
#ifdef not_now_maybe_not_ever
struct user u;
unsigned int
BSD_get_fp_modes()
{
  /* All this is highly dependent on FreeBSD internals. Watch Out! */
  /* offset to where NPX state is saved in a process */
  unsigned int fpoff = (char*)&u.u_pcb.pcb_savefpu - (char*)&u;
  unsigned int fplen = sizeof u.u_pcb.pcb_savefpu / sizeof(int);
  /* offset to the last exception status word */
  unsigned int swoff = (char*)&u.u_pcb.pcb_savefpu.sv_ex_sw - (char*)&u;
  pid_t pid;
  /* fork to capture NPX state in another process */
  pid =  fork();
  if(pid)
    {
      u_long ex_sw, ex_cw;
      int status;
      printf("p: wait1\n"); fflush(stdout);
      wait4(pid, &status, WUNTRACED, NULL);
      printf("P: wait over\n"); fflush(stdout);
      ex_sw = ptrace(PT_READ_U, pid, (caddr_t)swoff, 0);
      if(ex_sw == -1)
	perror("ptrace");
      {
	/* Might as well get the rest  of the saved state. */
	int i, *ip = (int*)&u.u_pcb.pcb_savefpu;
	unsigned int*uaddr = (unsigned int*)fpoff;
	for(i=0; i<fplen; i++, uaddr++)
	  *ip++ = ptrace(PT_READ_U, pid, (caddr_t)uaddr, 0);
	ex_cw = u.u_pcb.pcb_savefpu.sv_env.en_cw & 0xffff;
      }
      printf("sw %x cw %x\n",ex_sw,ex_cw); fflush(stdout);
      printf("p: Kill\n"); fflush(stdout);
      ptrace(PT_CONTINUE, pid, NULL, 0);
      printf("p: wait2\n"); fflush(stdout);
      wait4(pid, &status, 0, NULL);
      printf("p: wait over\n"); fflush(stdout);
      ex_sw &= 0xffff;
      ex_cw &= 0xffff;
      ex_cw ^= 0x3f;
      return (ex_sw << 16) | ex_cw ;
    }
  else
    {
      /* As child, notify OS to allow ptrace calls */
      int status = ptrace(PT_TRACE_ME, getpid(), NULL, 0);
      if(status == -1)
	perror("kid");
      printf("c:\n"); fflush(stdout);
      /* Go idle so parent can poke at process contents. */
      raise(SIGSTOP);
      printf("c: stopped?\n");
      while(0)
	{ sigsuspend(0); printf("c:\n"); fflush(stdout); }
      exit(1);
    }
}
#endif

