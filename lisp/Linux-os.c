/*
 * Linux-os.c. 
 * Form FreeBSD-os.c
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
 * Morfed from the FreeBSD file by Peter Van Eynde (July 1996)
 * GENCGC support by Douglas Crosher, 1996, 1997.
 *
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/Linux-os.c,v 1.4 1998/05/01 01:21:40 dtc Exp $
 *
 */

#include <stdio.h>
#include <sys/param.h>
#include <sys/file.h>
#include <errno.h>
#include "./signal.h"
#include "os.h"
#include "arch.h"
#include "globals.h"
#include "interrupt.h"
#include "lispregs.h"
#include "internals.h"
#include <sys/socket.h>
#include <sys/utsname.h>

#include <sys/types.h>
#include <signal.h>
/* #include <sys/sysinfo.h> */
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>

#include "x86-validate.h"
size_t os_vm_page_size;

#define DPRINTF(t,a) {if(t)fprintf a;}

#undef HAVE_SIGVEC		/* defined  - now obsolete */
#undef HAVE_SIGACTION		/* replacement for SIGVEC          */
/* SIGSTKSZ == 40Kb */
#define SIG_STACK_SIZE (SIGSTKSZ/sizeof(double))
/* make sure the stack is 8 byte aligned */
#if defined USE_SIG_STACK
static double estack_buf[SIG_STACK_SIZE];
#endif

#if defined GENCGC
#include "gencgc.h"
#endif

#if ((LINUX_VERSION_CODE >= linuxversion(2,1,0)) || (__GNU_LIBRARY__ >= 6))
int PVE_stub_errno;
#endif

#if ((LINUX_VERSION_CODE >= linuxversion(2,1,0)) || (__GNU_LIBRARY__ >= 6))
void update_errno (void)
{
  PVE_stub_errno = errno;
}
#endif


void
os_init(void)
{
  struct utsname name;

  uname(&name);

/* we need this for mmap */

  if ((name.release[0]) < '2')
   {
    printf("Linux version must be later then 2.0.0!\n");
    exit(2);
  }

#if defined USE_SIG_STACK
  static struct sigaltstack estack;
  estack.ss_base = (char*)estack_buf; /* this should be ss_sp */
  estack.ss_size = SIGSTKSZ;
  estack.ss_flags = 0;
  if (sigaltstack(&estack, 0) < 0)
    perror("sigaltstack");
#endif
#if 0
  os_vm_page_size=OS_VM_DEFAULT_PAGESIZE;
#else
  os_vm_page_size=getpagesize();
#endif
__setfpucw(0x1372|4|8|16|32); /*no interrupts */
}

int
#if (LINUX_VERSION_CODE >= linuxversion(2,1,0)) || (__GNU_LIBRARY__ >= 6)
sc_reg(struct sigcontext *c, int offset)
#else
sc_reg(struct sigcontext_struct *c, int offset)
#endif
{
  switch(offset)
    {
    case  0: return c->eax;
    case  2: return c->ecx;
    case  4: return c->edx;
    case  6: return c->ebx;
    case  8: return c->esp;
    case 10: return c->ebp;
    case 12: return c->esi;
    case 14: return c->edi;
    }
  return 0;
}

void
os_save_context(void)
{
  /* Called from interrupt handlers so C stuff knows things set in Lisp
   */
}

void
os_set_context(void)
{
}

int do_mmap(os_vm_address_t *addr, os_vm_size_t len,
	     int flags)
{
  /* we _must_ have the memory where we want it...*/
  os_vm_address_t old_addr=*addr;

  *addr=mmap(*addr,len,OS_VM_PROT_ALL,flags,-1,0);
  if (((old_addr != NULL) && (*addr != old_addr)) || 
	(*addr == (os_vm_address_t) -1)) 
    {
      fprintf(stderr, "Error in allocating memory, do you have more then 16MB of memory+swap?\n");
      perror("mmap");
      return 1;
    }
 return 0;
}

os_vm_address_t
os_validate(os_vm_address_t addr, os_vm_size_t len)
{
  int flags = MAP_PRIVATE|MAP_ANONYMOUS;
  int oa=addr,olen=len;

  if(addr) flags|=MAP_FIXED;
  else flags|=MAP_VARIABLE;

  DPRINTF(0,(stderr,"os_validate %x %d => ",addr,len));
  if (addr)
    {
      do {
	if (len <= 0x1000000 )
	  {
	    if (do_mmap(&addr,len,flags))
	      return NULL;
	    len=0;
	  }
	else
	  {
	    len = len-0x1000000;
	    if (do_mmap(&addr,0x1000000,flags))
		return NULL;
	    addr+=0x1000000;
	  }
      }
      while (len>0);
    }
  else
    {
      if(do_mmap(&addr,len,flags))
	  return NULL;
      return addr;
    }
  DPRINTF(0,(stderr,"%x\n",addr));
  return oa;
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



#if defined GENCGC
void sigsegv_handler(HANDLER_ARGS)
{
  GET_CONTEXT

  int  fault_addr = ((struct sigcontext_struct *)(&contextstruct))->cr2;
  int  page_index = find_page_index(fault_addr);

  /*signal(sig,sigsegv_handler);  /* Re-install; necessary? */

  /* Check if the fault is within the dynamic space. */
  if ( page_index!=-1 ) {
    /* Un-protect the page */
    /* The page should have been marked write_protected */
    if (page_table[page_index].write_protected != 1)
      fprintf(stderr,"*** Sigsegv in page not marked as write protected");
    os_protect(page_address(page_index), 4096, OS_VM_PROT_ALL);
    page_table[page_index].write_protected = 0;
    page_table[page_index].write_protected_cleared = 1;
    return;
  }

  DPRINTF(0,(stderr,"sigsegv: eip: %p\n",context->eip));
  interrupt_handle_now(signal, contextstruct);
  return;
}
#else
static void
sigsegv_handler(HANDLER_ARGS)
{
  GET_CONTEXT

  DPRINTF(1,(stderr,"sigsegv\n"));
  interrupt_handle_now(signal,contextstruct);
}
#endif

static void
sigbus_handler(HANDLER_ARGS)
{
  GET_CONTEXT

  DPRINTF(1,(stderr,"sigbus:\n")); /* there is no sigbus in linux??? */
  interrupt_handle_now(signal,contextstruct);
}

void 
os_install_interrupt_handlers(void)
{
  interrupt_install_low_level_handler(SIGSEGV,sigsegv_handler);
  interrupt_install_low_level_handler(SIGBUS,sigbus_handler);
}

#if 0
/* functions that disapear ! */
#define Force_Fct(fct) int * Force_ ## fct (void) {return &fct;}

Force_Fct(select)
Force_Fct(stat)
Force_Fct(lstat)
Force_Fct(fstat)
Force_Fct(socket)
Force_Fct(connect)
Force_Fct(listen)
Force_Fct(recv)
Force_Fct(accept)
Force_Fct(bind)
#endif


