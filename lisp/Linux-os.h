/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/Linux-os.h,v 1.24 2007/07/15 21:33:14 cshapiro Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

 Morfed from the FreeBSD file by Peter Van Eynde (July 1996)
 Alpha support by Julian Dolby, 1999.

*/

#ifndef _LINUX_OS_H_
#define _LINUX_OS_H_

#include <stdlib.h>
#include <signal.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/signal.h>
#include <string.h>
 /* #include <dlfcn.h> */
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <asm/unistd.h>
#include <errno.h>

typedef caddr_t os_vm_address_t;	/* like hpux */
typedef size_t os_vm_size_t;	/* like hpux */
typedef off_t os_vm_offset_t;	/* like hpux */
typedef int os_vm_prot_t;	/* like hpux */

#define OS_VM_PROT_READ PROT_READ	/* like hpux */
#define OS_VM_PROT_WRITE PROT_WRITE	/* like hpux */
#define OS_VM_PROT_EXECUTE PROT_EXEC	/* like hpux */

#ifndef __alpha__
#define OS_VM_DEFAULT_PAGESIZE	4096	/* like hpux */
#else
#define OS_VM_DEFAULT_PAGESIZE	8192	/* like hpux */
#endif

int *sc_reg(struct sigcontext *, int);

typedef struct sigcontext sigcontext;

/* Don't want the SIGINFO flag on linux as it causes the creation
   of real-time interrupt frames.
*/
#define USE_SA_SIGINFO 0

/* Alpha uses OSF/1 signals which are the defaults in os.h,
   so there is no need to define the following for Alpha 
   Linux 
*/
#if (defined(i386) || defined(__x86_64))

#define HANDLER_ARGS int signal, struct sigcontext contextstruct
#define GET_CONTEXT int code=0; struct sigcontext *context=&contextstruct;

#include <fpu_control.h>
#define setfpucw(cw) {fpu_control_t cw_tmp=cw;_FPU_SETCW(cw_tmp);} 

#define uc_sigmask 	oldmask
#if defined (__x86_64)
#define sc_pc		rip
#define sc_sp		rsp
#else
#define sc_pc		eip
#define sc_sp		esp
#endif
#define sc_mask		oldmask
#define sigcontext	sigcontext
#define sc_efl		eflags

#ifdef __x86_64
#define sc_rax rax
#define sc_rcx rcx
#define sc_rdx rdx
#define sc_rbx rbx
#define sc_rsp rsp
#define sc_rbp rbp
#define sc_rsi rsi
#define sc_rdi rdi
#define sc_rip rip
#else
#define sc_eax eax
#define sc_ecx ecx
#define sc_edx edx
#define sc_ebx ebx
#define sc_esp esp
#define sc_ebp ebp
#define sc_esi esi
#define sc_edi edi
#define sc_eip eip
#endif

#endif /* i386 */

#ifdef alpha
#define uc_sigmask	sc_mask
#endif /* alpha */

#ifndef sa_sigaction
#define sa_sigaction	sa_handler
#endif

#define PROTECTION_VIOLATION_SIGNAL SIGSEGV

#endif /* _LINUX_OS_H_ */
