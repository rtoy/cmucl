/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

 Morfed from the FreeBSD file by Peter Van Eynde (July 1996)
*/

#include <stdlib.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/signal.h>
#include <asm/sigcontext.h>
#include <string.h> 
#include <dlfcn.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <unistd.h>
#include <sys/syscall.h>
#include <asm/unistd.h>
#include <errno.h>
#include <linux/version.h>
#define MAP_VARIABLE 0

#define linuxversion(a, b, c) (((a)<<16)+((b)<<8)+(c))

typedef caddr_t os_vm_address_t; /* like hpux */
typedef size_t os_vm_size_t;     /* like hpux */
typedef off_t os_vm_offset_t;    /* like hpux */
typedef int os_vm_prot_t;        /* like hpux */

#define OS_VM_PROT_READ PROT_READ    /* like hpux */
#define OS_VM_PROT_WRITE PROT_WRITE  /* like hpux */
#define OS_VM_PROT_EXECUTE PROT_EXEC /* like hpux */
     
#define OS_VM_DEFAULT_PAGESIZE	4096 /* like hpux */ 

#if LINUX_VERSION_CODE >= linuxversion(2,1,0)
int sc_reg(struct sigcontext *,int);
#else
int sc_reg(struct sigcontext_struct *,int);
#endif
void os_save_context(void);

#define SAVE_CONTEXT os_save_context

#if LINUX_VERSION_CODE >= linuxversion(2,1,0)
typedef struct sigcontext sigcontext;
#else
typedef struct sigcontext_struct sigcontext;
#endif

#define POSIX_SIGS

#if LINUX_VERSION_CODE >= linuxversion(2,1,0)
#define HANDLER_ARGS int signal, struct sigcontext contextstruct
#define GET_CONTEXT int code=0; struct sigcontext *context=&contextstruct;
#else
#define HANDLER_ARGS int signal, struct sigcontext_struct contextstruct
#define GET_CONTEXT int code=0; struct sigcontext_struct *context=&contextstruct;
#endif

#define sigvec          sigaction
#define sv_mask         sa_mask
#define sv_flags        sa_flags
#define sv_handler      sa_handler
#define sv_onstack      sa_mask /* ouch, this one really hurts */
#define uc_sigmask 	oldmask
#define sc_pc		eip
#define sc_mask		oldmask 
#define sc_sp		esp
#if LINUX_VERSION_CODE >= linuxversion(2,1,0)
#define sigcontext	sigcontext 
#else
#define sigcontext	sigcontext_struct 
#endif
#define sa_sigaction	sa_handler
#define SA_SIGINFO	0
#define sc_efl		eflags

#define sc_eax eax
#define sc_ecx ecx
#define sc_edx edx
#define sc_ebx ebx
#define sc_esp esp
#define sc_ebp ebp
#define sc_esi esi
#define sc_edi edi
