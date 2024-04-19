/* 

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

 Morfed from the FreeBSD file by Peter Van Eynde (July 1996)
 Alpha support by Julian Dolby, 1999.

*/

#ifndef _LINUX_OS_H_
#define _LINUX_OS_H_

#include <stdlib.h>
#include <signal.h>
#include <ucontext.h>
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

typedef char *os_vm_address_t;
typedef size_t os_vm_size_t;	/* like hpux */
typedef off_t os_vm_offset_t;	/* like hpux */
typedef int os_vm_prot_t;	/* like hpux */
#define os_context_t ucontext_t

#define OS_VM_PROT_READ PROT_READ	/* like hpux */
#define OS_VM_PROT_WRITE PROT_WRITE	/* like hpux */
#define OS_VM_PROT_EXECUTE PROT_EXEC	/* like hpux */

#ifndef __alpha__
#define OS_VM_DEFAULT_PAGESIZE	4096	/* like hpux */
#else
#define OS_VM_DEFAULT_PAGESIZE	8192	/* like hpux */
#endif

void restore_fpu(ucontext_t *);

#define HANDLER_ARGS int signal, siginfo_t *code, void *context
#define CODE(code) ((code) ? code->si_code : 0)
#define RESTORE_FPU(context) restore_fpu(context)

#define PROTECTION_VIOLATION_SIGNAL SIGSEGV

#endif /* _LINUX_OS_H_ */
