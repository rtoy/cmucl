/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef NETBSD_OS_H
#define NETBSD_OS_H

#include <sys/param.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <sys/signal.h>
#include <sys/stat.h>
#include <ucontext.h>
#include <string.h>
#include <unistd.h>

typedef caddr_t os_vm_address_t;
typedef size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;
#define os_context_t ucontext_t

#define OS_VM_PROT_READ PROT_READ
#define OS_VM_PROT_WRITE PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#if defined(sparc)
#define OS_VM_DEFAULT_PAGESIZE	8192
#else
#define OS_VM_DEFAULT_PAGESIZE	4096
#endif

#define HANDLER_ARGS int signal, siginfo_t *code, ucontext_t *context
#define CODE(code)  ((code) ? code->si_code : 0)
#define RESTORE_FPU(context) restore_fpu(context)

void restore_fpu(ucontext_t *);

#define PROTECTION_VIOLATION_SIGNAL SIGSEGV

#endif /* NETBSD_OS_H */
