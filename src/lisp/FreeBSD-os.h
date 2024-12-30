/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef FREEBSD_OS_H
#define FREEBSD_OS_H

#include <sys/mman.h>

#include <osreldate.h>
#include <signal.h>
#include <ucontext.h>

typedef void *os_vm_address_t;
typedef size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;
#define os_context_t ucontext_t

#define OS_VM_PROT_READ    PROT_READ
#define OS_VM_PROT_WRITE   PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#define OS_VM_DEFAULT_PAGESIZE	4096

/* ucontext_t *context; using `void *' to compile without warning */
#define HANDLER_ARGS int signal, siginfo_t *code, void *context

#define CODE(code)  ((code) ? code->si_code : 0)
#define RESTORE_FPU(context) restore_fpu(context)

void restore_fpu(ucontext_t *);

#if __FreeBSD_version < 700004
#define PROTECTION_VIOLATION_SIGNAL SIGBUS
#define PROTECTION_VIOLATION_CODE BUS_PAGE_FAULT
#else
#define PROTECTION_VIOLATION_SIGNAL SIGSEGV
#define PROTECTION_VIOLATION_CODE SEGV_ACCERR
#endif

#endif /* FREEBSD_OS_H */
