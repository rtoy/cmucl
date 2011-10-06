/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/FreeBSD-os.h,v 1.23 2009/01/20 04:45:18 agoncharov Rel $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _FREEBSD_OS_H_
#define _FREEBSD_OS_H_

#include <sys/types.h>
#include <sys/mman.h>
#include <osreldate.h>
#include <signal.h>
#include <ucontext.h>

typedef caddr_t os_vm_address_t;
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

#endif /* _FREEBSD_OS_H_ */
