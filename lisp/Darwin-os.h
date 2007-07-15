/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/Darwin-os.h,v 1.6 2007/07/15 06:53:34 cshapiro Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _DARWIN_OS_H_
#define _DARWIN_OS_H_

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/signal.h>
#include <signal.h>
#include <ucontext.h>
#include <mach/vm_types.h>

typedef caddr_t os_vm_address_t;
typedef vm_size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

#define OS_VM_PROT_READ PROT_READ
#define OS_VM_PROT_WRITE PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#define OS_VM_DEFAULT_PAGESIZE	4096

#define HANDLER_ARGS int signal, siginfo_t *code, os_context_t *context
#define CODE(code)  ((code) ? code->si_code : 0)
#define os_context_t ucontext_t

int *sc_reg(os_context_t *, int);

#define PROTECTION_VIOLATION_SIGNAL SIGBUS

#endif /* _DARWIN_OS_H_ */
