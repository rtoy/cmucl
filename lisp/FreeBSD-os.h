/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/FreeBSD-os.h,v 1.20 2007/12/07 06:54:28 cshapiro Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _FREEBSD_OS_H_
#define _FREEBSD_OS_H_

#include <sys/param.h>
#include <sys/uio.h>
#include <sys/mman.h>
#include <sys/signal.h>

#include <osreldate.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ucontext.h>

typedef caddr_t os_vm_address_t;
typedef vm_size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;
#define os_context_t ucontext_t

#define OS_VM_PROT_READ    PROT_READ
#define OS_VM_PROT_WRITE   PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#define OS_VM_DEFAULT_PAGESIZE	4096

#define HANDLER_ARGS int signal, siginfo_t *code, ucontext_t *context
#define CODE(code)  ((code) ? code->si_code : 0)

int *sc_reg(ucontext_t *, int);

#if __FreeBSD_version < 700004
#define PROTECTION_VIOLATION_SIGNAL SIGBUS
#define PROTECTION_VIOLATION_CODE BUS_PAGE_FAULT
#else
#define PROTECTION_VIOLATION_SIGNAL SIGSEGV
#define PROTECTION_VIOLATION_CODE SEGV_ACCERR
#endif

#undef PAGE_SIZE

#endif /* _FREEBSD_OS_H_ */
