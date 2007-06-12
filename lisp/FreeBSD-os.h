/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/FreeBSD-os.h,v 1.13 2007/06/12 03:21:46 cshapiro Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _FREEBSD_OS_H_
#define _FREEBSD_OS_H_

#include <sys/param.h>
#include <sys/uio.h>
#include <sys/mman.h>
#include <sys/signal.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef caddr_t os_vm_address_t;
typedef vm_size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

#define OS_VM_PROT_READ    PROT_READ
#define OS_VM_PROT_WRITE   PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#define OS_VM_DEFAULT_PAGESIZE	4096

int *sc_reg(struct sigcontext *, int);
void os_save_context(void);

/* If we used SA_SIGINFO in sigaction() the third argument to signal
   handlers would be a struct ucontext_t.  (The manpage for
   sigaction(2) is wrong!)  Sigcontext and ucontext_t are
   "compatible", but access to registers in a ucontext_t goes through
   the uc_mcontext field, so we just won't bother.  */
#define USE_SA_SIGINFO 0
#define uc_sigmask sc_mask

#define PROTECTION_VIOLATION_SIGNAL SIGBUS

#undef PAGE_SIZE

#endif /* _FREEBSD_OS_H_ */
