/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/NetBSD-os.h,v 1.1 2002/01/28 20:17:11 pmai Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/signal.h>

#define MAP_ANONYMOUS MAP_ANON
#define MAP_VARIABLE 0

typedef caddr_t os_vm_address_t;
typedef size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

#define OS_VM_PROT_READ PROT_READ
#define OS_VM_PROT_WRITE PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#define OS_VM_DEFAULT_PAGESIZE	4096

#define POSIX_SIGS
/* NetBSD 1.5.2 doesn't have this yet */
#define USE_SA_SIGINFO 0

#ifndef sa_sigaction
#define sa_sigaction    sa_handler
#endif

#define uc_sigmask sc_mask
int sc_reg(struct sigcontext*,int);
