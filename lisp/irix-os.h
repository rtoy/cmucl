/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/irix-os.h,v 1.2.2.1 1998/06/23 11:25:01 pw Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <sys/types.h>
#include <sys/mman.h>

typedef caddr_t os_vm_address_t;
typedef size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

#define OS_VM_PROT_READ PROT_READ
#define OS_VM_PROT_WRITE PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXECUTE

/* formerly 4096, on irix 6.2 on an R4400 Onyx, and irix 6.4 on an Octane,
   pagesize is 16384 */
#define OS_VM_DEFAULT_PAGESIZE	16384
