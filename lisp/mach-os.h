/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/mach-os.h,v 1.2 1994/10/27 17:13:54 ram Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <mach.h>

typedef vm_address_t os_vm_address_t;
typedef vm_size_t os_vm_size_t;
typedef vm_offset_t os_vm_offset_t;
typedef vm_prot_t os_vm_prot_t;

#define OS_VM_PROT_READ VM_PROT_READ
#define OS_VM_PROT_WRITE VM_PROT_WRITE
#define OS_VM_PROT_EXECUTE VM_PROT_EXECUTE

#define OS_VM_DEFAULT_PAGESIZE	4096
