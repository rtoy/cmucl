/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef OSF1_OS_H
#define OSF1_OS_H

#include <sys/types.h>
#include <sys/mman.h>

typedef caddr_t os_vm_address_t;
typedef size_t os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

#define OS_VM_PROT_READ PROT_READ
#define OS_VM_PROT_WRITE PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#define OS_VM_DEFAULT_PAGESIZE	8192

#endif /* OSF1_OS_H */
