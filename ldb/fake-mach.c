#include "ldb.h"
#include "os.h"

#define KERN_SUCCESS		0
#define KERN_INVALID_ADDRESS	1
#define KERN_PROTECTION_FAILURE	2
#define KERN_NO_SPACE		3
#define KERN_INVALID_ARGUMENT	4
#define KERN_FAILURE		5

int task_self()
{
    return 0;
}

int thread_reply()
{
    return 0;
}

int task_notify()
{
    return 0;
}

int vm_allocate(task,addr_p,len,free)
int task;
os_vm_address_t *addr_p;
os_vm_size_t len;
int free;
{
    *addr_p=os_validate(free ? NULL : *addr_p,len);
    return (*addr_p)==NULL ? KERN_FAILURE : KERN_SUCCESS;
}

int vm_deallocate(task,addr,len)
int task;
os_vm_address_t addr;
os_vm_size_t len;
{
    os_invalidate(addr,len);
    return KERN_SUCCESS;
}

int vm_copy()
{
    return KERN_FAILURE;
}

int vm_statistics()
{
    return KERN_FAILURE;
}
