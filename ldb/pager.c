/*
 * Mach Memory Manager for GC Support
 *
 * Written by Christopher Hoover
 */

#include <stdio.h>
#include <mach.h>
#include <mach/message.h>
#include <mach/mig_errors.h>
#include <cthreads.h>
#include "pager.h"

static port_t pager_port;
static port_set_name_t pager_port_set;


/* Pager Object Registry */

static pager_object_t *pager_objects = NULL_PAGER_OBJECT;
static mutex_t pager_objects_mutex;

static pager_object_t *
allocate_pager_object()
{
	pager_object_t *object;

	object = (pager_object_t *) malloc(sizeof(pager_object_t));
	
	if (object == NULL_PAGER_OBJECT)
		return NULL_PAGER_OBJECT;

	mutex_lock(pager_objects_mutex);

        object->next = pager_objects;
	object->prev = NULL_PAGER_OBJECT;
	
	if (pager_objects != NULL_PAGER_OBJECT)
		pager_objects->prev = object;

	pager_objects = object;

	mutex_unlock(pager_objects_mutex);

	return object;
}

static void
deallocate_pager_object(object)
pager_object_t *object;
{
	mutex_lock(pager_objects_mutex);

	if (object->prev == NULL_PAGER_OBJECT) {
		/* Deleting item at head of list */
		pager_objects = object->next;
		if (pager_objects != NULL_PAGER_OBJECT)
			pager_objects->prev = NULL_PAGER_OBJECT;
	} else {
		pager_object_t *prev;
		pager_object_t *next;

		prev = object->prev;
		next = object->next;

		prev->next = next;
		if (next != NULL_PAGER_OBJECT)
			next->prev = prev;
	}

	mutex_unlock(pager_objects_mutex);

	(void) free((char *) object);
}

static pager_object_t *
find_pager_object(memory_object)
memory_object_t memory_object;
{
	pager_object_t *p;

	mutex_lock(pager_objects_mutex);

	for (p = pager_objects; p != NULL_PAGER_OBJECT; p = p->next)
		if (p->object == memory_object)
			break;

	mutex_unlock(pager_objects_mutex);

	return p;
}


/* Memory Object Allocation */

static kern_return_t
pager_allocate_memory_object(memory_object, address, size)
memory_object_t *memory_object;
vm_address_t address;
vm_size_t size;
{
	kern_return_t kr;
	pager_object_t *p;
	port_t port;
	memory_object_t object;

	pagerlog("pager_allocate_memory_object(memory_object, size = 0x%08x)\n",
	       size);

	if (size != round_page(size)) {
		kr = KERN_INVALID_ARGUMENT;
		goto e0;
	}


	p = allocate_pager_object();
	if (p == NULL_PAGER_OBJECT) {
		kr = KERN_RESOURCE_SHORTAGE;
		goto e0;
	}

	kr = port_allocate(task_self(), &port);
	if (kr != KERN_SUCCESS)
		goto e1;

	object = (memory_object_t) port;

	kr = port_set_add(task_self(), pager_port_set, port);
	if (kr != KERN_SUCCESS)
		goto e2;

	p->object = object;
	p->control = (memory_object_control_t) PORT_NULL;
	p->name = (memory_object_name_t) PORT_NULL;
	p->size = size;
	p->backing_store = address;

	*memory_object = object;

	pagerlog("Allocated memory object %d.  Backing store: addr = 0x%08x, length = 0x%08x\n",
	       port, address, size);

	return KERN_SUCCESS;

 e2:
	(void) port_deallocate(port);
 e1:
	deallocate_pager_object(p);
 e0:
	return kr;
}

static kern_return_t
pager_deallocate_memory_object(memory_object)
memory_object_t memory_object;
{
	pager_object_t *object;
	kern_return_t kr0, kr1, kr2, kr3;

	pagerlog("pager_deallocate_memory_object(memory_object = %d)\n",
	       memory_object);

	/* Find the pager object associated with this memory object. */
	object = find_pager_object(memory_object);
	if (object == NULL_PAGER_OBJECT)
		return KERN_INVALID_ARGUMENT;
	
	/* Roll through these ignoring errors until later. */
	kr0 = port_deallocate(task_self(), (port_t) object->object);
	kr1 = port_deallocate(task_self(), (port_t) object->control);
	kr2 = port_deallocate(task_self(), (port_t) object->name);
	kr3 = vm_deallocate(task_self(), object->backing_store,
			    object->size);

	deallocate_pager_object(object);
	
	if (kr0 != KERN_SUCCESS)
		return kr0;
	else if (kr1 != KERN_SUCCESS)
		return kr1;
	else if (kr2 != KERN_SUCCESS)
		return kr2;
	else if (kr3 != KERN_SUCCESS)
		return kr3;
	else
		return KERN_SUCCESS;

}	


/* Calls from the kernel */

kern_return_t
memory_object_init(memory_object, memory_control, memory_object_name,
		   memory_object_page_size)
memory_object_t memory_object;
memory_object_control_t memory_control;
memory_object_name_t memory_object_name;
vm_size_t memory_object_page_size;
{
	kern_return_t;
	pager_object_t *object;

	pagerlog("memory_object_init(memory_object = %d, memory_control = %d, memory_object_name = %d, memory_object_pager_size = 0x%0x)\n",
	       memory_object, memory_control, memory_object_name,
	       memory_object_page_size);

	/* Find the pager object associated with this memory object. */
	object = find_pager_object(memory_object);
	if (object == NULL_PAGER_OBJECT) {
		pagerlog("No pager object for memory object!\n");
		return KERN_FAILURE;
	}

	/* Record the interesting information. */
	object->control = memory_control;
	object->name = memory_object_name;

	/* Handshake with kernel. */

	/* ### May want to turn caching on ... probably won't be */
	/* useful though. */
	SYSCALL_OR_LOSE(memory_object_set_attributes(memory_control,
						     TRUE, FALSE,
						     MEMORY_OBJECT_COPY_DELAY));

	return KERN_SUCCESS;
}

kern_return_t
memory_object_terminate(memory_object, memory_control, memory_object_name)
memory_object_t memory_object;
memory_object_control_t memory_control;
memory_object_name_t memory_object_name;
{
	pager_object_t *object;

	pagerlog("memory_object_terminate(memory_object = %d, memory_control = %d, memory_object_name = %d)\n",
	       memory_object, memory_control, memory_object_name);

	/* Find the pager object associated with this memory object. */
	object = find_pager_object(memory_object);
	if (object == NULL_PAGER_OBJECT) {
		pagerlog("No pager object for memory object!\n");
		return KERN_FAILURE;
	}

	SYSCALL_OR_LOSE(pager_deallocate_memory_object(memory_object));

	return KERN_SUCCESS;
}

kern_return_t
memory_object_data_request(memory_object, memory_control, offset, length,
			   desired_access)
memory_object_t memory_object;
memory_object_control_t memory_control;
vm_offset_t offset;
vm_size_t length;
vm_prot_t desired_access;
{
	pager_object_t *object;

	pagerlog("memory_object_data_request(memory_object = %d, memory_control = %d, offset = 0x%08x, length = 0x%08x, desired access = 0x%0x)\n",
	       memory_object, memory_control, offset, length, desired_access);

	/* Find the pager object associated with this memory object. */
	object = find_pager_object(memory_object);
	if (object == NULL_PAGER_OBJECT) {
		pagerlog("No pager object for memory object!\n");
		return KERN_FAILURE;
	}

	if ((offset + length) <= object->size) {
		/* ### The lock value is something we want to play with. */
		SYSCALL_OR_LOSE(memory_object_data_provided(memory_control,
							    offset,
							    (object->backing_store +
							     offset),
							    length, VM_PROT_NONE));
	} else {
		/* Out of bounds. */
		pagerlog("Out of bounds memory request\n");
		SYSCALL_OR_LOSE(memory_object_data_error(memory_control,
							 offset, length,
							 KERN_INVALID_ADDRESS));
	}
	return KERN_SUCCESS;
}

kern_return_t
memory_object_data_write(memory_object, memory_control, offset, data, dataCnt)
memory_object_t memory_object;
memory_object_control_t memory_control;
vm_offset_t offset;
pointer_t data;
unsigned int dataCnt;
{
	pager_object_t *object;

	pagerlog("memory_object_data_write(memory_object = %d, memory_control = %d, offset = 0x%08x, data = 0x%08x, dataCnt = 0x%08x)\n",
	       memory_object, memory_control, offset, data, dataCnt);

	/* ### Stick recording mechanism in this routine. */

	/* Find the pager object associated with this memory object. */
	object = find_pager_object(memory_object);
	if (object == NULL_PAGER_OBJECT) {
		pagerlog("No pager object for memory object!\n");
		return KERN_FAILURE;
	}

	SYSCALL_OR_LOSE(vm_copy(task_self(), data, dataCnt,
				object->backing_store + offset));
	SYSCALL_OR_LOSE(vm_deallocate(task_self(), data, dataCnt));

	return KERN_SUCCESS;
}

kern_return_t
memory_object_copy(old_memory_object, old_memory_control, offset, length,
		   new_memory_object)
memory_object_t old_memory_object;
memory_object_control_t old_memory_control;
vm_offset_t offset;
vm_size_t length;
memory_object_t new_memory_object;
{
	pagerlog("memory_object_copy(old_memory_object = %d, old_memory_control = %d, offset = 0x%08x, length = 0x%08x, new_memory_object = %d)\n",
	       old_memory_object, old_memory_control, offset, length,
	       new_memory_object);

	pagerlog("Received memory_object_copy() RPC???\n");

	return KERN_SUCCESS;
}

kern_return_t
memory_object_data_unlock(memory_object, memory_control, offset, length,
			  desired_access)
memory_object_t memory_object;
memory_object_control_t memory_control;
vm_offset_t offset;
vm_size_t length;
vm_prot_t desired_access;
{
	pagerlog("memory_object_data_unlock(memory_object = %d, memory_control = %d, offset = 0x%08x, length = 0x%08x, desired access = 0x%0x)\n",
	       memory_object, memory_control, offset, length, desired_access);

	return KERN_SUCCESS;
}

kern_return_t memory_object_lock_completed(memory_object, memory_control,
					   offset, length)
memory_object_t memory_object;
memory_object_control_t memory_control;
vm_offset_t offset;
vm_size_t length;
{
	pagerlog("memory_object_lock_completed(memory_object = %d, memory_control = %d, offset = 0x%08x, length = 0x%08x)\n",
	       memory_object, memory_control, offset, length);

	return KERN_SUCCESS;
}


/* Server Loop */

typedef struct {
	msg_header_t head;
	msg_type_t return_code_type;
	kern_return_t return_code;
} reply_t;

static
pager_serve_requests()
{
	struct {
		msg_header_t header;
		char data[MSG_SIZE_MAX - sizeof(msg_header_t)];
	} in_msg, out_msg;

	while (1) {
		kern_return_t kr;
		boolean_t won;
		
		in_msg.header.msg_local_port = pager_port_set;
		in_msg.header.msg_size = sizeof(in_msg);
		
		kr = msg_receive(&in_msg.header, MSG_OPTION_NONE, 0);
		if (kr != RCV_SUCCESS) {
			pagerlog("msg_receive() lost.  kr = %d.\n", kr);
			continue;
		}

#if DEBUG_MESSAGE
		pagerlog("Message received on port %d\n",
		       in_msg.header.msg_local_port);
#endif

		won = memory_object_server(&in_msg.header, &out_msg.header);
		if (!won) {
			pagerlog("memory_objectserver() lost.\n");
			continue;
		}
		if ((((reply_t *) &out_msg)->return_code != MIG_NO_REPLY) &&
		    ((out_msg.header.msg_remote_port != PORT_NULL))) {
			kr = msg_send(&out_msg.header, MSG_OPTION_NONE, 0);
			pagerlog("msg_send() lost.  kr = %d.\n", kr);
		}
	}
}


/* Mach Routine Emulation */

kern_return_t
pager_vm_allocate(task, address, size, anywhere)
task_t task;
vm_address_t *address;
vm_size_t size;
boolean_t anywhere;
{
	kern_return_t kr;
	memory_object_t object;
	vm_address_t backing_store;

	pagerlog("pager_vm_allocate(*address = 0x%08x, size = 0x%08x, anywhere = %d)\n",
	       *address, size, anywhere);

	if (task != task_self())
		return KERN_INVALID_ARGUMENT;

	/* Emulate vm_allocate() ... */

	if (size == 0) {
		*address = 0;
		return KERN_SUCCESS;
	}

	*address = trunc_page(*address);
	size = round_page(size);

	kr = vm_allocate(task_self(), &backing_store, size, TRUE);
	if (kr != KERN_SUCCESS)
		goto e0;

	/* Now get a memory_object */
	kr = pager_allocate_memory_object(&object, backing_store, size);
	if (kr != KERN_SUCCESS)
		goto e0;

	kr = vm_map(task_self(), address, size, 0, anywhere,
		    object, 0, FALSE, VM_PROT_ALL, VM_PROT_ALL,
		    VM_INHERIT_COPY);
	if (kr != KERN_SUCCESS)
		goto e1;

	return KERN_SUCCESS;

 e1:
	(void) pager_deallocate_memory_object(object);
 e0:
	return kr;
}

kern_return_t
pager_vm_deallocate(task, address, size)
task_t task;
vm_address_t address;
vm_size_t size;
{
	pagerlog("pager_vm_deallocate(address = 0x%08x, size = 0x%08x)\n",
	       address, size);

	if (task != task_self())
		return KERN_INVALID_ARGUMENT;

	return vm_deallocate(task_self(), address, size);
}

kern_return_t
pager_map_fd(fd, offset, address, anywhere, size)
int fd;
vm_offset_t offset;
vm_address_t *address;
boolean_t anywhere;
vm_size_t size;
{
	kern_return_t kr;
	memory_object_t object;
	vm_address_t backing_store;

	pagerlog("pager_map_fd(fd = %d, offset = 0x%08x, *address = 0x%08x, anywhere = %d, size = 0x%08x)\n",
	       fd, offset, *address, anywhere, size);

	kr = map_fd(fd, offset, &backing_store, TRUE, size);
	if (kr != KERN_SUCCESS)
		goto e0;

	/* Now get a memory_object */
	kr = pager_allocate_memory_object(&object, backing_store, size);
	if (kr != KERN_SUCCESS)
		goto e0;

	(void) vm_deallocate(task_self(), *address, size);
	kr = vm_map(task_self(), address, size, 0, anywhere,
		    object, 0, FALSE, VM_PROT_ALL, VM_PROT_ALL,
		    VM_INHERIT_COPY);
	if (kr != KERN_SUCCESS)
		goto e1;

	return KERN_SUCCESS;

 e1:
	(void) pager_deallocate_memory_object(object);
 e0:
	return kr;
}


/* Initialization */

static
pager_thread(arg)
any_t arg;
{
	pagerlog("pager_thread() started.\n");
	
	pager_serve_requests();
}

pager_init()
{
	pager_objects_mutex = mutex_alloc();

	pagerlog("*** lisp pager log start ***\n");

	SYSCALL_OR_LOSE(port_allocate(task_self(), &pager_port));
	pagerlog("pager_port = %d\n", pager_port);

	SYSCALL_OR_LOSE(port_set_allocate(task_self(), &pager_port_set));
	pagerlog("pager_port_set = %d\n", pager_port_set);

	SYSCALL_OR_LOSE(port_set_add(task_self(), pager_port_set, pager_port));

	cthread_detach(cthread_fork(pager_thread, (any_t) 0));
}
