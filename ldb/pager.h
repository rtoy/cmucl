/*
 * pager.h
 */

#if !defined(_PAGER_H_INCLUDED_)
#define _PAGER_H_INCLUDED_

typedef struct pager_object {
	memory_object_t object;
	memory_object_control_t control;
	memory_object_name_t name;
	vm_size_t size;
	vm_address_t backing_store;
	struct pager_object *prev;
	struct pager_object *next;
} pager_object_t;

#define NULL_PAGER_OBJECT ((pager_object_t *) 0)

#define SYSCALL_OR_LOSE(syscall) {                         	\
	kern_return_t kr;                                  	\
                                                           	\
	if ((kr = (syscall)) != KERN_SUCCESS) {            	\
		fprintf(stderr, "ERROR:\n");			\
		fprintf(stderr, "In file \"%s\", line %d:",	\
			__FILE__, __LINE__);               	\
		mach_error("", kr);                        	\
		exit(1);                                   	\
	}                                                  	\
}						   
						   
#define SYSCALL_OR_WARN(syscall) {                         	\
	kern_return_t kr;                                  	\
                                                           	\
	if ((kr = (syscall)) != KERN_SUCCESS) {            	\
		fprintf(stderr, "WARNING:\n");			\
		fprintf(stderr, "In file \"%s\", line %d:",	\
			__FILE__, __LINE__);               	\
		mach_error("", kr);                        	\
	}                                                  	\
}						   
						   
#endif
