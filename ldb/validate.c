/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/validate.c,v 1.3 1990/06/04 01:32:12 ch Exp $
 *
 * Memory Validation
 */

#include <stdio.h>
#include "lisp.h"
#include "os.h"
#include "globals.h"
#include "validate.h"

validate()
{
	printf("Validating memory ...");
	fflush(stdout);

	/* Read-Only Space */
	read_only_space = (lispobj *) READ_ONLY_SPACE_START;
	os_validate((os_vm_address_t) read_only_space,
		    (os_vm_size_t) READ_ONLY_SPACE_SIZE);

	/* Static Space */
	static_space = (lispobj *) STATIC_SPACE_START;
	os_validate((os_vm_address_t) static_space,
		    (os_vm_size_t) STATIC_SPACE_SIZE);

	/* Dynamic-0 Space */
	dynamic_0_space = (lispobj *) DYNAMIC_0_SPACE_START;
	os_validate((os_vm_address_t) dynamic_0_space,
		    (os_vm_size_t) DYNAMIC_SPACE_SIZE);
	current_dynamic_space = dynamic_0_space;

	/* Dynamic-1 Space */
	dynamic_1_space = (lispobj *) DYNAMIC_1_SPACE_START;
	os_validate((os_vm_address_t) dynamic_1_space,
		    (os_vm_size_t) DYNAMIC_SPACE_SIZE);

	/* Control Stack */
	control_stack = (lispobj *) CONTROL_STACK_START;
	os_validate((os_vm_address_t) control_stack,
		    (os_vm_size_t) CONTROL_STACK_SIZE);

	/* Binding Stack */
	binding_stack = (lispobj *) BINDING_STACK_START;
	os_validate((os_vm_address_t) binding_stack,
		    (os_vm_size_t) BINDING_STACK_SIZE);

	printf(" done.\n");
}
