/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/validate.c,v 1.6 1991/09/13 20:42:24 wlott Exp $
 *
 * Memory Validation
 */

#include <stdio.h>
#include "ldb.h"
#include "lisp.h"
#include "os.h"
#include "globals.h"
#include "validate.h"

static void ensure_space(start,size)
lispobj *start;
unsigned long size;
{
    if(os_validate((os_vm_address_t)start,(os_vm_size_t)size)==NULL){
	fprintf(stderr,
		"ensure_space: Failed to validate %ld bytes at 0x%08x\n",
		size,
		start);
	exit(1);
    }
}

validate()
{
#ifdef PRINTNOISE
	printf("Validating memory ...");
	fflush(stdout);
#endif

	/* Read-Only Space */
	read_only_space = (lispobj *) READ_ONLY_SPACE_START;
	ensure_space(read_only_space, READ_ONLY_SPACE_SIZE);

	/* Static Space */
	static_space = (lispobj *) STATIC_SPACE_START;
	ensure_space(static_space, STATIC_SPACE_SIZE);

	/* Dynamic-0 Space */
	dynamic_0_space = (lispobj *) DYNAMIC_0_SPACE_START;
	ensure_space(dynamic_0_space, DYNAMIC_SPACE_SIZE);

	current_dynamic_space = dynamic_0_space;

	/* Dynamic-1 Space */
	dynamic_1_space = (lispobj *) DYNAMIC_1_SPACE_START;
	ensure_space(dynamic_1_space, DYNAMIC_SPACE_SIZE);

	/* Control Stack */
	control_stack = (lispobj *) CONTROL_STACK_START;
	ensure_space(control_stack, CONTROL_STACK_SIZE);

	/* Binding Stack */
	binding_stack = (lispobj *) BINDING_STACK_START;
	ensure_space(binding_stack, BINDING_STACK_SIZE);

#ifdef PRINTNOISE
	printf(" done.\n");
#endif
}
