/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/validate.c,v 1.2 1990/03/29 21:15:22 ch Exp $ */

#include <stdio.h>
#include "lisp.h"
#include "globals.h"
#include "validate.h"

validate()
{
	printf("Validating memory ...");
	fflush(stdout);

	/* Read only space */
	read_only_space = (lispobj *) READ_ONLY_SPACE_START;
	os_validate(read_only_space, READ_ONLY_SPACE_SIZE);

	/* Static Space */
	static_space = (lispobj *) STATIC_SPACE_START;
	os_validate(static_space, STATIC_SPACE_SIZE);

	/* Dynamic 0 Space */
	dynamic_0_space = (lispobj *) DYNAMIC_0_SPACE_START;
	os_validate(dynamic_0_space, DYNAMIC_SPACE_SIZE);
	current_dynamic_space = dynamic_0_space;

	dynamic_1_space = (lispobj *) DYNAMIC_1_SPACE_START;

	/* Control Stack */
	control_stack = (lispobj *) CONTROL_STACK_START;
	os_validate(control_stack, CONTROL_STACK_SIZE);

	/* Binding Stack */
	binding_stack = (lispobj *) BINDING_STACK_START;
	os_validate(binding_stack, BINDING_STACK_SIZE);

	printf(" done.\n");
}
