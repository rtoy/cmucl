/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/validate.c,v 1.1 1990/03/28 22:51:19 ch Exp $ */

#include <stdio.h>
#include "lisp.h"
#include "globals.h"

validate()
{
	printf("Validating memory ...");
	fflush(stdout);

	/* Read only space */
	read_only_space = (lispobj *) 0x20000000;
	os_validate(read_only_space, 0x1000000);

	/* Static Space */
	static_space = (lispobj *) 0x30000000;
	os_validate(static_space, 0x1000000);

	/* Dynamic Spaces */
	dynamic_0_space = (lispobj *) 0x40000000;
	os_validate(dynamic_0_space, 0x2000000);
	current_dynamic_space = dynamic_0_space;

	dynamic_1_space = (lispobj *) 0x48000000;
	os_validate(dynamic_1_space, 0x08000000);

	/* Control Stack */
	control_stack = (lispobj *) 0x50000000;
	os_validate(control_stack, 0x08000000);

	/* Binding Stack */
	binding_stack = (lispobj *) 0x60000000;
	os_validate(binding_stack, 0x2000000);

	printf(" done.\n");
}
