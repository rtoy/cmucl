/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/validate.c,v 1.11 2003/03/23 21:23:41 gerd Exp $
 *
 * Memory Validation
 */

#include <stdio.h>
#include "lisp.h"
#include "os.h"
#include "globals.h"
#include "validate.h"

static void
ensure_space(lispobj *start, size_t size)
{
    if (os_validate((os_vm_address_t) start, size) == NULL) {
	fprintf(stderr,
		"ensure_space: Failed to validate %ld bytes at 0x%08lx\n",
		(unsigned long) size,
		(unsigned long) start);
	exit(1);
    }
}

void
validate(void)
{
    /* Read-Only Space */
    read_only_space = (lispobj *) READ_ONLY_SPACE_START;
    ensure_space(read_only_space, READ_ONLY_SPACE_SIZE);

    /* Static Space */
    static_space = (lispobj *) STATIC_SPACE_START;
    ensure_space(static_space, STATIC_SPACE_SIZE);

    /* Dynamic-0 Space */
    dynamic_0_space = (lispobj *) DYNAMIC_0_SPACE_START;
    ensure_space(dynamic_0_space, dynamic_space_size);

    current_dynamic_space = dynamic_0_space;

#ifndef GENCGC
    /* Dynamic-1 Space */
    dynamic_1_space = (lispobj *) DYNAMIC_1_SPACE_START;
    ensure_space(dynamic_1_space, dynamic_space_size);
#endif

    /* Control Stack */
    control_stack = (lispobj *) CONTROL_STACK_START;
#ifdef i386
    control_stack_end = (lispobj *) (CONTROL_STACK_START
				     + CONTROL_STACK_SIZE);
#endif
    ensure_space(control_stack, CONTROL_STACK_SIZE);

#ifdef SIGNAL_STACK_START
    ensure_space((lispobj *) SIGNAL_STACK_START, SIGNAL_STACK_SIZE);
#endif

    /* Binding Stack */
    binding_stack = (lispobj *) BINDING_STACK_START;
    ensure_space(binding_stack, BINDING_STACK_SIZE);
#ifdef LINKAGE_TABLE
    ensure_space((lispobj *)FOREIGN_LINKAGE_SPACE_START,
		 FOREIGN_LINKAGE_SPACE_SIZE);
#endif
#ifdef sparc
    make_holes();
#endif

#ifdef PRINTNOISE
    printf(" done.\n");
#endif

#ifdef RED_ZONE_HIT
    fprintf (stderr, "guard\n");
    os_guard_control_stack (0, 1);
#endif
}
