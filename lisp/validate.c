/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/validate.c,v 1.14 2003/08/12 16:41:40 gerd Exp $
 *
 * Memory Validation
 */

#include <stdio.h>
#include <unistd.h>
#include "lisp.h"
#include "os.h"
#include "globals.h"
#include "validate.h"
#include "internals.h"

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


/* We use the linker symbol redefinition trick here to get the dynamic
   space size when the core is built in to the executable.  Note that
   initial_function_addr is used as a flag indicating that the lisp image
   is built into the executable.  FMG */
extern int initial_function_addr;
int image_dynamic_space_size = 0;

void
validate(void)
{
    void *dynamic_space_data;

    /* Note: XXX use alloca here because it's not malloc.  I'm assuming
     that anyone who wants to make this scheme work this will be using
     GCC.  FMG */
    if (initial_function_addr != 0)
      dynamic_space_data = alloca((int)(&image_dynamic_space_size));

    /* Read-Only Space */
    read_only_space = (lispobj *) READ_ONLY_SPACE_START;
    /* Don't try to map this space if the executable contains the
       image. */
    if (initial_function_addr == 0)
      ensure_space(read_only_space, READ_ONLY_SPACE_SIZE);

    /* Static Space */
    static_space = (lispobj *) STATIC_SPACE_START;
    /* Don't try to map this space if the executable contains the
       image. */
    if (initial_function_addr == 0)
      ensure_space(static_space, STATIC_SPACE_SIZE);

    /* Dynamic-0 Space */
    dynamic_0_space = (lispobj *) DYNAMIC_0_SPACE_START;
    if (initial_function_addr != 0) {
      /* If the executable contains the lisp image, we want to copy the
	 data in the dynamic space out of its segment, then map the
	 dynamic space (which has the side effect of unmapping the
	 dynamic space segment in the executable), then copy the data
	 back into it.  This is necessary to make the data in the
	 dynamic space segment available to the new lisp process.  */
      bcopy(dynamic_0_space, dynamic_space_data, (int)&image_dynamic_space_size);
      ensure_space(dynamic_0_space, dynamic_space_size);
      bcopy(dynamic_space_data, dynamic_0_space, (int)&image_dynamic_space_size);
    } else
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
    os_guard_control_stack (0, 1);
#endif
}
