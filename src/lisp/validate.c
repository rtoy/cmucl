/*
 * Memory Validation
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#ifdef sparc
#include <alloca.h>
#endif

#include "lisp.h"
#include "os.h"
#include "globals.h"
#include "validate.h"
#include "internals.h"

#ifdef sparc
extern void make_holes(void);
extern void make_stack_holes(void);
#endif

static void
ensure_space(lispobj * start, size_t size)
{
    if (os_validate((os_vm_address_t) start, size) == NULL) {
	fprintf(stderr,
		"ensure_space: Failed to validate %ld bytes at 0x%08lx\n",
		(unsigned long) size, (unsigned long) start);
	exit(1);
    }
}


/* builtin_image_flag is used as a flag indicating that the lisp image is
   built into the executable.  The other variables are set to actual values
   in elf.c when the core section is mapped. FMG
*/
extern int builtin_image_flag;
long image_dynamic_space_size = 0;
long image_read_only_space_size = 0;
long image_static_space_size = 0;

void
validate(void)
{
    /* void *dynamic_space_data = NULL; */

    /* Read-Only Space */
    read_only_space = (lispobj *) READ_ONLY_SPACE_START;
    /* Note that if the lisp core is not built into the image,
       the below expression will be equal to this:
       ensure_space(read_only_space, READ_ONLY_SPACE_SIZE);
       FMG
    */
    ensure_space((lispobj *)((int)read_only_space + image_read_only_space_size),
		 read_only_space_size - image_read_only_space_size);

    /* Static Space */
    static_space = (lispobj *) STATIC_SPACE_START;
    /* Note that if the lisp core is not built into the image,
       the below expression will be equal to this:
       ensure_space(static_space, STATIC_SPACE_SIZE);
       FMG
    */
    ensure_space((lispobj *)((int)static_space + image_static_space_size),
		 static_space_size - image_static_space_size);

    /* Dynamic-0 Space */
    dynamic_0_space = (lispobj *) DYNAMIC_0_SPACE_START;
    /* Note that if the lisp core is not built into the image,
       the below expression will be equal to this:
       ensure_space(dynamic_0_space, dynamic_space_size);
       FMG
    */
    ensure_space((lispobj *)((int)dynamic_0_space + image_dynamic_space_size),
		 dynamic_space_size - image_dynamic_space_size);

    current_dynamic_space = dynamic_0_space;

#ifndef GENCGC
    /* Dynamic-1 Space */
    dynamic_1_space = (lispobj *) DYNAMIC_1_SPACE_START;
    ensure_space(dynamic_1_space, dynamic_space_size);
    /* I'm not sure about the following, or if the lisp executable
       stuff will work with a garbage collector other than gencgc.
       FMG
    */
    /*
      if (builtin_image_flag != 0)
      dynamic_space_data = alloca((int) (&image_dynamic_space_size));
    */
#endif

#ifdef SIGNAL_STACK_START
    ensure_space((lispobj *) SIGNAL_STACK_START, SIGNAL_STACK_SIZE);
#endif

#ifdef LINKAGE_TABLE
    ensure_space((lispobj *) FOREIGN_LINKAGE_SPACE_START,
		 FOREIGN_LINKAGE_SPACE_SIZE);
#endif
#ifdef sparc
    make_holes();
#endif

#ifdef PRINTNOISE
    printf(" done.\n");
#endif

}

void
validate_stacks(void)
{
    /* Control Stack */
#ifdef CONTROL_STACK_START
    /* Map the control stack at a fixed location */
    control_stack = (lispobj *) CONTROL_STACK_START;
    control_stack_end = (lispobj *) (CONTROL_STACK_START + control_stack_size);
    ensure_space(control_stack, control_stack_size);
#else
    /* Map the conrol stack wherever we have space */
    control_stack = (lispobj*) os_validate(NULL, control_stack_size);
    control_stack_end = (void*)control_stack + control_stack_size;
#endif

    /* Binding Stack */
#ifdef BINDING_STACK_START
    binding_stack = (lispobj *) BINDING_STACK_START;
    ensure_space(binding_stack, binding_stack_size);
#else
    /* Map the binding stack wherever we have space */
    binding_stack = (lispobj*) os_validate(NULL, binding_stack_size);
#endif
#ifdef sparc
    make_stack_holes();
#endif

#ifdef RED_ZONE_HIT
    os_guard_control_stack(0, 1);
#endif
}
