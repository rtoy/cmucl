/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/


#define READ_ONLY_SPACE_START   (0x01000000)
#define READ_ONLY_SPACE_SIZE    (0x01800000) /* 24MB!! */

#define STATIC_SPACE_START	(0x05000000)
#ifdef GENCGC
#define STATIC_SPACE_SIZE	(0x00fff000) /* 16MB - 1 page */
#else
#define STATIC_SPACE_SIZE	(0x02fff000) /* 64MB - 1 page */
#endif

/* Note that GENCGC only uses dynamic_space 0. */
#define DYNAMIC_0_SPACE_START	(0x09000000)
#ifdef GENCGC
#define DYNAMIC_1_SPACE_START	(0x29000000)
#define DYNAMIC_SPACE_SIZE	(0x20000000) /* 512MB */
#else
#define DYNAMIC_1_SPACE_START	(0x0d000000)
#define DYNAMIC_SPACE_SIZE	(0x04000000)
#endif

/* Note that i386 has the control stack growing from high to low
 * addresses, as opposed to the control stack used on the other RISC
 * systems for which the stack grows the other way. */
#define CONTROL_STACK_START	(0x50000000)
#define CONTROL_STACK_SIZE	(0x00100000)
#define CONTROL_STACK_END	(CONTROL_STACK_START + CONTROL_STACK_SIZE)

#define BINDING_STACK_START	(0x60000000)
#define BINDING_STACK_SIZE	(0x00100000)
