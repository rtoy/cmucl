/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/


#define READ_ONLY_SPACE_START   (0x01000000)
#define READ_ONLY_SPACE_SIZE    (0x04000000) /* 64MB!! */

#define STATIC_SPACE_START	(0x05000000)
#define STATIC_SPACE_SIZE	(0x02fff000) /* 64MB - 1 page */

#define DYNAMIC_0_SPACE_START	(0x09000000)
#define DYNAMIC_1_SPACE_START	(0x0d000000)
#define DYNAMIC_SPACE_SIZE	(0x04000000)

/* Note that i386 has the stack growing from high
 * to low addresses. The code for the RISC systems
 * seem to go the other way. May have to make
 * a lot of changes in the GC and supporting code
 * to account for stack direction.
 */
#define CONTROL_STACK_START	(0x50000000)
#define CONTROL_STACK_SIZE	(0x00100000)

#define BINDING_STACK_START	(0x60000000)
#define BINDING_STACK_SIZE	(0x00100000)
