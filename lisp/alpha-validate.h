/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/alpha-validate.h,v 1.2 1994/10/27 17:13:54 ram Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/


#define READ_ONLY_SPACE_START   (0x20000000)
#define READ_ONLY_SPACE_SIZE    (0x04000000)

#define STATIC_SPACE_START	(0x28000000)
#define STATIC_SPACE_SIZE	(0x04000000)

#define DYNAMIC_0_SPACE_START	(0x30000000)
#define DYNAMIC_1_SPACE_START	(0x38000000)
#define DYNAMIC_SPACE_SIZE	(0x04000000)

#define CONTROL_STACK_START	(0x50000000)
#define CONTROL_STACK_SIZE	(0x00100000)

#define BINDING_STACK_START	(0x70000000)
#define BINDING_STACK_SIZE	(0x00100000)
