/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/ppc-validate.h,v 1.1 2004/07/13 00:26:22 pmai Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/


#define READ_ONLY_SPACE_START	(0x01000000)
#define READ_ONLY_SPACE_SIZE	(0x03ff8000)
  
#define STATIC_SPACE_START  	(0x08000000)
#define STATIC_SPACE_SIZE   	(0x017fff00)

#define DYNAMIC_0_SPACE_START	(0x40000000)
#define DYNAMIC_1_SPACE_START	(0x48000000)
#define DYNAMIC_SPACE_SIZE  	(0x07fff000)

#define CONTROL_STACK_START 	(0x57000000)
#define CONTROL_STACK_SIZE  	(0x00ff0000)

#define BINDING_STACK_START 	(0x56000000)
#define BINDING_STACK_SIZE  	(0x00ff0000)

#if 0
#define HOLES {0x04ff8000, 0x06ff8000, 0x0aff8000, 0x1fff8000}
#define HOLE_SIZE 0x2000
#endif
