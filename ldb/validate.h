/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/validate.h,v 1.8 1991/09/18 06:37:47 wlott Exp $ */

#if !defined(_INCLUDE_VALIDATE_H_)
#define _INCLUDE_VALIDATE_H_

#ifdef ibmrt
#define READ_ONLY_SPACE_START	(0x00100000)
#define READ_ONLY_SPACE_SIZE    (0x04F00000)
#else
#define READ_ONLY_SPACE_START   (0x01000000)
#define READ_ONLY_SPACE_SIZE    (0x04000000)
#endif

#define STATIC_SPACE_START	(0x05000000)
#define STATIC_SPACE_SIZE	(0x02000000)

#define DYNAMIC_0_SPACE_START	(0x07000000)
#define DYNAMIC_1_SPACE_START	(0x0b000000)
#define DYNAMIC_SPACE_SIZE	(0x04000000)

#ifdef sparc
#define CONTROL_STACK_START	(0x00e00000)
#define CONTROL_STACK_SIZE	(0x00080000)
#else
#define CONTROL_STACK_START	(0x50000000)
#define CONTROL_STACK_SIZE	(0x00100000)
#endif

#ifdef sparc
#define BINDING_STACK_START	(0x00f00000)
#define BINDING_STACK_SIZE	(0x00080000)
#else
#define BINDING_STACK_START	(0x60000000)
#define BINDING_STACK_SIZE	(0x00100000)
#endif

#endif
