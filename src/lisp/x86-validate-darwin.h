/*
 *
 * This code was written as part of the CMU Common Lisp project at
 * Carnegie Mellon University, and has been placed in the public domain.
 *
 */

#ifndef _X86_VALIDATE_DARWIN_H_
#define _X86_VALIDATE_DARWIN_H_

/*
 * Also look in compiler/x86/parms.lisp for some of the parameters.
 */

#define READ_ONLY_SPACE_START   (SpaceStart_TargetReadOnly)
#define READ_ONLY_SPACE_SIZE    (0x0ffff000)	/* 256MB - 1 page */

#define STATIC_SPACE_START	(SpaceStart_TargetStatic)
#define STATIC_SPACE_SIZE	(0x0ffff000)	/* 256MB - 1 page */

#define BINDING_STACK_SIZE	(0x07fff000)	/* 128MB - 1 page */

/*
 * According to /usr/include/sys/signal.h, MINSIGSTKSZ is 32K and
 * SIGSTKSZ is 128K.  We should account for that appropriately.
 */
#define CONTROL_STACK_SIZE	(0x07fdf000)	/* 128MB - SIGSTKSZ - 1 page */

#define SIGNAL_STACK_SIZE	SIGSTKSZ

#define DYNAMIC_0_SPACE_START	(SpaceStart_TargetDynamic)
#ifdef GENCGC

/*
 * On Darwin, /usr/lib/dyld appears to always be loaded at address
 * #x8fe2e000.  Hence, the maximum dynamic space size is 1206050816
 * bytes, or just over 1.150 GB.  Set the limit to 1.150 GB.
 */
#define DYNAMIC_SPACE_SIZE	(0x47E00000U)	/* 1.150GB */

#else
#define DYNAMIC_SPACE_SIZE	(0x04000000U)	/* 64MB */
#endif

#define DEFAULT_DYNAMIC_SPACE_SIZE	(0x20000000U)	/* 512MB */

#ifdef LINKAGE_TABLE
#define FOREIGN_LINKAGE_SPACE_START (LinkageSpaceStart)
#define FOREIGN_LINKAGE_SPACE_SIZE (0x100000)	/* 1MB */
#endif

#endif /*_X86_VALIDATE_DARWIN_H_*/
