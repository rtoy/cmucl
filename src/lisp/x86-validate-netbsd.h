/*
 *
 * This code was written as part of the CMU Common Lisp project at
 * Carnegie Mellon University, and has been placed in the public domain.
 *
 */

#ifndef X86_VALIDATE_NETBSD_H
#define X86_VALIDATE_NETBSD_H

/*
 * Also look in compiler/x86/parms.lisp for some of the parameters.
 *
 * Address map:
 *  NetBSD:
 *	0x00000000->0x0E000000  224M C program and memory allocation.
 *	0x0E000000->0x10000000   32M Foreign segment.
 *	0x10000000->0x20000000  256M Read-Only Space.
 *	0x28000000->0x38000000  256M Static Space.
 *	0x38000000->0x40000000  128M Binding stack growing up.
 *	0x40000000->0x48000000  128M Control stack growing down.
 *	0x48800000->0xB0000000 1656M Dynamic Space.
 *      0xB0000000->0xB1000000   16M Foreign Linkage Table
 *	0xE0000000->            256M C stack - Alien stack.
 *
 */

#define READ_ONLY_SPACE_START   (SpaceStart_TargetReadOnly)
#define READ_ONLY_SPACE_SIZE    (0x0ffff000)	/* 256MB - 1 page */

#define STATIC_SPACE_START	(SpaceStart_TargetStatic)
#define STATIC_SPACE_SIZE	(0x0ffff000)	/* 256MB - 1 page */

#define BINDING_STACK_START	(0x38000000)
#define BINDING_STACK_SIZE	(0x07fff000)	/* 128MB - 1 page */

#define CONTROL_STACK_START	(0x40000000)

#define CONTROL_STACK_SIZE	(0x07fd8000)	/* 128MB - SIGSTKSZ */

#define SIGNAL_STACK_START	(0x47fd8000)
#define SIGNAL_STACK_SIZE	SIGSTKSZ

#define DYNAMIC_0_SPACE_START	(SpaceStart_TargetDynamic)

#ifdef GENCGC
#define DYNAMIC_SPACE_SIZE	(0x67800000U)	/* 1.656GB */
#else
#define DYNAMIC_SPACE_SIZE	(0x04000000U)	/* 64MB */
#endif

#define DEFAULT_DYNAMIC_SPACE_SIZE	(0x20000000U)	/* 512MB */

#ifdef LINKAGE_TABLE
#define FOREIGN_LINKAGE_SPACE_START (LinkageSpaceStart)
#define FOREIGN_LINKAGE_SPACE_SIZE (0x100000)	/* 1MB */
#endif

#endif
