/*
 *
 * This code was written as part of the CMU Common Lisp project at
 * Carnegie Mellon University, and has been placed in the public domain.
 *
 */

#ifndef X86_VALIDATE_SOLARIS_H
#define X86_VALIDATE_SOLARIS_H

/*
 * Also look in compiler/x86/parms.lisp for some of the parameters.
 *
 * The memory map for Solaris/x86 looks roughly like
 *
 *	0x08045000->0x08050000   C stack?
 *      0x08050000->             Code + C heap
 *      0x10000000->0x20000000   256 MB read-only space
 *	0x20000000->0x28000000   128M Binding stack growing up.
 *	0x28000000->0x30000000   256M Static Space.
 *      0x30000000->0x31000000   16M Foreign linkage table
 *	0x38000000->0x40000000   128M Control stack growing down.
 *	0x40000000->0xD0000000   2304M Dynamic Space.
 *
 * Starting at 0xd0ce0000 there is some mapped anon memory.  libc
 * seems to start at 0xd0d40000 and other places.  Looks like memory
 * above 0xd0ffe000 or so is not mapped.
 */

#define READ_ONLY_SPACE_START   (SpaceStart_TargetReadOnly)
#define READ_ONLY_SPACE_SIZE    (0x0ffff000)	/* 256MB - 1 page */

#define STATIC_SPACE_START	(SpaceStart_TargetStatic)
#define STATIC_SPACE_SIZE	(0x0ffff000)	/* 256MB - 1 page */

#define BINDING_STACK_START	(0x20000000)
#define BINDING_STACK_SIZE	(0x07fff000)	/* 128MB - 1 page */

#define CONTROL_STACK_START	0x38000000
#define CONTROL_STACK_SIZE	(0x07fff000 - 8192)
#define SIGNAL_STACK_START	CONTROL_STACK_END
#define SIGNAL_STACK_SIZE	SIGSTKSZ

#define DYNAMIC_0_SPACE_START	(SpaceStart_TargetDynamic)

#ifdef GENCGC
#define DYNAMIC_SPACE_SIZE	(0x90000000)	/* 2.304GB */
#else
#define DYNAMIC_SPACE_SIZE	(0x04000000)	/* 64MB */
#endif

#define DEFAULT_DYNAMIC_SPACE_SIZE	(0x20000000)	/* 512MB */

#ifdef LINKAGE_TABLE
#define FOREIGN_LINKAGE_SPACE_START (LinkageSpaceStart)
#define FOREIGN_LINKAGE_SPACE_SIZE (0x100000)	/* 1MB */
#endif

#endif

