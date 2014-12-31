/*
 * This code was written as part of the CMUCL project and has been
 * placed in the public domain.
 */

#ifndef _ARM_VALIDATE_H_
#define _ARM_VALIDATE_H_

/*
 * Address map:
 *
 *	0x00000000->0x0f800000  248M C code and stuff(?)
 *      0x0f800000->0x10000000    8M for linkage table area
 *	0x10000000->0x20000000  256M Read-Only Space.
 *	0x20000000->0x28000000  128M Binding stack growing up.
 *	0x28000000->0x38000000  256M Static Space.
 *	0x38000000->0x40000000  128M Control stack growing up.
 *	0x40000000->0x80000000 1024M Dynamic space 1
 *	0x80000000->0xc0000000 1024M Dynamic space 2
 *      0xc0000000->0xffffffff 1024M C stack, dynamic libs, etc.      
 *
 */

/* 128 MB */
#define MB_128	(0x08000000)

#ifdef LINKAGE_TABLE
/*
 * This space start better match the value of
 * target-foreign-linkage-space-start defined in sparc/parms.lisp!
 *
 * See the notes there!
 */

#define FOREIGN_LINKAGE_SPACE_START (LinkageSpaceStart)

/*
 * This allows for about 510K symbols (assuming each entry is 16 bytes
 * long).  Hope that's enough!  Make sure this doesn't overlap the
 * READ_ONLY_SPACE_START!
 */
#define FOREIGN_LINKAGE_SPACE_SIZE  (0xF000000U)	/* 8 MB */
#endif

#define READ_ONLY_SPACE_START	(SpaceStart_TargetReadOnly)
#define READ_ONLY_SPACE_SIZE	(2*MB_128)	       /* 256 MB max */

#define BINDING_STACK_START 	(0x20000000)
#define BINDING_STACK_SIZE  	(MB_128)               /* 128 MB max */

#define STATIC_SPACE_START  	(SpaceStart_TargetStatic)
#define STATIC_SPACE_SIZE   	(2*MB_128)             /* 256 MB max */

#define CONTROL_STACK_START 	(0x38000000)
#define CONTROL_STACK_SIZE  	(MB_128)               /* 128 MB max */
#define CONTROL_STACK_END       (CONTROL_STACK_START + control_stack_size)

/* The default dynamic space to allocate */
#define DEFAULT_DYNAMIC_SPACE_SIZE  	(0x10000000)	/* 256 MB */

/* The maximum dynamic space that we can allocate */
#ifdef GENCGC
/*
 * For GENCGC, we can use both dynamic spaces, so we get at least
 * double the heap size.
 */
#define DYNAMIC_SPACE_SIZE      (0xB0000000)	/* 2816 MB max */
#else
#define DYNAMIC_SPACE_SIZE      (0x40000000)	/* 1GB max */
#endif

#define DYNAMIC_0_SPACE_START	(SpaceStart_TargetDynamic)
/* This isn't used with GENCGC */
#define DYNAMIC_1_SPACE_START	(DYNAMIC_1_SPACE_START + DYNAMIC_SPACE_SIZE)

#endif
