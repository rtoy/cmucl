/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef PPC_VALIDATE_H
#define PPC_VALIDATE_H

/*
 * Address map:
 *
 *  Darwin:
 *	0x04000000->0x0c000000  128M Read-Only Space.
 *	0x10000000->0x18000000  128M Static Space.
 *	0x17000000->0x17800000    8M Foreign Linkage Table (inside the
 *				     end of the static space).
 *	0x30000000->0x38000000  128M Control stack growing up.
 *	0x38000000->0x40000000  128M Binding stack growing up.
 *	0x40000000->0x80000000 1024M Dynamic Space (GENCGC).
 *
 *  Linux:
 *	0x04000000->0x0c000000  128M Read-Only Space.
 *	0x10000000->		     C program and malloc heap.
 *	0x20000000->0x28000000  128M Static Space.
 *	0x28000000->0x28800000    8M Foreign Linkage Table.
 *	0x30000000->0x38000000  128M Control stack growing up.
 *	0x38000000->0x40000000  128M Binding stack growing up.
 *	0x40000000->0x80000000 1024M Dynamic Space (GENCGC).
 *	High memory:		     Shared libraries and C stack.
 *
 * Without GENCGC, the dynamic space is two 512M spaces at 0x40000000
 * and 0x60000000.
 *
 * The sizes above are the maximums.  Each space except the linkage
 * table leaves a 32K hole at its end, and the dynamic space actually
 * allocated defaults to DEFAULT_DYNAMIC_SPACE_SIZE.
 *
 * The static space is moved on Linux because ELF executables are
 * loaded at 0x10000000.
 */

#ifdef LINKAGE_TABLE
/*
 * This space start better match the value of
 * target-foreign-linkage-space-start defined in sparc/parms.lisp!
 *
 * See the notes there!
 */

#ifdef LinkageSpaceStart
#define FOREIGN_LINKAGE_SPACE_START (LinkageSpaceStart)
#elif defined(__linux__)
#define FOREIGN_LINKAGE_SPACE_START (0x28000000)
#else
#define FOREIGN_LINKAGE_SPACE_START (0x17000000)
#endif
/*
 * This allows for about 510K symbols (assuming each entry is 16 bytes
 * long).  Hope that's enough!  Make sure this doesn't overlap the
 * READ_ONLY_SPACE_START!
 */
#define FOREIGN_LINKAGE_SPACE_SIZE  (0x00800000)	/* 8 MB */
#endif

/* 
 * The read-only space must be in low memory because the BA
 * instruction only has 26-bits to specify the address.  When this is
 * fixed, the read-only space can be moved.  (The BA instruction is
 * used to jump to assembly routines.)
 */
#define READ_ONLY_SPACE_START	(0x04000000)
#define READ_ONLY_SPACE_SIZE	(0x07ff8000)	/* 128 MB, almost */

/*
 * These must match target-static-space-start and
 * target-foreign-linkage-space-start in ppc/parms.lisp.
 */
#ifdef __linux__
#define STATIC_SPACE_START  	(0x20000000)
#else
#define STATIC_SPACE_START  	(0x10000000)
#endif
#define STATIC_SPACE_SIZE   	(0x07ff8000)	/* 128 MB, almost */

#define CONTROL_STACK_START 	(0x30000000)
#define CONTROL_STACK_SIZE  	(0x07ff8000)	/* 128 MB, almost */
#define CONTROL_STACK_END       (CONTROL_STACK_START + control_stack_size)

#define SIGNAL_STACK_SIZE	SIGSTKSZ

#define BINDING_STACK_START 	(0x38000000)
#define BINDING_STACK_SIZE  	(0x07ff8000)	/* 128 MB, almost */

#if 0
#define DYNAMIC_0_SPACE_START	(0x40000000)
#define DYNAMIC_1_SPACE_START	(0x48000000)
#define DYNAMIC_SPACE_SIZE  	(0x07fff000)

#define CONTROL_STACK_START 	(0x57000000)
#define CONTROL_STACK_SIZE  	(0x00ff0000)

#define BINDING_STACK_START 	(0x56000000)
#define BINDING_STACK_SIZE  	(0x00ff0000)
#endif

#define DEFAULT_DYNAMIC_SPACE_SIZE (0x10000000)	/* 256 MB */
#define DYNAMIC_0_SPACE_START	(0x40000000)
/* This isn't used with GENCGC */
#define DYNAMIC_1_SPACE_START	(0x60000000)

/* The maximum dynamic space we can allocate */
#ifndef GENCGC
#define DYNAMIC_SPACE_SIZE  	(0x1fff0000)	/* 512 MB, almost */
#else
/*
 * For GENCGC, we can use both dynamic spaces (because they are
 * contiguous) so we get double the heap size.
 */
#define DYNAMIC_SPACE_SIZE	(0x3fff0000)	/* 1GB, almost */
#endif

#if 0
#define HOLES {0x04ff8000, 0x06ff8000, 0x0aff8000, 0x1fff8000}
#define HOLE_SIZE 0x2000
#endif

#endif /* PPC_VALIDATE_H */
