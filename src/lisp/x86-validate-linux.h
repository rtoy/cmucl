/*
 *
 * This code was written as part of the CMU Common Lisp project at
 * Carnegie Mellon University, and has been placed in the public domain.
 *
 */

#ifndef _X86_VALIDATE_LINUX_H_
#define _X86_VALIDATE_LINUX_H_

/*
 * Also look in compiler/x86/parms.lisp for some of the parameters.
 *
 * Address map:
 *
 *  Linux:
 *	0x00000000->0x08000000  128M Unused.
 *	0x08000000->0x10000000  128M C program and memory allocation.
 *	0x10000000->0x20000000  256M Read-Only Space.
 *	0x20000000->0x28000000  128M Binding stack growing up.
 *	0x28000000->0x38000000  256M Static Space.
 *	0x38000000->0x40000000  128M Control stack growing down.
 *	0x40000000->0x48000000  128M Reserved for shared libraries.
 *      0x58000000->0x58100000   16M Foreign Linkage Table
 *	0x58100000->0xB7B00000 1530M Dynamic Space.
 *      0xB7B82000->0xC0000000       Unknown Linux mapping, including stack
 *
 *     
 *      (Note: 0x58000000 allows us to run on a Linux system on an AMD
 *      x86-64.  Hence we have a gap of unused memory starting at
 *      0x48000000.)
 *
 * It appears as if the actual upper limit depends on the particular
 * Linux distribution.  Ubuntu 11.10 (32-bit) appears to have
 * something mapped at 0xb78b2000, so we can't allocate the dynamic
 * space past that.  That results in a max heap size of 1530 MB.
 * However, Fedora 22 ther appears to be nothing mapped there.  In
 * fact it appears to be free all the way to 0xf7c1b000.  That would
 * allow a heap of size 2555 MB.
 *
 * On even newer OSes like Ubuntu 17.04, the map looks something like
 *
 *	0x00000000->0x10000000  128M ?
 *	0x10000000->0x20000000  256M Read-Only Space.
 *	0x20000000->0x28000000  128M Binding stack growing up.
 *	0x28000000->0x38000000  256M Static Space.
 *      0x38000000->                 ?
 *      0x56555000->                 C space
 *      0xf7dfe000->                 [anon]
 *      0xf7e00000->0xf7fb4000       libc.so
 *      0xf7fb5000->0xf7fd9000       [anon]
 *      0xf7fd9000->0xf7ffd000       ld.so
 *      0xfffdd000->                 [stack]
 *
 * There's a potential clash if the lisp heap is mapped near
 * 0x58100000 as shown above.  This happens if a lisp exectuable image
 * is created.
 */

#define READ_ONLY_SPACE_START   (SpaceStart_TargetReadOnly)
#define READ_ONLY_SPACE_SIZE    (0x0ffff000)	/* 256MB - 1 page */

#define STATIC_SPACE_START	(SpaceStart_TargetStatic)
#define STATIC_SPACE_SIZE	(0x0ffff000)	/* 256MB - 1 page */

#define BINDING_STACK_SIZE	(0x07fff000)	/* 128MB - 1 page */

#define CONTROL_STACK_SIZE	(0x07fff000 - 8192)

#define SIGNAL_STACK_SIZE	SIGSTKSZ

#define DYNAMIC_0_SPACE_START	(SpaceStart_TargetDynamic)

#ifdef GENCGC
#define DYNAMIC_SPACE_SIZE	(0x5FA00000)	/* 1.530GB */
#else
#define DYNAMIC_SPACE_SIZE	(0x04000000)	/* 64MB */
#endif

#define DEFAULT_DYNAMIC_SPACE_SIZE	(0x20000000)	/* 512MB */

#ifdef LINKAGE_TABLE
#define FOREIGN_LINKAGE_SPACE_START (LinkageSpaceStart)
#define FOREIGN_LINKAGE_SPACE_SIZE (0x100000)	/* 1MB */
#endif

#endif

