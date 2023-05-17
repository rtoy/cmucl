/*
 *
 * This code was written as part of the CMU Common Lisp project at
 * Carnegie Mellon University, and has been placed in the public domain.
 *
 */

#ifndef _X86_VALIDATE_H_
#define _X86_VALIDATE_H_

/*
 * Also look in compiler/x86/parms.lisp for some of the parameters.
 *
 * Address map:
 *
 *  OpenBSD:
 *	0x00000000->0x0E000000  224M C program and memory allocation.
 *	0x0E000000->0x10000000   32M Foreign segment.
 *	0x10000000->0x20000000  256M Read-Only Space.
 *	0x20000000->0x28000000  128M Binding stack growing up.
 *	0x28000000->0x38000000  256M Static Space.
 *	0x38000000->0x40000000  128M Control stack growing down.
 *	0x40000000->0x48000000  128M Reserved for shared libraries.
 *	0x48000000->0xB0000000 1664M Dynamic Space.
 *      0xB0000000->0xB1000000   16M Foreign Linkage Table
 *	0xE0000000->            256M C stack - Alien stack.
 *
 */

#define READ_ONLY_SPACE_START   (0x10000000)
#define READ_ONLY_SPACE_SIZE    (0x0ffff000)	/* 256MB - 1 page */

#define STATIC_SPACE_START	(0x28000000)
#define STATIC_SPACE_SIZE	(0x0ffff000)	/* 256MB - 1 page */

#define BINDING_STACK_SIZE	(0x07fff000)	/* 128MB - 1 page */

#define CONTROL_STACK_SIZE	(0x07fd8000)	/* 128MB - SIGSTKSZ */

#define SIGNAL_STACK_START	(0x47fd8000)
#define SIGNAL_STACK_SIZE	SIGSTKSZ

#define DYNAMIC_0_SPACE_START	(0x48000000)

#ifdef GENCGC
#define DYNAMIC_SPACE_SIZE	(0x68000000)	/* 1.625GB */
#else
#define DYNAMIC_SPACE_SIZE	(0x04000000)	/* 64MB */
#endif

#define DEFAULT_DYNAMIC_SPACE_SIZE	(0x20000000)	/* 512MB */

#endif
