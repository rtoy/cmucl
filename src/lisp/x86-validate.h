/*
 *
 * This code was written as part of the CMU Common Lisp project at
 * Carnegie Mellon University, and has been placed in the public domain.
 *
 */

#ifndef _X86_VALIDATE_H_
#define _X86_VALIDATE_H_

#if defined(__linux__)
#include "x86-validate-linux.h"
#endif

#if defined(DARWIN)
#include "x86-validate-darwin.h"
#endif

#if defined(SOLARIS)
#include "x86-validate-solaris.h"
#endif

#if defined(__NetBSD__)
#include "x86-validate-netbsd.h"
#endif

#if defined(__FreeBSD__)
#include "x86-validate-freebsd.h"
#endif

/* Note that GENCGC only uses dynamic_space 0. */
#define DYNAMIC_1_SPACE_START	(DYNAMIC_0_SPACE_START + DYNAMIC_SPACE_SIZE)

#endif /* _X86_VALIDATE_H_ */
