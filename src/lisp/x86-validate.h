/*
 *
 * This code was written as part of the CMU Common Lisp project at
 * Carnegie Mellon University, and has been placed in the public domain.
 *
 */

#ifndef X86_VALIDATE_H
#define X86_VALIDATE_H

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

#define CONTROL_STACK_END	(CONTROL_STACK_START + control_stack_size)

/* Note that GENCGC only uses dynamic_space 0. */
#define DYNAMIC_1_SPACE_START	(DYNAMIC_0_SPACE_START + DYNAMIC_SPACE_SIZE)

#endif /* X86_VALIDATE_H */
