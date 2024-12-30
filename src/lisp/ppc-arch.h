/*

 This code was written as part of the CMU Common Lisp project and has
 been placed in the public domain.

*/
#ifndef PPC_ARCH_H
#define PPC_ARCH_H

/*
 * Define macro to allocate a local array of the appropriate size
 * where the fpu state can be stored.
 *
 * PPC has 32 (double-precision) floating-point registers.
 */
#define FPU_STATE_SIZE 32
#define FPU_STATE(name)    long long name[FPU_STATE_SIZE];

#endif
