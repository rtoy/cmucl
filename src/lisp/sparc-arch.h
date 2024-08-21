/*

 This code was written as part of the CMU Common Lisp project and has
 been placed in the public domain.

*/
#ifndef SPARC_ARCH_H
#define SPARC_ARCH_H

/*
 * Define macro to allocate a local array of the appropriate size
 * where the fpu state can be stored.
 *
 *
 * 32 (single-precision) FP registers, and the FP state register.
 * But Sparc V9 has 32 double-precision registers (equivalent to 64
 * single-precision, but can't be accessed), so we leave enough room
 * for that.
 */
#define FPU_STATE_SIZE (((32 + 32 + 1) + 1)/2)
#define FPU_STATE(name)    long long name[FPU_STATE_SIZE];

#endif
