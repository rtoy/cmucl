/*

 This code was written as part of the CMU Common Lisp project and has
 been placed in the public domain.

*/
#ifndef __X86_ARCH_H

extern int arch_support_sse2(void);
extern boolean os_support_sse2(void);

/*
 * Define macro to allocate a local array of the appropriate size
 * where the fpu state can be stored.
 */

#define FPU_STATE_SIZE 27

/* 
 * Need 512 byte area, aligned on a 16-byte boundary.  So allocate
 * 512+16 bytes of space and let the routine adjust the appropriate
 * alignment.
 */
#define SSE_STATE_SIZE ((512+16)/4)

/*
 * Just use the SSE size for both x87 and sse2 since the SSE size is
 * enough for either.
 */
#define FPU_STATE(name)    int name[SSE_STATE_SIZE];

#endif
