/*

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef LISP_H
#define LISP_H

#define LowtagOf(obj) ((obj)&lowtag_Mask)
#define TypeOf(obj) ((obj)&type_Mask)
#define HeaderValue(obj) ((unsigned long) ((obj)>>type_Bits))

#define Pointerp(obj) ((obj) & 0x01)
#define PTR(obj) ((obj)&~lowtag_Mask)

#define CONS(obj) ((struct cons *)((obj)-type_ListPointer))
#define SYMBOL(obj) ((struct symbol *)((obj)-type_OtherPointer))
#define FDEFN(obj) ((struct fdefn *)((obj)-type_OtherPointer))

#if !defined alpha
typedef unsigned long lispobj;

#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__linux__)
typedef unsigned int u32;
typedef signed int s32;
#endif

#else
typedef unsigned int u32;
typedef signed int s32;
typedef u32 lispobj;
#endif

#define make_fixnum(n) ((lispobj)((n)<<(lowtag_Bits-1)))
#define fixnum_value(n) (((long)n)>>(lowtag_Bits-1))

#define boolean int
#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define SymbolValue(sym) \
    (((struct symbol *)((sym)-type_OtherPointer))->value)
#define SetSymbolValue(sym,val) \
    (((struct symbol *)((sym)-type_OtherPointer))->value = (val))

/* This only words for static symbols. */
#define SymbolFunction(sym) \
    (((struct fdefn *)(SymbolValue(sym)-type_OtherPointer))->function)

typedef enum {AUTO, X87, SSE2} fpu_mode_t;

#endif /* LISP_H */
