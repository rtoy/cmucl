/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/alloc.c,v 1.1 1990/02/24 19:37:10 wlott Exp $ */
#include "lisp.h"
#include "ldb.h"
#include "alloc.h"


/****************************************************************
Allocation Routines.
****************************************************************/

static char *alloc(bytes)
int bytes;
{
    char *result;

    /* Round to dual word boundry. */
    bytes = (bytes + lowtag_Mask) & ~lowtag_Mask;

    result = (char *)SymbolValue(SAVED_ALLOCATION_POINTER);
    SetSymbolValue(SAVED_ALLOCATION_POINTER, (lispobj)(result + bytes));

    return result;
}

char *alloc_unboxed(type, words)
int type, words;
{
    char *result = alloc((1 + words) * sizeof(long));

    *((long *)result) = (words << type_Bits) | type;

    return result;
}

lispobj alloc_vector(type, length, size)
int type, length, size;
{
    struct vector *result = (struct vector *)alloc((2 + (length*size + 31) / 32) * sizeof(long));

    result->header = type;
    result->length = fixnum(length);

    return ((lispobj)result)|type_OtherPointer;
}

lispobj alloc_cons(car, cdr)
lispobj car, cdr;
{
    struct cons *ptr = (struct cons *)alloc(sizeof(struct cons));

    ptr->car = car;
    ptr->cdr = cdr;

    return (lispobj)ptr | type_ListPointer;
}

lispobj alloc_number(n)
long n;
{
    struct bignum *ptr;

    if (-0x20000000 < n && n < 0x20000000)
        return fixnum(n);
    else {
        ptr = (struct bignum *)alloc_unboxed(type_Bignum, 1);

        ptr->digits[0] = n;
    }
}

lispobj alloc_string(str)
char *str;
{
    int len = strlen(str);
    lispobj result = alloc_vector(type_SimpleString, len+1, 8);
    struct vector *vec = (struct vector *)PTR(result);

    vec->length = fixnum(len);
    strcpy(vec->data, str);

    return result;
}

lispobj alloc_sap(ptr)
char *ptr;
{
    struct sap *sap = (struct sap *)alloc_unboxed(type_Sap, 2);

    sap->pointer = ptr;
}

