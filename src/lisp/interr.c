/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/interr.c,v 1.10 2009/06/11 16:04:01 rtoy Rel $
 *
 * Stuff to handle internal errors.
 *
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

#include "arch.h"
#include "signal.h"

#include "lisp.h"
#include "internals.h"
#include "interr.h"
#include "print.h"
#include "lispregs.h"


/* Lossage handler. */

static void
default_lossage_handler(void)
{
    exit(1);
}

static void (*lossage_handler) (void) = default_lossage_handler;

void
set_lossage_handler(void handler(void))
{
    lossage_handler = handler;
}

void
lose(char *fmt, ...)
{
    va_list ap;

    if (fmt != NULL) {
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	fflush(stderr);
	va_end(ap);
    }
    lossage_handler();
}


/* Internal error handler for when the Lisp error system doesn't exist. */

static char *errors[] = ERRORS;

void
internal_error(os_context_t * context)
{
    unsigned char *ptr = arch_internal_error_arguments(context);
    int len, scoffset, sc, offset, ch;

    len = *ptr++;
    printf("Error: %s\n", errors[*ptr++]);
    len--;
    while (len > 0) {
	scoffset = *ptr++;
	len--;
	if (scoffset == 253) {
	    scoffset = *ptr++;
	    len--;
	} else if (scoffset == 254) {
	    scoffset = ptr[0] + ptr[1] * 256;
	    ptr += 2;
	    len -= 2;
	} else if (scoffset == 255) {
	    scoffset = ptr[0] + (ptr[1] << 8) + (ptr[2] << 16) + (ptr[3] << 24);
	    ptr += 4;
	    len -= 4;
	}
	sc = scoffset & 0x1f;
	offset = scoffset >> 5;

	printf("    SC: %d, Offset: %d", sc, offset);
	switch (sc) {
	  case sc_AnyReg:
	  case sc_DescriptorReg:
	      putchar('\t');
	      brief_print(SC_REG(context, offset));
	      break;

	  case sc_BaseCharReg:
	      ch = SC_REG(context, offset);
#ifdef i386
	      if (offset & 1)
		  ch = ch >> 8;
	      ch = ch & 0xff;
#endif
	      switch (ch) {
		case '\n':
		    printf("\t'\\n'\n");
		    break;
		case '\b':
		    printf("\t'\\b'\n");
		    break;
		case '\t':
		    printf("\t'\\t'\n");
		    break;
		case '\r':
		    printf("\t'\\r'\n");
		    break;
		default:
		    if (ch < 32 || ch > 127)
			printf("\\%03o", ch);
		    else
			printf("\t'%c'\n", ch);
		    break;
	      }
	      break;
	  case sc_SapReg:
#ifdef sc_WordPointerReg
	  case sc_WordPointerReg:
#endif
	      printf("\t0x%08lx\n", SC_REG(context, offset));
	      break;
	  case sc_SignedReg:
	      printf("\t%ld\n", SC_REG(context, offset));
	      break;
	  case sc_UnsignedReg:
	      printf("\t%lu\n", SC_REG(context, offset));
	      break;
#if 0				/* broken */
#ifdef sc_SingleReg
	  case sc_SingleReg:
	      printf("\t%g\n", *(float *) &context->sc_fpregs[offset]);
	      break;
#endif
#ifdef sc_DoubleReg
	  case sc_DoubleReg:
	      printf("\t%g\n", *(double *) &context->sc_fpregs[offset]);
	      break;
#endif
#ifdef sc_LongReg
	  case sc_LongReg:
	      printf("\t%Lg\n", *(long double *) &context->sc_fpregs[offset]);
	      break;
#endif
#endif
	  default:
	      printf("\t???\n");
	      break;
	}
    }

    lose(NULL);
}




/* Utility routines used by random pieces of code. */

#if defined(UNICODE)

/* The Unicode replacement character code */
#define REPLACEMENT_CODE 0xfffd

/*
 * Convert a unicode code point to a set of utf8-encoded octets to
 * standard output.  This is the algorithm used by the Lisp utf8
 * encoder in src/code/extfmts.lisp.
 */
static void
utf8(int code, int len)
{
    int k;
    int j = 6 - len;
    int p = 6 * len;
    int init = 0xff & (0x7e << j);
    int c;

    /*
     * (ldb (byte j p) code): Extract j bits from position p of the code
     */
    c = (code >> p) & ((1 << j) - 1);
    
    putchar(init | c);

    for (k = 0; k < len; ++k) {
        p -= 6;
        /* (ldb (byte 6 p) code) */
        c = (code >> p) & ((1 << 6) - 1);
        putchar(128 | c);
    }
}

/*
 * Test if code is a surrogate.  Returns true if so. If the code is a
 * surrogate, then type indicates if it is a high (0) or low (1)
 * surrogate.  If not a surrogate, type is not modified.  If type is
 * NULL, then no type is returned.
 */
static boolean
surrogatep(int code, int *type)
{
    boolean result;

    if ((code >> 11) == 0x1b) {
        result = 1;
        if (type) {
            *type = (code >> 10) & 1;
        }
    } else {
        result = 0;
    }

    return result;
}

/*
 * Convert one or two utf16 code units into a code point.  utf16
 * points to the string, len is the length of the string.  The
 * codepoint is returned and the number of code units consumed is
 * returned in consumed.
 */
static int
utf16_codepoint(unsigned short int* utf16, int len, int* consumed)
{
    int codepoint = REPLACEMENT_CODE;
    int code_unit = *utf16;
    int code_type;
    int read = 1;

    /*
     * If the current code unit is not a surrogate, we're done.
     * Otherwise process the surrogate.  If this is a high (leading)
     * surrogate and the next code unit is a low (trailing) surrogate,
     * compute the code point.  Otherwise we have a bare surrogate or
     * an invalid surrogate sequence, so just return the replacement
     * character.
     */
    
    if (surrogatep(code_unit, &code_type)) {
        if (code_type == 0 && len > 0) {
            int next_unit = utf16[1];
            int next_type;
            if (surrogatep(next_unit, &next_type)) {
                if (next_type == 1) {
                    /* High followed by low surrogate */
                    codepoint = ((code_unit - 0xd800) << 10) + next_unit + 0x2400;
                    ++read;
                }
            }
        }
    } else {
        codepoint = code_unit;
    }

    *consumed = read;
    return codepoint;
}

/*
 * Send the utf-16 Lisp unicode string to standard output as a
 * utf8-encoded sequence of octets.
 */
static void
utf16_output(unsigned short int* utf16, int len)
{
    while (len) {
        int consumed;
        int code = utf16_codepoint(utf16, len, &consumed);

        /* Output the codepoint */
        if (code < 0x80) {
            putchar(code);
        } else if (code < 0x800) {
            utf8(code, 1);
        } else if (code < 0x10000) {
            utf8(code, 2);
        } else if (code < 0x110000) {
            utf8(code, 3);
        } else {
            /*
             * This shouldn't happen, but if it does we don't want to
             * signal any kind of error so just output a question mark
             * so we can continue.
             */
            putchar('?');
        }

        len -= consumed;
        utf16 += consumed;
    }
}
#endif

/*
 * debug_print is used by %primitive print to output a string.
 */
lispobj
debug_print(lispobj object)
{
    
#ifndef UNICODE
    printf("%s\n", (char *) (((struct vector *) PTR(object))->data));
    fflush(stdout);
#else    
    if (Pointerp(object)) {
        struct vector *lisp_string = (struct vector*) PTR(object);
        
        if ((unsigned long) lisp_string->header == type_SimpleString) {
            unsigned short int* lisp_chars;
            int len;

            len = lisp_string->length >> 2;
            lisp_chars = (unsigned short int*) lisp_string->data;

            utf16_output(lisp_chars, len);
            putchar('\n');
    
            fflush(stdout);
        } else {
            print(object);
        }
    } else {
        /*
         * We shouldn't actually ever get here because %primitive
         * print is only supposed to take strings.  But if we do, it's
         * useful to print something out anyway.
         */
#if 1
	printf("obj @0x%lx: ", (unsigned long) object);
#endif
        print(object);
    }
#endif            
    return NIL;
}

