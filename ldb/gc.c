/*
 * Stop and Copy GC based on Cheney's algorithm.
 *
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/ldb/Attic/gc.c,v 1.3 1990/04/02 00:30:22 ch Exp $
 * 
 * Written by Christopher Hoover.
 */

#include <stdio.h>
#include <mach.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <signal.h>
#include "lisp.h"
#include "ldb.h"
#include "gc.h"
#include "globals.h"
#include "interrupt.h"
#include "validate.h"
#include "lispregs.h"

lispobj *from_space;
lispobj *from_space_free_pointer;

lispobj *new_space;
lispobj *new_space_free_pointer;

static int (*scavtab[256])();
static lispobj (*transother[256])();
static int (*sizetab[256])();


/* Predicates */

#if defined(DEBUG_SPACE_PREDICATES)

from_space_p(object)
lispobj object;
{
	lispobj *ptr;

	gc_assert(pointerp(object));

	ptr = (lispobj *) PTR(object);

	return ((from_space <= ptr) &&
		(ptr < from_space_free_pointer));
}	    

new_space_p(object)
lispobj object;
{
	lispobj *ptr;

	gc_assert(pointerp(object));

	ptr = (lispobj *) PTR(object);
		
	return ((new_space <= ptr) &&
		(ptr < new_space_free_pointer));
}	    

#else

#define from_space_p(ptr) \
	((from_space <= ((lispobj *) ptr)) && \
	 (((lispobj *) ptr) < from_space_free_pointer))

#define new_space_p(ptr) \
	((new_space <= ((lispobj *) ptr)) && \
	 (((lispobj *) ptr) < new_space_free_pointer))

#endif


/* GC Lossage */

void
gc_lose()
{
	exit(1);
}


/* Copying Objects */

#if defined(DEBUG_USE_ANAL_BCOPY)

static bcopy(src, dest, length)
char *src, *dest;
int length;
{
	while (length-- > 0) {
		if (*dest != 0) {
			fprintf(stderr, "GC lossage.  Copying into non-zero memory!\n");
			gc_lose();
		}
		*dest++ = *src++;
	}
}

#endif

static lispobj
copy_object(object, nwords)
lispobj object;
int nwords;
{
	int tag;
	lispobj *new;

	gc_assert(pointerp(object));
	gc_assert(from_space_p(object));
	gc_assert((nwords & 0x01) == 0);

	/* get tag of object */
	tag = LowtagOf(object);

	/* allocate space */
	new = new_space_free_pointer;
	new_space_free_pointer += nwords;

	/* copy object */
	bcopy((char *) PTR(object), (char *) new, nwords * 4);

	/* return lisp pointer of new object */
	return ((lispobj) new) | tag;
}


/* Collect Garbage */

static double tv_diff(x, y)
struct timeval *x, *y;
{
    return (((double) x->tv_sec + (double) x->tv_usec * 1.0e-6) -
	    ((double) y->tv_sec + (double) y->tv_usec * 1.0e-6));
}

collect_garbage()
{
	struct timeval start_tv, stop_tv;
	struct rusage start_rusage, stop_rusage;
	double real_time, system_time, user_time;
	lispobj *current_static_space_free_pointer;
	long static_space_size;
	long control_stack_size, binding_stack_size;
	long size_retained, size_discarded;
	int oldmask;
	
	getrusage(RUSAGE_SELF, &start_rusage);
	gettimeofday(&start_tv, (struct timezone *) 0);

	printf("[Collecting garbage ... \n");

	oldmask = sigblock(BLOCKABLE);

	current_static_space_free_pointer =
		(lispobj *) SymbolValue(STATIC_SPACE_FREE_POINTER);


	/* Set up from space and new space pointers */

	from_space = current_dynamic_space;
	from_space_free_pointer = current_dynamic_space_free_pointer;

	if (current_dynamic_space == dynamic_0_space)
		new_space = dynamic_1_space;
	else if (current_dynamic_space == dynamic_1_space)
		new_space = dynamic_0_space;
	else {
		fprintf(stderr, "GC lossage.  Current dynamic space is bogus!\n");
		gc_lose();
	}

	os_validate(new_space, DYNAMIC_SPACE_SIZE);
	new_space_free_pointer = new_space;


	/* Scavenge the roots */

	printf("Scavenging interrupt contexts ...\n");
	scavenge_interrupt_contexts();

	printf("Scavenging interrupt handlers (%d bytes) ...\n",
	       sizeof(interrupt_handlers));
	scavenge((lispobj *) interrupt_handlers,
		 sizeof(interrupt_handlers) / sizeof(lispobj));

	control_stack_size = current_control_stack_pointer - control_stack;
	printf("Scavenging the control stack (%d bytes) ...\n",
	       control_stack_size * sizeof(lispobj));
	scavenge(control_stack, control_stack_size);

	binding_stack_size = current_binding_stack_pointer - binding_stack;
	printf("Scavenging the binding stack (%d bytes) ...\n",
	       binding_stack_size * sizeof(lispobj));
	scavenge(binding_stack, binding_stack_size);

	static_space_size = current_static_space_free_pointer - static_space;
	printf("Scavenging static space (%d bytes) ...\n",
	       static_space_size * sizeof(lispobj));
	scavenge(static_space, static_space_size);

	printf("Scavenging new space ...\n");
	scavenge_newspace();

#if defined(DEBUG_PRINT_GARBAGE)
	print_garbage(from_space, from_space_free_pointer);
#endif

	/* Flip spaces */
	os_invalidate(current_dynamic_space, DYNAMIC_SPACE_SIZE);
	current_dynamic_space = new_space;
	current_dynamic_space_free_pointer = new_space_free_pointer;

	size_discarded = (from_space_free_pointer - from_space) * sizeof(lispobj);
	size_retained = (new_space_free_pointer - new_space) * sizeof(lispobj);

	(void) sigsetmask(oldmask);

	printf("done.]\n");

	printf("Total of %d bytes out of %d bytes retained (%3.2f%%).\n",
	       size_retained, size_discarded,
	       (((float) size_retained) / ((float) size_discarded)) * 100.0);

	gettimeofday(&stop_tv, (struct timezone *) 0);
	getrusage(RUSAGE_SELF, &stop_rusage);

	real_time = tv_diff(&stop_tv, &start_tv);
	user_time = tv_diff(&stop_rusage.ru_utime, &start_rusage.ru_utime);
	system_time = tv_diff(&stop_rusage.ru_stime, &start_rusage.ru_stime);

	printf("Statistics:\n");
	printf("%10.2f msec of real time\n", real_time * 1000.0);
	printf("%10.2f msec of user time,\n", user_time * 1000.0);
	printf("%10.2f msec of system time.\n", system_time * 1000.0);

	printf("%10.2f M bytes/sec collected.\n",
	       (((float) size_retained / (float) (1<<20)) / real_time));
}


/* Scavenging */

static
scavenge(start, nwords)
lispobj *start;
long nwords;
{
	while (nwords > 0) {
		lispobj object;
		int type, words_scavenged;

		object = *start;
		type = TypeOf(object);

#if defined(DEBUG_SCAVENGE_VERBOSE)
		printf("Scavenging object at 0x%08x, object = 0x%08x, type = %d\n",
		       (unsigned long) start, (unsigned long) object, type);
#endif

		words_scavenged = (scavtab[type])(start, object);

		start += words_scavenged;
		nwords -= words_scavenged;
	}
	gc_assert(nwords == 0);
}

static
scavenge_newspace()
{
	lispobj *here;

	here = new_space;
	while (here < new_space_free_pointer) {
		lispobj object;
		int type, words_scavenged;

		object = *here;
		type = TypeOf(object);

#if defined(DEBUG_SCAVENGE_VERBOSE)
		printf("Scavenging object at 0x%08x, object = 0x%08x, type = %d\n",
		       (unsigned long) here, (unsigned long) object, type);
#endif

		words_scavenged = (scavtab[type])(here, object);

		here += words_scavenged;
	}
	gc_assert(here == new_space_free_pointer);
}


/* Scavenging Interrupt Contexts */

scavenge_interrupt_contexts()
{
	int i, index;
	struct sigcontext *context;

	index = FIXNUM_TO_INT(SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX));
#if defined(DEBUG_PRINT_CONTEXT_INDEX)
	printf("Number of active contexts: %d\n", index);
#endif

	for (i = 0; i < index; i++) {
		context = lisp_interrupt_contexts[i];
		scavenge_interrupt_context(context); 
	}
}

static int boxed_registers[] = {
	A0, A1, A2, A3, A4, A5, CNAME, LEXENV,
	ARGS, OLDCONT, LRA, L0, L1, L2, CODE
};

scavenge_interrupt_context(context)
struct sigcontext *context;
{
	int i;
	unsigned long lip;
	unsigned long lip_offset;
	int lip_register_pair;
	unsigned long pc_code_offset;

	/* Find the LIP's register pair and calculate it's offset */
	/* before we scavenge the context. */
	lip = context->sc_regs[LIP];
	lip_offset = 0xFFFFFFFF;
	lip_register_pair = -1;
	for (i = 0; i < (sizeof(boxed_registers) / sizeof(int)); i++) {
		unsigned long reg, offset;
		int index;

		index = boxed_registers[i];
		reg = context->sc_regs[index];
		if (reg <= lip) {
			offset = lip - reg;
			if (offset < lip_offset) {
				lip_offset = offset;
				lip_register_pair = index;
			}
		}
	}

#if defined(DEBUG_LIP)
	printf("LIP = %08x, Pair is R%d = %08x, Offset = %08x\n",
	       context->sc_regs[LIP],
	       lip_register_pair,
	       context->sc_regs[lip_register_pair],
	       lip_offset);
#endif

	/* Compute the PC's offset from the start of the CODE */
	/* register. */
	pc_code_offset = context->sc_pc - context->sc_regs[CODE];

#if defined(DEBUG_PC)
	printf("PC = %08x, CODE = %08x, Offset = %08x\n",
	       context->sc_pc, context->sc_regs[CODE], pc_code_offset);
#endif
	       
	/* Scanvenge all boxed registers in the context. */
	for (i = 0; i < (sizeof(boxed_registers) / sizeof(int)); i++) {
		int index;
		unsigned long reg;

		index = boxed_registers[i];
		reg = context->sc_regs[index];
		scavenge((lispobj *) &(context->sc_regs[index]), 1);
#if defined(DEBUG_SCAVENGE_REGISTERS)
		printf("Scavenged R%d: was 0x%08x now 0x%08x\n",
		       index, reg, context->sc_regs[index]);
#endif
	}

	/* Fix the LIP */
	context->sc_regs[LIP] =
		context->sc_regs[lip_register_pair] + lip_offset;
	
#if defined(DEBUG_LIP)
	printf("LIP = %08x, Pair is R%d = %08x, Offset = %08x\n",
	       context->sc_regs[LIP],
	       lip_register_pair,
	       context->sc_regs[lip_register_pair],
	       lip_offset);
#endif

	/* Fix the PC if it was in from space */
	if (from_space_p(context->sc_pc))
		context->sc_pc = context->sc_regs[CODE] + pc_code_offset;

#if defined(DEBUG_PC)
	printf("PC = %08x, CODE = %08x, Offset = %08x\n",
	       context->sc_pc, context->sc_regs[CODE], pc_code_offset);
#endif
	       
}


/* Debugging Code */

print_garbage(from_space, from_space_free_pointer)
lispobj *from_space, *from_space_free_pointer;
{
	lispobj *start;
	int total_words_not_copied;

	printf("Scanning from space ...\n");

	total_words_not_copied = 0;
	start = from_space;
	while (start < from_space_free_pointer) {
		lispobj object;
		int forwardp, type, nwords;
		lispobj header;

		object = *start;
		forwardp = pointerp(object) && new_space_p(object);

		if (forwardp) {
			int tag;
			lispobj *pointer;

			tag = LowtagOf(object);

			switch (tag) {
			case type_ListPointer:
				nwords = 2;
				break;
			case type_StructurePointer:
				printf("Don't know about structures yet!\n");
				nwords = 1;
				break;
			case type_FunctionPointer:
				nwords = 1;
				break;
			case type_OtherPointer:
				pointer = (lispobj *) PTR(object);
				header = *pointer;
				type = TypeOf(header);
				nwords = (sizetab[type])(pointer);
			}
		} else {
			type = TypeOf(object);
			nwords = (sizetab[type])(start);
			total_words_not_copied += nwords;
			printf("%4d words not copied at 0x%08x; ",
			       nwords, (unsigned long) start);
			printf("Header word is 0x%08x\n", (unsigned long) object);
		}
		start += nwords;
	}
	printf("%d total words not copied.\n", total_words_not_copied);
}


/* Code and Code-Related Objects */

static lispobj trans_function_header();
static lispobj trans_closure_function_header();

static
scav_function_pointer(where, object)
lispobj *where, object;
{
	gc_assert(pointerp(object));

	if (from_space_p(object)) {
		lispobj first, *first_pointer;

		/* object is a pointer into from space.  check to see */
		/* if it has been forwarded */
		first_pointer = (lispobj *) PTR(object);
		first = *first_pointer;
		
		if (!(pointerp(first) && new_space_p(first))) {
			int type;

			/* must transport object -- object may point */
			/* to either a function header or to a closure */
			/* header. */
			
			type = TypeOf(first);
			if (type == type_FunctionHeader)
				first = *first_pointer = trans_function_header(object);
			else if (type == type_ClosureHeader)
				first = *first_pointer = trans_closure_function_header(object);
			else {
				fprintf(stderr, "GC lossage.  Bogus function pointer.\n");
				fprintf(stderr, "Pointer: 0x%08x, Header: 0x%08x\n",
					(unsigned long) object, (unsigned long) first);
				gc_lose();
			}
		}

		gc_assert(pointerp(first));
		gc_assert(!from_space_p(first));

		*where = first;
	}
	return 1;
}

static struct code *
trans_code(code)
struct code *code;
{
	struct code *new_code;
	lispobj first, l_code, l_new_code;
	int nheader_words, ncode_words, nwords;
	unsigned long displacement;
	lispobj fheaderl, *prev_pointer;

#if defined(DEBUG_CODE_GC)
	printf("\nTransporting code object located at 0x%08x.\n",
	       (unsigned long) code);
#endif

	/* if object has already been transported, just return pointer */
	first = code->header;
	if (pointerp(first) && new_space_p(first))
		return (struct code *) PTR(first);
	
	gc_assert(TypeOf(first) == type_CodeHeader);

	/* prepare to transport the code vector */
	l_code = (lispobj) code | type_OtherPointer;

	ncode_words = FIXNUM_TO_INT(code->code_size);
	nheader_words = HEADER_VALUE(code->header);
	nwords = ncode_words + nheader_words;
	nwords = CEILING(nwords, 2);

	l_new_code = copy_object(l_code, nwords);
	new_code = (struct code *) PTR(l_new_code);

	displacement = l_new_code - l_code;

#if defined(DEBUG_CODE_GC)
	printf("Old code object at 0x%08x, new code object at 0x%08x.\n",
	       (unsigned long) code, (unsigned long) new_code);
	printf("Code object is %d words long.\n", nwords);
#endif

	/* set forwarding pointer */
	code->header = l_new_code;
	
	/* set forwarding pointers for all the function headers in the */
	/* code object.  also fix all self pointers */

	fheaderl = code->entry_points;
	prev_pointer = &new_code->entry_points;

	while (fheaderl != NIL) {
		struct function_header *fheaderp, *nfheaderp;
		lispobj nfheaderl, header;
		
		fheaderp = (struct function_header *) PTR(fheaderl);
		header = fheaderp->header;
		gc_assert(TypeOf(header) == type_FunctionHeader);

		/* calcuate the new function pointer and the new */
		/* function header */
		nfheaderl = fheaderl + displacement;
		nfheaderp = (struct function_header *) PTR(nfheaderl);

		/* set forwarding pointer */
		fheaderp->header = nfheaderl;
		
		/* fix self pointer */
		nfheaderp->self = nfheaderl;

		*prev_pointer = nfheaderl;

		fheaderl = fheaderp->next;
		prev_pointer = &nfheaderp->next;
	}

	return new_code;
}

static
scav_code_header(where, object)
lispobj *where, object;
{
	struct code *code;
	int nheader_words, ncode_words, nwords;
	lispobj fheaderl;
	struct function_header *fheaderp;

	code = (struct code *) where;
	ncode_words = FIXNUM_TO_INT(code->code_size);
	nheader_words = HEADER_VALUE(object);
	nwords = ncode_words + nheader_words;
	nwords = CEILING(nwords, 2);

#if defined(DEBUG_CODE_GC)
	printf("\nScavening code object at 0x%08x.\n",
	       (unsigned long) where);
	printf("Code object is %d words long.\n", nwords);
	printf("Scavenging boxed section of code data block (%d words).\n",
	       nheader_words - 1);
#endif

	/* Scavenge the boxed section of the code data block */
	scavenge(where + 1, nheader_words - 1);

	/* Scavenge the boxed section of each function object in the */
	/* code data block */
	fheaderl = code->entry_points;
	while (fheaderl != NIL) {
		lispobj header;
		
		fheaderp = (struct function_header *) PTR(fheaderl);
		header = fheaderp->header;
		gc_assert(TypeOf(header) == type_FunctionHeader);
		
#if defined(DEBUG_CODE_GC)
		printf("Scavenging boxed section of entry point located at 0x%08x.\n",
		       (unsigned long) PTR(fheaderl));
#endif
		scavenge(&fheaderp->name, 1);
		scavenge(&fheaderp->arglist, 1);
		scavenge(&fheaderp->type, 1);
		
		fheaderl = fheaderp->next;
	}
	
	return nwords;
}

static lispobj
trans_code_header(object)
lispobj object;
{
	struct code *ncode;

	ncode = trans_code((struct code *) PTR(object));
	return (lispobj) ncode | type_OtherPointer;
}

static
size_code_header(where)
lispobj *where;
{
	struct code *code;
	int nheader_words, ncode_words, nwords;

	code = (struct code *) where;
	
	ncode_words = FIXNUM_TO_INT(code->code_size);
	nheader_words = HEADER_VALUE(code->header);
	nwords = ncode_words + nheader_words;
	nwords = CEILING(nwords, 2);

	return nwords;
}


static
scav_return_pc_header(where, object)
lispobj *where, object;
{
	fprintf(stderr, "GC lossage.  Should not be scavenging a ");
	fprintf(stderr, "Return PC Header.\n");
	fprintf(stderr, "where = 0x%08x, object = 0x%08x",
		(unsigned long) where, (unsigned long) object);
	gc_lose();
}

static lispobj
trans_return_pc_header(object)
lispobj object;
{
	struct function_header *return_pc;
	unsigned long offset;
	struct code *code, *ncode;
	
	return_pc = (struct function_header *) PTR(object);
	offset = HEADER_VALUE(return_pc->header) * 4;

	/* Transport the whole code object */
	code = (struct code *) ((unsigned long) return_pc - offset);
	ncode = trans_code(code);

	return ((lispobj) ncode + offset) | type_OtherPointer;
}


static
scav_function_header(where, object)
lispobj *where, object;
{
	fprintf(stderr, "GC lossage.  Should not be scavenging a ");
	fprintf(stderr, "Function Header.\n");
	fprintf(stderr, "where = 0x%08x, object = 0x%08x",
		(unsigned long) where, (unsigned long) object);
	gc_lose();
}

static lispobj
trans_function_header(object)
lispobj object;
{
	struct function_header *fheader;
	unsigned long offset;
	struct code *code, *ncode;
	
	fheader = (struct function_header *) PTR(object);
	offset = HEADER_VALUE(fheader->header) * 4;

	/* Transport the whole code object */
	code = (struct code *) ((unsigned long) fheader - offset);
	ncode = trans_code(code);

	return ((lispobj) ncode + offset) | type_FunctionPointer;
}


static
scav_closure_function_header(where, object)
lispobj *where, object;
{
	fprintf(stderr, "GC lossage.  Should not be scavenging a ");
	fprintf(stderr, "Closure Function Header.\n");
	fprintf(stderr, "where = 0x%08x, object = 0x%08x",
		(unsigned long) where, (unsigned long) object);
	gc_lose();
}

static lispobj
trans_closure_function_header(object)
lispobj object;
{
	struct function_header *fheader;
	unsigned long offset;
	struct code *code, *ncode;
	
	fheader = (struct function_header *) PTR(object);
	offset = HEADER_VALUE(fheader->header) * 4;

	/* Transport the whole code object */
	code = (struct code *) ((unsigned long) fheader - offset);
	ncode = trans_code(code);

	return ((lispobj) ncode + offset) | type_FunctionPointer;
}


/* Structures */

static
scav_structure_pointer(where, object)
lispobj *where, object;
{
	if (from_space_p(object)) {
		/* ### I don't know how to transport these! */

		fprintf(stderr, "GC lossage.  Cannot scavenge structure pointer in from space!\n");
		gc_lose();
	} else
		return 1;
}


/* Lists and Conses */

static lispobj trans_list();

static
scav_list_pointer(where, object)
lispobj *where, object;
{
	gc_assert(pointerp(object));

	if (from_space_p(object)) {
		lispobj first, *first_pointer;

		/* object is a pointer into from space.  check to see */
		/* if it has been forwarded */
		first_pointer = (lispobj *) PTR(object);
		first = *first_pointer;
		
		if (!(pointerp(first) && new_space_p(first)))
			first = *first_pointer = trans_list(object);

		gc_assert(pointerp(first));
		gc_assert(!from_space_p(first));
	
		*where = first;
	}
	return 1;
}

static lispobj
trans_list(object)
lispobj object;
{
	lispobj new_list_pointer;
	struct cons *cons, *new_cons;
	
	cons = (struct cons *) PTR(object);

	/* ### Don't use copy_object here */
	new_list_pointer = copy_object(object, 2);
	new_cons = (struct cons *) PTR(new_list_pointer);

	/* Set forwarding pointer */
	cons->car = new_list_pointer;
	
	/* Try to linearize the list in the cdr direction to help reduce */
	/* paging. */

	while (1) {
		lispobj cdr, new_cdr;
		struct cons *cdr_cons, *new_cdr_cons;

		cdr = cons->cdr;

		if (!((LowtagOf(cdr) == type_ListPointer) && from_space_p(cdr)))
			break;

		cdr_cons = (struct cons *) PTR(cdr);

		/* ### Don't use copy_object here */
		new_cdr = copy_object(cdr, 2);
		new_cdr_cons = (struct cons *) PTR(new_cdr);

		/* Set forwarding pointer */
		cdr_cons->car = new_cdr;

		/* Update the cdr of the last cons copied into new */
		/* space to keep the newspace scavenge from having to */
		/* do it. */
		new_cons->cdr = new_cdr;
		
		cons = cdr_cons;
		new_cons = new_cdr_cons;
	}

	return new_list_pointer;
}


/* Scavenging and Transporting Other Pointers */

static
scav_other_pointer(where, object)
lispobj *where, object;
{
	gc_assert(pointerp(object));

	if (from_space_p(object)) {
		lispobj first, *first_pointer;

		/* object is a pointer into from space.  check to see */
		/* if it has been forwarded */
		first_pointer = (lispobj *) PTR(object);
		first = *first_pointer;
		
		if (!(pointerp(first) && new_space_p(first)))
			first = *first_pointer = 
				(transother[TypeOf(first)])(object);

		gc_assert(pointerp(first));
		gc_assert(!from_space_p(first));

		*where = first;
	}
	return 1;
}


/* Immediate, Boxed, and Unboxed Objects */

static
size_pointer(where)
lispobj *where;
{
	return 1;
}


static
scav_immediate(where, object)
lispobj *where, object;
{
	return 1;
}

static lispobj
trans_immediate(object)
lispobj object;
{
	fprintf(stderr, "GC lossage.  Trying to transport an immediate!?\n");
	gc_lose();
}

static
size_immediate(where)
lispobj *where;
{
	return 1;
}


static
scav_boxed(where, object)
lispobj *where, object;
{
	return 1;
}

static lispobj
trans_boxed(object)
lispobj object;
{
	lispobj header;
	unsigned long length;

	gc_assert(pointerp(object));

	header = *((lispobj *) PTR(object));
	length = HEADER_VALUE(header) + 1;
	length = CEILING(length, 2);

	return copy_object(object, length);
}

static
size_boxed(where)
lispobj *where;
{
	lispobj header;
	unsigned long length;

	header = *where;
	length = HEADER_VALUE(header) + 1;
	length = CEILING(length, 2);

	return length;
}


static
scav_unboxed(where, object)
lispobj *where, object;
{
	unsigned long length;

	length = HEADER_VALUE(object) + 1;
	length = CEILING(length, 2);

	return length;
}

static lispobj
trans_unboxed(object)
lispobj object;
{
	lispobj header;
	unsigned long length;


	gc_assert(pointerp(object));

	header = *((lispobj *) PTR(object));
	length = HEADER_VALUE(header) + 1;
	length = CEILING(length, 2);

	return copy_object(object, length);
}

static
size_unboxed(where)
lispobj *where;
{
	lispobj header;
	unsigned long length;

	header = *where;
	length = HEADER_VALUE(header) + 1;
	length = CEILING(length, 2);

	return length;
}


/* Vector-Like Objects */

#define NWORDS(x,y) (CEILING((x),(y)) / (y))

static
scav_string(where, object)
lispobj *where, object;
{
	struct vector *vector;
	int length, nwords;

	/* NOTE: Strings contain one more byte of data than the length */
	/* slot indicates. */

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length) + 1;
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return nwords;
}

static lispobj
trans_string(object)
{
	struct vector *vector;
	int length, nwords;

	gc_assert(pointerp(object));

	/* NOTE: Strings contain one more byte of data than the length */
	/* slot indicates. */

	vector = (struct vector *) PTR(object);
	length = FIXNUM_TO_INT(vector->length) + 1;
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return copy_object(object, nwords);
}

static
size_string(where)
lispobj *where;
{
	struct vector *vector;
	int length, nwords;

	/* NOTE: Strings contain one more byte of data than the length */
	/* slot indicates. */

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length) + 1;
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return nwords;
}


static lispobj
trans_vector(object)
lispobj object;
{
	struct vector *vector;
	int length, nwords;

	gc_assert(pointerp(object));

	vector = (struct vector *) PTR(object);
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(length + 2, 2);

	return copy_object(object, nwords);
}

static
size_vector(where)
lispobj *where;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(length + 2, 2);

	return nwords;
}


static
scav_vector_bit(where, object)
lispobj *where, object;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 32) + 2, 2);

	return nwords;
}

static lispobj
trans_vector_bit(object)
lispobj object;
{
	struct vector *vector;
	int length, nwords;

	gc_assert(pointerp(object));

	vector = (struct vector *) PTR(object);
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 32) + 2, 2);

	return copy_object(object, nwords);
}

static
size_vector_bit(where)
lispobj *where;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 32) + 2, 2);

	return nwords;
}


static
scav_vector_unsigned_byte_2(where, object)
lispobj *where, object;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 16) + 2, 2);

	return nwords;
}

static lispobj
trans_vector_unsigned_byte_2(object)
lispobj object;
{
	struct vector *vector;
	int length, nwords;

	gc_assert(pointerp(object));

	vector = (struct vector *) PTR(object);
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 16) + 2, 2);

	return copy_object(object, nwords);
}

static
size_vector_unsigned_byte_2(where)
lispobj *where;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 16) + 2, 2);

	return nwords;
}


static
scav_vector_unsigned_byte_4(where, object)
lispobj *where, object;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 8) + 2, 2);

	return nwords;
}

static lispobj
trans_vector_unsigned_byte_4(object)
lispobj object;
{
	struct vector *vector;
	int length, nwords;

	gc_assert(pointerp(object));

	vector = (struct vector *) PTR(object);
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 8) + 2, 2);

	return copy_object(object, nwords);
}

static
size_vector_unsigned_byte_4(where, object)
lispobj *where, object;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 8) + 2, 2);

	return nwords;
}


static
scav_vector_unsigned_byte_8(where, object)
lispobj *where, object;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return nwords;
}

static lispobj
trans_vector_unsigned_byte_8(object)
lispobj object;
{
	struct vector *vector;
	int length, nwords;

	gc_assert(pointerp(object));

	vector = (struct vector *) PTR(object);
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return copy_object(object, nwords);
}

static
size_vector_unsigned_byte_8(where)
lispobj *where;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 4) + 2, 2);

	return nwords;
}


static
scav_vector_unsigned_byte_16(where, object)
lispobj *where, object;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 2) + 2, 2);

	return nwords;
}

static lispobj
trans_vector_unsigned_byte_16(object)
lispobj object;
{
	struct vector *vector;
	int length, nwords;

	gc_assert(pointerp(object));

	vector = (struct vector *) PTR(object);
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 2) + 2, 2);

	return copy_object(object, nwords);
}

static
size_vector_unsigned_byte_16(where)
lispobj *where;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(NWORDS(length, 2) + 2, 2);

	return nwords;
}


static
scav_vector_unsigned_byte_32(where, object)
lispobj *where, object;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(length + 2, 2);

	return nwords;
}

static lispobj
trans_vector_unsigned_byte_32(object)
lispobj object;
{
	struct vector *vector;
	int length, nwords;

	gc_assert(pointerp(object));

	vector = (struct vector *) PTR(object);
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(length + 2, 2);

	return copy_object(object, nwords);
}

static
size_vector_unsigned_byte_32(where)
lispobj *where;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(length + 2, 2);

	return nwords;
}


static
scav_vector_single_float(where, object)
lispobj *where, object;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(length + 2, 2);

	return nwords;
}

static lispobj
trans_vector_single_float(object)
lispobj object;
{
	struct vector *vector;
	int length, nwords;

	gc_assert(pointerp(object));

	vector = (struct vector *) PTR(object);
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(length + 2, 2);

	return copy_object(object, nwords);
}

static
size_vector_single_float(where)
lispobj *where;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(length + 2, 2);

	return nwords;
}


static
scav_vector_double_float(where, object)
lispobj *where, object;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(length * 2 + 2, 2);

	return nwords;
}

static lispobj
trans_vector_double_float(object)
lispobj object;
{
	struct vector *vector;
	int length, nwords;

	gc_assert(pointerp(object));

	vector = (struct vector *) PTR(object);
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(length * 2 + 2, 2);

	return copy_object(object, nwords);
}

static
size_vector_double_float(where)
lispobj *where;
{
	struct vector *vector;
	int length, nwords;

	vector = (struct vector *) where;
	length = FIXNUM_TO_INT(vector->length);
	nwords = CEILING(length * 2 + 2, 2);

	return nwords;
}


/* Initialization */

static
scav_lose(object)
lispobj object;
{
	fprintf(stderr, "GC lossage.  No scavenge function for object 0x%08x\n",
		(unsigned long) object);
	gc_lose();
}

static lispobj
trans_lose(object)
lispobj object;
{
	fprintf(stderr, "GC lossage.  No transport function for object 0x%08x\n",
		(unsigned long) object);
	gc_lose();
}

static
size_lose(where)
lispobj *where;
{
	fprintf(stderr, "Size lossage.  No size function for object at 0x%08x\n",
		(unsigned long) where);
	fprintf(stderr, "First word of object: 0x%08x\n",
		(unsigned long) *where);
	return 1;
}

gc_init()
{
	int i;

	/* Scavenge Table */
	for (i = 0; i < 256; i++)
		scavtab[i] = scav_lose;

	for (i = 0; i < 32; i++) {
		scavtab[type_EvenFixnum|(i<<3)] = scav_immediate;
		scavtab[type_FunctionPointer|(i<<3)] = scav_function_pointer;
		/* OtherImmediate0 */
		scavtab[type_ListPointer|(i<<3)] = scav_list_pointer;
		scavtab[type_OddFixnum|(i<<3)] = scav_immediate;
		scavtab[type_StructurePointer|(i<<3)] = scav_structure_pointer;
		/* OtherImmediate1 */
		scavtab[type_OtherPointer|(i<<3)] = scav_other_pointer;
	}

	scavtab[type_Bignum] = scav_unboxed;
	scavtab[type_Ratio] = scav_boxed;
	scavtab[type_SingleFloat] = scav_unboxed;
	scavtab[type_DoubleFloat] = scav_unboxed;
	scavtab[type_Complex] = scav_boxed;
	scavtab[type_SimpleArray] = scav_boxed;
	scavtab[type_SimpleString] = scav_string;
	scavtab[type_SimpleBitVector] = scav_vector_bit;
	scavtab[type_SimpleVector] = scav_boxed;
	scavtab[type_SimpleArrayUnsignedByte2] = scav_vector_unsigned_byte_2;
	scavtab[type_SimpleArrayUnsignedByte4] = scav_vector_unsigned_byte_4;
	scavtab[type_SimpleArrayUnsignedByte8] = scav_vector_unsigned_byte_8;
	scavtab[type_SimpleArrayUnsignedByte16] = scav_vector_unsigned_byte_16;
	scavtab[type_SimpleArrayUnsignedByte32] = scav_vector_unsigned_byte_32;
	scavtab[type_SimpleArraySingleFloat] = scav_vector_single_float;
	scavtab[type_SimpleArrayDoubleFloat] = scav_vector_double_float;
	scavtab[type_ComplexString] = scav_boxed;
	scavtab[type_ComplexBitVector] = scav_boxed;
	scavtab[type_ComplexVector] = scav_boxed;
	scavtab[type_ComplexArray] = scav_boxed;
	scavtab[type_CodeHeader] = scav_code_header;
	scavtab[type_FunctionHeader] = scav_function_header;
	scavtab[type_ClosureFunctionHeader] = scav_closure_function_header;
	scavtab[type_ReturnPcHeader] = scav_return_pc_header;
	scavtab[type_ClosureHeader] = scav_boxed;
	scavtab[type_ValueCellHeader] = scav_boxed;
	scavtab[type_SymbolHeader] = scav_boxed;
	scavtab[type_BaseCharacter] = scav_immediate;
	scavtab[type_Sap] = scav_unboxed;
	scavtab[type_UnboundMarker] = scav_immediate;


	/* Transport Other Table */
	for (i = 0; i < 256; i++)
		transother[i] = trans_lose;

	transother[type_Bignum] = trans_unboxed;
	transother[type_Ratio] = trans_boxed;
	transother[type_SingleFloat] = trans_unboxed;
	transother[type_DoubleFloat] = trans_unboxed;
	transother[type_Complex] = trans_boxed;
	transother[type_SimpleArray] = trans_boxed;
	transother[type_SimpleString] = trans_string;
	transother[type_SimpleBitVector] = trans_vector_bit;
	transother[type_SimpleVector] = trans_vector;
	transother[type_SimpleArrayUnsignedByte2] = trans_vector_unsigned_byte_2;
	transother[type_SimpleArrayUnsignedByte4] = trans_vector_unsigned_byte_4;
	transother[type_SimpleArrayUnsignedByte8] = trans_vector_unsigned_byte_8;
	transother[type_SimpleArrayUnsignedByte16] = trans_vector_unsigned_byte_16;
	transother[type_SimpleArrayUnsignedByte32] = trans_vector_unsigned_byte_32;
	transother[type_SimpleArraySingleFloat] = trans_vector_single_float;
	transother[type_SimpleArrayDoubleFloat] = trans_vector_double_float;
	transother[type_ComplexString] = trans_boxed;
	transother[type_ComplexBitVector] = trans_boxed;
	transother[type_ComplexVector] = trans_boxed;
	transother[type_ComplexArray] = trans_boxed;
	transother[type_CodeHeader] = trans_code_header;
	transother[type_FunctionHeader] = trans_function_header;
	transother[type_ClosureFunctionHeader] = trans_closure_function_header;
	transother[type_ReturnPcHeader] = trans_return_pc_header;
	transother[type_ClosureHeader] = trans_boxed;
	transother[type_ValueCellHeader] = trans_boxed;
	transother[type_SymbolHeader] = trans_boxed;
	transother[type_BaseCharacter] = trans_immediate;
	transother[type_Sap] = trans_unboxed;
	transother[type_UnboundMarker] = trans_immediate;

	/* Size table */

	for (i = 0; i < 256; i++)
		sizetab[i] = size_lose;

	for (i = 0; i < 32; i++) {
		sizetab[type_EvenFixnum|(i<<3)] = size_immediate;
		sizetab[type_FunctionPointer|(i<<3)] = size_pointer;
		/* OtherImmediate0 */
		sizetab[type_ListPointer|(i<<3)] = size_pointer;
		sizetab[type_OddFixnum|(i<<3)] = size_immediate;
		sizetab[type_StructurePointer|(i<<3)] = size_pointer;
		/* OtherImmediate1 */
		sizetab[type_OtherPointer|(i<<3)] = size_pointer;
	}

	sizetab[type_Bignum] = size_unboxed;
	sizetab[type_Ratio] = size_boxed;
	sizetab[type_SingleFloat] = size_unboxed;
	sizetab[type_DoubleFloat] = size_unboxed;
	sizetab[type_Complex] = size_boxed;
	sizetab[type_SimpleArray] = size_boxed;
	sizetab[type_SimpleString] = size_string;
	sizetab[type_SimpleBitVector] = size_vector_bit;
	sizetab[type_SimpleVector] = size_vector;
	sizetab[type_SimpleArrayUnsignedByte2] = size_vector_unsigned_byte_2;
	sizetab[type_SimpleArrayUnsignedByte4] = size_vector_unsigned_byte_4;
	sizetab[type_SimpleArrayUnsignedByte8] = size_vector_unsigned_byte_8;
	sizetab[type_SimpleArrayUnsignedByte16] = size_vector_unsigned_byte_16;
	sizetab[type_SimpleArrayUnsignedByte32] = size_vector_unsigned_byte_32;
	sizetab[type_SimpleArraySingleFloat] = size_vector_single_float;
	sizetab[type_SimpleArrayDoubleFloat] = size_vector_double_float;
	sizetab[type_ComplexString] = size_boxed;
	sizetab[type_ComplexBitVector] = size_boxed;
	sizetab[type_ComplexVector] = size_boxed;
	sizetab[type_ComplexArray] = size_boxed;
	sizetab[type_CodeHeader] = size_code_header;
#if 0
	/* Shouldn't see these so just lose if it happens */
	sizetab[type_FunctionHeader] = size_function_header;
	sizetab[type_ClosureFunctionHeader] = size_closure_function_header;
	sizetab[type_ReturnPcHeader] = size_return_pc_header;
#endif
	sizetab[type_ClosureHeader] = size_boxed;
	sizetab[type_ValueCellHeader] = size_boxed;
	sizetab[type_SymbolHeader] = size_boxed;
	sizetab[type_BaseCharacter] = size_immediate;
	sizetab[type_Sap] = size_unboxed;
	sizetab[type_UnboundMarker] = size_immediate;
}

