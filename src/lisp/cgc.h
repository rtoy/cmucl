/* cgc.h -*- Mode: C; comment-column: 40; -*-
 *
 * Conservative GC for CMUCL X86
 *
 */

#ifndef _CGC_H_
#define _CGC_H_

void *cgc_alloc(int);
void cgc_free_heap(void);

lispobj *component_ptr_from_pc(lispobj * pc);
#endif /* _CGC_H_ */
