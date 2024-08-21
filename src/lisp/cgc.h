/* cgc.h -*- Mode: C; comment-column: 40; -*-
 *
 * Conservative GC for CMUCL X86
 *
 */

#ifndef CGC_H
#define CGC_H

void *cgc_alloc(int);
void cgc_free_heap(void);

#endif /* CGC_H */
