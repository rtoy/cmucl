/*
 * Header file for GC
 *
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/gc.h,v 1.3 2003/07/28 17:43:11 gerd Exp $
 */

#ifndef _GC_H_
#define _GC_H_

extern void gc_init(void);
extern void collect_garbage(void);
extern lispobj *component_ptr_from_pc (lispobj *pc);

#ifndef ibmrt

#include "os.h"

extern void set_auto_gc_trigger(os_vm_size_t usage);
extern void clear_auto_gc_trigger(void);

#endif /* ibmrt */

#endif /* _GC_H_ */
