/*
 * Header file for GC
 *
 */

#ifndef GC_H
#define GC_H

extern void gc_init(void);
extern void collect_garbage(void);
extern lispobj *component_ptr_from_pc(lispobj * pc);

#ifndef ibmrt

#include "os.h"

extern void set_auto_gc_trigger(os_vm_size_t usage);
extern void clear_auto_gc_trigger(void);

#endif /* ibmrt */

#endif /* GC_H */
