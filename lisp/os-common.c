/*

 $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/os-common.c,v 1.6 2002/08/28 13:29:25 pmai Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>
#include <string.h>

#include "os.h"
#include "internals.h"
#include "validate.h"
#include "lisp.h"

/* Except for os_zero, these routines are only called by Lisp code.  These
   routines may also be replaced by os-dependent versions instead.  See
   hpux-os.c for some useful restrictions on actual usage. */

void os_zero(addr, length)
os_vm_address_t addr;
os_vm_size_t length;
{
    os_vm_address_t block_start;
    os_vm_size_t block_size;

#ifdef DEBUG
    fprintf(stderr,";;; os_zero: addr: 0x%08x, len: 0x%08x\n",addr,length);
#endif

    block_start=os_round_up_to_page(addr);

    length-=block_start-addr;
    block_size=os_trunc_size_to_page(length);

    if(block_start>addr)
	bzero((char *)addr,block_start-addr);
    if(block_size<length)
	bzero((char *)block_start+block_size,length-block_size);
    
    if (block_size != 0) {
	/* Now deallocate and allocate the block so that it */
	/* faults in  zero-filled. */

	os_invalidate(block_start,block_size);
	addr=os_validate(block_start,block_size);

	if(addr==NULL || addr!=block_start)
	    fprintf(stderr,"os_zero: block moved, 0x%08x ==> 0x%08x!\n",block_start,addr);
    }
}

os_vm_address_t os_allocate(len)
os_vm_size_t len;
{
    return os_validate((os_vm_address_t)NULL,len);
}

os_vm_address_t os_allocate_at(os_vm_address_t addr, os_vm_size_t len)
{
    return os_validate(addr, len);
}

void os_deallocate(addr,len)
os_vm_address_t addr;
os_vm_size_t len;
{
    os_invalidate(addr,len);
}

/* This function once tried to grow the chunk by asking os_validate if the
   space was available, but this really only works under Mach. */

os_vm_address_t os_reallocate(os_vm_address_t addr, os_vm_size_t old_len,
			      os_vm_size_t len)
{
    addr=os_trunc_to_page(addr);
    len=os_round_up_size_to_page(len);
    old_len=os_round_up_size_to_page(old_len);

    if(addr==NULL)
	return os_allocate(len);
    else{
	long len_diff=len-old_len;

	if(len_diff<0)
	    os_invalidate(addr+len,-len_diff);
	else{
	    if(len_diff!=0){
	      os_vm_address_t new=os_allocate(len);

	      if(new!=NULL){
		bcopy(addr,new,old_len);
		os_invalidate(addr,old_len);
		}
		
	      addr=new;
	    }
	}
	return addr;
    }
}

#ifdef LINKAGE_TABLE
extern void resolve_linkage_tramp(void);

/* In words */
#define LINKAGE_DATA_ENTRY_SIZE 3
#endif

void os_foreign_linkage_init (void)
{
#ifdef LINKAGE_TABLE
    lispobj linkage_data_obj =  SymbolValue(LINKAGE_TABLE_DATA);
    struct array *linkage_data = 0;
    long table_size = 0;
    struct vector *data_vector = 0;
    long i;
    
    linkage_data = (struct array *)PTR(linkage_data_obj);
    table_size = fixnum_value(linkage_data->fill_pointer);
    data_vector = (struct vector *)PTR(linkage_data->data);
    for (i = 0; i < table_size; i += LINKAGE_DATA_ENTRY_SIZE) {
	struct vector *symbol_name
	  = (struct vector *)PTR(data_vector->data[i]);
	long type = fixnum_value(data_vector->data[i + 1]);
	lispobj lib_list = data_vector->data[i + 2];
	
	if (i == 0) {
	    if (type != 1 || strcmp((char *)symbol_name->data,
				    "resolve_linkage_tramp")) {
		lose("First element of linkage_data is bogus.\n");
	    }
	    arch_make_linkage_entry(0, &resolve_linkage_tramp, 1);
	    continue;
	}
	if (type == 2 && lib_list == NIL) {
	    void *target_addr = os_dlsym((char *)symbol_name->data, NIL);

	    if (!target_addr) {
		lose("%s is not defined.\n", (char *)symbol_name->data);
	    }
	    arch_make_linkage_entry(i / LINKAGE_DATA_ENTRY_SIZE, target_addr,
				    type);
	} else {
	    arch_make_lazy_linkage(i / LINKAGE_DATA_ENTRY_SIZE);
	}
    }
#endif /* LINKAGE_TABLE */
}

/* At the second stage of initialization, after Lisp has dlopened all
   needed shared libraries, go back through the table and initialize
   data symbols. */

void
os_resolve_data_linkage(void)
{
#ifdef LINKAGE_TABLE
    lispobj linkage_data_obj =  SymbolValue(LINKAGE_TABLE_DATA);
    struct array *linkage_data = 0;
    long table_size = 0;
    struct vector *data_vector = 0;
    long i;
    
    linkage_data = (struct array *)PTR(linkage_data_obj);
    table_size = fixnum_value(linkage_data->fill_pointer);
    data_vector = (struct vector *)PTR(linkage_data->data);
    for (i = 0; i < table_size; i += LINKAGE_DATA_ENTRY_SIZE) {
	struct vector *symbol_name
	  = (struct vector *)PTR(data_vector->data[i]);
	long type = fixnum_value(data_vector->data[i + 1]);
	lispobj lib_list = data_vector->data[i + 2];

    	if (type == 2 && lib_list != NIL) {
	    void *target_addr = os_dlsym((char *)symbol_name->data, lib_list);

	    if (!target_addr) {
		lose("%s is not defined.\n", (char *)symbol_name->data);
	    }
	    arch_make_linkage_entry(i / LINKAGE_DATA_ENTRY_SIZE, target_addr,
				    type);
	}
    }
#endif /* LINKAGE_TABLE */
}

/* Make entry for the symbol at entry in LINKAGE_TABLE_DATA.  Called
   from register-foreign-linkage. */
#ifdef LINKAGE_TABLE
extern void undefined_foreign_symbol_trap(lispobj arg);
#endif

unsigned long os_link_one_symbol(long entry)
{
#ifdef LINKAGE_TABLE
    lispobj linkage_data_obj =  SymbolValue(LINKAGE_TABLE_DATA);
    struct array *linkage_data = 0;
    long table_size = 0;
    struct vector *data_vector = 0;
    struct vector *symbol_name;
    long type;
    void *target_addr;
    long table_index = entry * LINKAGE_DATA_ENTRY_SIZE;
        
    linkage_data = (struct array *)PTR(linkage_data_obj);
    table_size = fixnum_value(linkage_data->fill_pointer);
    if (table_index >= table_size - 1) {
	return 0;
    }
    data_vector = (struct vector *)PTR(linkage_data->data);
    symbol_name = (struct vector *)PTR(data_vector->data[table_index]);
    type = fixnum_value(data_vector->data[table_index + 1]);
    target_addr = os_dlsym((char *)symbol_name->data,
			   data_vector->data[table_index + 2]);
    if (!target_addr) {
	undefined_foreign_symbol_trap((lispobj)data_vector->data[table_index]);
    }
    arch_make_linkage_entry(entry, target_addr, type);
    return target_addr;
#else
    return 0;
#endif /* LINKAGE_TABLE */
}

unsigned long lazy_resolve_linkage(unsigned long retaddr)
{
#ifdef LINKAGE_TABLE
    unsigned long target_addr
	= os_link_one_symbol(arch_linkage_entry(retaddr));

    return target_addr;
#else
    return 0;
#endif /* LINKAGE_TABLE */
}
