#ifndef __ARCH_H__
#define __ARCH_H__

extern char *arch_init();
extern void arch_skip_instruction();
extern os_vm_address_t arch_get_bad_addr();

#endif /* __ARCH_H__ */
