/* $Id */

/* This code was written by Fred Gilham and has been placed in the public domain.  It is
   provided "AS-IS" and without warranty of any kind.
*/

#if !defined(_ELF_H_INCLUDED_)

#define _ELF_H_INCLUDED_

#define LINKER_SCRIPT "link.sh"

int write_elf_object(const char *, int, os_vm_address_t, os_vm_address_t);
void elf_cleanup(const char *);
int elf_run_linker(long, char *);

void map_core_sections(char *);

#endif
