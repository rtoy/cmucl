/* $Id: elf.h,v 1.8 2007/08/14 15:57:47 rtoy Exp $ */

/* This code was written by Fred Gilham and has been placed in the public domain.  It is
   provided "AS-IS" and without warranty of any kind.
*/

#if !defined(_ELF_H_INCLUDED_)

#define _ELF_H_INCLUDED_

#define LINKER_SCRIPT "linker.sh"

#if defined(SOLARIS)
#include <sys/elf.h>
#else
#include <elf.h>
#endif

/*
 * We need to know which compiler was used to build lisp.  I think gcc
 * is used everywhere, except on Solaris/sparc, where we can use
 * either gcc or Sun C.
 */
#if defined(__SUNPRO_C) && defined(__sparc)
#define C_COMPILER "cc"
#else
#define C_COMPILER "gcc"
#endif

int write_elf_object(const char *, int, os_vm_address_t, os_vm_address_t);
void elf_cleanup(const char *);
int elf_run_linker(long, char *);

void map_core_sections(char *);

#if defined(SOLARIS) || defined(linux)
typedef Elf32_Ehdr Elf_Ehdr;
typedef Elf32_Shdr Elf_Shdr;
typedef Elf32_Word Elf_Word;
typedef Elf32_Addr Elf_Addr;
#endif

#endif
