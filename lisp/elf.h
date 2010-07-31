/* $Id: elf.h,v 1.13 2010/07/31 01:07:15 rtoy Exp $ */

/* This code was written by Fred Gilham and has been placed in the public domain.  It is
   provided "AS-IS" and without warranty of any kind.
*/

/*
 * Despite the fact that this file is named elf.h, it's really the
 * interface to both elf and mach-o support.  I (rtoy) was too lazy to
 * change the name to something more descriptive.
 */
#if !defined(_ELF_H_INCLUDED_)

#define _ELF_H_INCLUDED_

#if defined(__linux__) || defined(DARWIN) || defined(sparc)
/*
 * Yes, it's named badly.  But it works for sparc too, not just x86.
 * If we ever get this fixed on FreeBSD, then we can move
 * linker-x86.sh to linker.sh and be done with it.
 */
#define LINKER_SCRIPT "linker-x86.sh"
#else
#define LINKER_SCRIPT "linker.sh"
#endif

#if defined(SOLARIS)
#include <sys/elf.h>
#elif defined(DARWIN)
#include <mach-o/loader.h>
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

int write_space_object(const char *, int, os_vm_address_t, os_vm_address_t);
void obj_cleanup(const char *);
int obj_run_linker(long, char *);

void map_core_sections(const char *);

#if defined(DARWIN)
#elif defined(SOLARIS) || defined(linux) || defined(__NetBSD__)
typedef Elf32_Ehdr Elf_Ehdr;
typedef Elf32_Shdr Elf_Shdr;
typedef Elf32_Word Elf_Word;
typedef Elf32_Addr Elf_Addr;
#endif

#endif
