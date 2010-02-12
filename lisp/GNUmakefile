# $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/GNUmakefile,v 1.33.14.3 2010/02/12 22:21:23 rtoy Exp $

all: lisp.nm translations

-include internals.inc
include Config

SRCS = lisp.c coreparse.c alloc.c monitor.c print.c interr.c \
	vars.c parse.c interrupt.c search.c validate.c globals.c \
	dynbind.c breakpoint.c regnames.c backtrace.c save.c purify.c \
	runprog.c time.c exec-init.c \
	${ARCH_SRC} ${ASSEM_SRC} ${OS_SRC} ${GC_SRC}

OBJS = $(patsubst %.c,%.o,$(patsubst %.S,%.o,$(patsubst %.s,%.o,$(SRCS))))

### Don't look in RCS for the files, because we might not want the latest.
%: RCS/%,v

lisp.nm: lisp lisp.a
	echo 'Map file for lisp version ' `cat version` > ,lisp.nm
	$(NM) lisp | grep -v " [F] " >> ,lisp.nm
	mv ,lisp.nm lisp.nm

version.o : version.c version
	echo '1 + ' `cat version` | bc > ,version
	mv ,version version
	$(CC) ${CFLAGS} $(CPPFLAGS) -DVERSION=`cat version` -c $<

lisp: ${OBJS} version.o
	$(CC) -g ${OS_LINK_FLAGS} -o ,lisp \
		${OBJS} version.o \
		${OS_LIBS} -lm
	mv -f ,lisp lisp

# Create a library out of all the object files so we can build an
# executable.  However, for Solaris, we need to remove exec-init.o
# because the linker will add the write symbols in.
lisp.a:	version.o ${OBJS}
	ar crs lisp.a version.o ${OBJS}
ifdef FEATURE_SOLARIS
	ar d lisp.a exec-init.o
endif

version:
	echo 0 > version

internals.h internals.inc:
	@echo "You must run genesis to create internals.h!"
	@false

clean:
	rm -f Depends *.o lisp lisp.nm core
	echo 'Map file for lisp version 0' > lisp.nm

depend: Depends

Depends: ${SRCS}
	$(DEPEND) ${DEPEND_FLAGS} ${CFLAGS} ${CPPFLAGS} $^ > ,depends
	mv ,depends Depends

-include Depends

# Find all directories in ../i18n/locale.  These are the locales we
# currently support.

# This would be a nice way to do it so we don't have to keep track of
# the directories, but Solaris' find doesn't grok -depth 1
#LOCALES=$(patsubst ../i18n/locale/%, %, $(shell find ../i18n/locale -type d -depth 1))
LOCALES=en@piglatin ko

# Convert locale names to the appropriate path where we want the mo files to go.
LOCALE_MO=$(patsubst %, ../i18n/locale/%/LC_MESSAGES/cmucl.mo, $(LOCALES))

translations: $(LOCALE_MO)

.PHONY : translations

# Create the mo files 
../i18n/locale/%/LC_MESSAGES/cmucl.mo : ../../src/i18n/locale/%/LC_MESSAGES/cmucl.po
	-msgfmt -v $^ -o $@

# Update the translations with the template
../../src/i18n/locale/%/LC_MESSAGES/cmucl.po : ../../src/i18n/locale/cmucl.pot
	msgmerge -v $@ $^ -o $@

