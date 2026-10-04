# $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/GNUmakefile,v 1.39 2010/10/14 17:47:12 rtoy Exp $

# These tell gmake where to look for .h, .c and .S files.  Mostly for
# building the binary outside of the src tree.

PATH1 = ../../src/lisp
vpath %.h $(PATH1)
vpath %.c $(PATH1)
vpath %.S $(PATH1)

CMULOCALE = ../../src/i18n/locale
vpath %.pot $(CMULOCALE)
vpath %.po  $(CMULOCALE)
vpath %.mo  $(CMULOCALE)

all: lisp.nm


-include internals.inc
include Config

# Need rem_pio2 routines because double-double routines need these for
# pi reduction.  This is independent of whether we are using core-math
# or not.
OPENLIBM_DIR = openlibm
CPPFLAGS += -I $(PATH1)/$(OPENLIBM_DIR) -DOPENLIBM_USE_HOST_MATH_H

REM_PIO2 = \
	$(OPENLIBM_DIR)/e_rem_pio2.c \
	$(OPENLIBM_DIR)/k_rem_pio2.c

ifndef FEATURE_CORE_MATH
OPENLIBM_DOUBLE = \
	$(OPENLIBM_DIR)/e_acos.c \
	$(OPENLIBM_DIR)/e_acosh.c \
	$(OPENLIBM_DIR)/e_asin.c \
	$(OPENLIBM_DIR)/e_atan2.c \
	$(OPENLIBM_DIR)/e_atanh.c \
	$(OPENLIBM_DIR)/e_cosh.c \
	$(OPENLIBM_DIR)/e_exp.c \
	$(OPENLIBM_DIR)/e_hypot.c \
	$(OPENLIBM_DIR)/e_log10.c \
	$(OPENLIBM_DIR)/e_log.c \
	$(OPENLIBM_DIR)/e_log2.c \
	$(OPENLIBM_DIR)/e_pow.c \
	$(OPENLIBM_DIR)/e_sinh.c \
	$(OPENLIBM_DIR)/k_cos.c \
	$(OPENLIBM_DIR)/k_sin.c \
	$(OPENLIBM_DIR)/k_tan.c \
	$(OPENLIBM_DIR)/k_exp.c \
	$(OPENLIBM_DIR)/s_asinh.c \
	$(OPENLIBM_DIR)/s_atan.c \
	$(OPENLIBM_DIR)/s_cos.c \
	$(OPENLIBM_DIR)/s_expm1.c \
	$(OPENLIBM_DIR)/s_log1p.c \
	$(OPENLIBM_DIR)/s_scalbn.c \
	$(OPENLIBM_DIR)/s_sin.c \
	$(OPENLIBM_DIR)/s_sincos.c \
	$(OPENLIBM_DIR)/s_tan.c \
	$(OPENLIBM_DIR)/s_tanh.c

OPENLIBM_FLOAT = \
	$(OPENLIBM_DIR)/e_rem_pio2f.c \
	$(patsubst %.c, %f.c, $(OPENLIBM_DOUBLE))


OPENLIBM_SRCS = $(OPENLIBM_DOUBLE) $(OPENLIBM_FLOAT)
endif

FDLIBM = setexception.c


SRCS = lisp.c coreparse.c alloc.c monitor.c print.c interr.c \
	vars.c parse.c interrupt.c search.c validate.c globals.c \
	dynbind.c breakpoint.c regnames.c backtrace.c save.c purify.c \
	runprog.c time.c case-mapping.c exec-init.c \
	irrat.c \
	${CORE_MATH_SRCS} \
	$(REM_PIO2) \
	${OPENLIBM_SRCS} \
	${FDLIBM} ${ARCH_SRC} ${ASSEM_SRC} ${OS_SRC} ${GC_SRC}


OBJS = $(patsubst %.c,%.o,$(patsubst %.S,%.o,$(patsubst %.s,%.o,$(SRCS))))

### Don't look in RCS for the files, because we might not want the latest.
%: RCS/%,v

lisp.nm: lisp lisp.a
	echo 'Map file for lisp version ' `cat version` > ,lisp.nm
	$(NM) lisp | grep -v " [F] " >> ,lisp.nm
	mv ,lisp.nm lisp.nm

version.o : version.c version
	expr 1 + `cat version` > ,version
	mv ,version version
	$(CC) ${CFLAGS} $(CPPFLAGS) -DVERSION=`cat version` -c $<

lisp: ${OBJS} version.o
	$(CC) -g ${OS_LINK_FLAGS} -o ,lisp $^ ${OS_LIBS} -lm
	mv -f ,lisp lisp

# Create a library out of all the object files so we can build an
# executable.  However, we need to remove exec-init.o from the library
lisp.a:	version.o ${OBJS} ${EXEC_FINAL_OBJ}
	$(AR) crs lisp.a ${OBJS} version.o
ifneq (${EXEC_FINAL_OBJ},)
	$(AR) d lisp.a exec-init.o
endif

version:
	echo 0 > version

internals.h internals.inc:
	@echo "You must run genesis to create internals.h!"
	@false

clean:
	$(RM) Depends $(OBJS) lisp lisp.nm core lisp.a
	echo 'Map file for lisp version 0' > lisp.nm

depend: Depends

Depends: ${SRCS}
	$(CC) ${DEPEND_FLAGS} ${CFLAGS} ${CPPFLAGS} $^ > ,depends
	mv ,depends Depends

-include Depends

# Find all directories in ../i18n/locale.  These are the locales we
# currently support.

# This would be a nice way to do it so we don't have to keep track of
# the directories, but Solaris' find doesn't grok -depth 1
#LOCALES=$(patsubst ../i18n/locale/%, %, $(shell find ../i18n/locale -type d -depth 1))
LOCALES=en@piglatin ko

# Convert locale names to the appropriate path where we want the mo files to go.
LOCALE_DIRS = $(patsubst %, i18n/locale/%/LC_MESSAGES, $(LOCALES))

# When processing the translations, we merge the po file from the src
# directory with the appropriate pot file.  The result is placed in
# the build directory.  Then we compare the original po file with the
# newly generated one (but sorting both before comparing).  If they
# are different we print out a note so that we know to look into it
# and decide if the po file in the src directory needs to be updated.
#
# The sorting is done so that differences in order of the lines is not
# important.  In particular, msgmerge on OSX wants to put the
# Language: line after the Content-Transfer-Encoding line.  But on
# FreeBSD, it moves the line to after the Language-Team line.  This
# makes things messy.  Hence, sort and diff.
translations: 
	for pot in ../../src/i18n/locale/*.pot; do \
	  for po in $(LOCALE_DIRS); do \
            d=`dirname $$pot`; \
	    f=`basename $$pot .pot`; \
	    echo ; \
	    echo '***' Processing $$f.pot:  $$po; \
	    msgmerge -v ../../src/$$po/$$f.po $$pot -o ../$$po/$$f.po; \
	    msgfmt -v  ../$$po/$$f.po -o ../$$po/$$f.mo; \
	    sort < ../$$po/$$f.po > /tmp/$$$$-updated-$$f.po; \
	    sort < ../../src/$$po/$$f.po > /tmp/$$$$-orig-$$f.po; \
	    diff -uwB /tmp/$$$$-orig-$$f.po /tmp/$$$$-updated-$$f.po || echo "Warning:  $$po/$$f.po changed"; \
	    rm -f /tmp/$$$$-updated-$$f.po /tmp/$$$$-orig-$$f.po; \
	  done; done

.PHONY : translations

# Like translations, but we don't compute the diff.  We just overwrite
# the po files in the repository so that we can tell if the
# translations have changed and need to be updated.  (This is what the
# old translations used to do.)
translations-update: 
	for pot in ../../src/i18n/locale/*.pot; do \
	  for po in $(LOCALE_DIRS); do \
            d=`dirname $$pot`; \
	    f=`basename $$pot .pot`; \
	    touch ../../src/$$po/$$f.po; \
	    echo ; \
	    echo '***' Processing $$f.pot:  $$po; \
	    msgmerge -v ../../src/$$po/$$f.po $$pot -o ../../src/$$po/$$f.po; \
	    msgfmt -v  ../../src/$$po/$$f.po -o ../$$po/$$f.mo; \
	  done; done


