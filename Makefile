# Makefile to build cmucl

TOPDIR	  := $(PWD)
TOOLSDIR  := $(TOPDIR)/src/tools
BINDIR    := $(TOPDIR)/bin
BUILDDIR  := $(TOPDIR)/build
BOOTCMUCL := cmucl
XHOST	  := x86
XTARGET	  := x86
BOOTFILE  :=

help:
	@echo -e "\
all        -- world (= xcompiler+genesis+runtime+compiler+pcl)\n\
help       -- Print out help information\n\
help-vars  -- Information about make variables\n\
help-other -- Information about rarely needed targets\n\
world      -- Create core file and C runtime\n\
xcompiler  -- Build a core file with cross-compiler loaded\n\
genesis    -- Cross-dump initial image (no compiler, no pcl) \n\
runtime    -- Build C runtime\n\
compiler   -- Build core file with compiler loaded\n\
pcl        -- Build core file with compiler+pcl loaded\n\
stage2     -- Compile world again using compiler (no cross-compiler)\n\
clean      -- Remove build directory\
"

help-vars:
	@echo -e "\
TOPDIR	   directory containing src directory ($(TOPDIR))\n\
TOOLSDIR   directory with build scripts ($(TOOLSDIR))\n\
BUILDDIR   build directory ($(BUILDDIR))\n\
BOOTCMUCL  compiler used for bootstrap ($(BOOTCMUCL))\n\
XHOST	   host system ($(XHOST))\n\
XTARGET	   target system ($(XTARGET))\n\
BOOTFILE   file for bootstrap hacks (default: none)\
"

help-other:
	@echo -e "\
xcompile-world     -- cross-compile library \n\
xcompile-compiler  -- cross-compile compiler \n\
xdump-world        -- cold-load library and cross-dump (genesis)\n\
clean-world        -- remove the build/world directory\n\
sanity-clean       -- remove fasl files in source directory\n\
run-xcompiler      -- open a REPL with the cross-compiler\
"

all: world

XCOMPILERDIR    := $(BUILDDIR)/xcompiler
KERNELDIR	:= $(BUILDDIR)/world
COMPILERDIR	:= $(BUILDDIR)/compiler
PCLDIR		:= $(BUILDDIR)/pcl
STAGE2DIR	:= $(BUILDDIR)/stage2

CROSSCORE	:= $(XCOMPILERDIR)/cross-$(XHOST)-$(XTARGET).core
KERNELCORE	:= $(KERNELDIR)/lisp/kernel.core
RUNTIME		:= $(KERNELDIR)/lisp/lisp
COMPILERCORE	:= $(COMPILERDIR)/lisp/compiler.core
PCLCORE		:= $(PCLDIR)/lisp/pcl.core
LISPCORE	:= $(KERNELDIR)/lisp/lisp.core
KERNELCORE2	:= $(STAGE2DIR)/lisp/kernel.core
RUNTIME2	:= $(STAGE2DIR)/lisp/lisp
LISPCORE2	:= $(STAGE2DIR)/lisp/lisp.core

XSETUP='							\
(intl::install)							\
(setf (ext:search-list "target:")				\
      (quote ("$(1)/" "src/")))					\
(load "target:tools/setup" :if-source-newer :load-source)	\
(comf "target:tools/setup" :load t)				\
(setq *gc-verbose* nil *interactive* nil)			\
'

SETUP2='							\
(intl::install)							\
(setq *compile-print* t)					\
(setq *load-verbose* t)						\
(load "target:setenv")						\
(pushnew :no-clx *features*)					\
(pushnew :no-clm *features*)					\
(pushnew :no-hemlock *features*)				\
(load "target:code/exports")					\
(load "target:tools/setup" :if-source-newer :load-source)	\
(comf "target:tools/setup" :load t)				\
(setq *gc-verbose* nil *interactive* nil)			\
'

LOAD_BOOTFILE='					\
(let ((bootfile "$(BOOTFILE)"))			\
  (unless (equal bootfile "")			\
    (load bootfile)))				\
'

SET_TARGET_SEARCH_LIST=(setf (ext:search-list "target:") (list $(1) "src/"))

XSETENV='					\
$(call SET_TARGET_SEARCH_LIST,$(1))		\
(pushnew :bootstrap *features*)			\
(load "target:setenv")				\
(pushnew :no-pcl *features*)			\
(pushnew :no-clx *features*)			\
(pushnew :no-clm *features*)			\
(pushnew :no-hemlock *features*)		\
'

#(load "target:tools/comcom")					\
#(comf "target:compiler/generic/new-genesis")			\

LOAD_WORLD='					\
(in-package :cl-user)				\
$(call SET_TARGET_SEARCH_LIST, "$(KERNELDIR)/")	\
(load "target:setenv")				\
(pushnew :no-compiler *features*)		\
(pushnew :no-clx *features*)			\
(pushnew :no-clm *features*)			\
(pushnew :no-hemlock *features*)		\
(pushnew :no-pcl *features*)			\
(load "target:tools/worldload")			\
'

#(setf (ext:search-list "target:")			\
#      (list "$(COMPILERDIR)/" "$(KERNELDIR)/" "src/"))	\

LOAD_COMPILER='								\
(in-package :cl-user)							\
$(call SET_TARGET_SEARCH_LIST,"$(COMPILERDIR)/" "$(KERNELDIR)/")	\
(load "target:setenv")							\
(pushnew :no-clx *features*)						\
(pushnew :no-clm *features*)						\
(pushnew :no-hemlock *features*)					\
(pushnew :no-pcl *features*)						\
(load "target:tools/worldload")						\
'

COMPILE_PCL='					\
(load "target:code/exports")			\
(pushnew :bootstrap *features*)			\
(load "target:setenv")				\
(pushnew :no-pcl *features*)			\
(pushnew :no-clx *features*)			\
(pushnew :no-clm *features*)			\
(load "target:tools/pclcom")			\
'

LOAD_PCL='								      \
(in-package :cl-user)							      \
$(call SET_TARGET_SEARCH_LIST,"$(PCLDIR)/" "$(COMPILERDIR)/" "$(KERNELDIR)/") \
(load "target:setenv")							      \
(pushnew :no-clx *features*)						      \
(pushnew :no-clm *features*)						      \
(pushnew :no-hemlock *features*)					      \
(load "target:tools/worldload")						      \
'

LOAD_PCL2='					\
(in-package :cl-user)				\
$(call SET_TARGET_SEARCH_LIST,"$(STAGE2DIR)/")	\
(load "target:setenv")				\
(pushnew :no-clx *features*)			\
(pushnew :no-clm *features*)			\
(pushnew :no-hemlock *features*)		\
(load "target:tools/worldload")			\
'

xcompiler: $(CROSSCORE)

$(BUILDDIR)/xcompiler/cross-%.core:
	$(MAKE) sanity
	rm -rf $(XCOMPILERDIR)
	mkdir -vp $(BUILDDIR)
	if [ ! -e $(BUILDDIR)/src ] ; then		\
		ln -s $(TOPDIR)/src $(BUILDDIR)/src ;	\
	fi
	$(BINDIR)/create-target.sh $(XCOMPILERDIR)
	cp -v $(TOOLSDIR)/cross-scripts/$(subst .core,.lisp,$(notdir $@)) \
	   $(XCOMPILERDIR)/cross.lisp
	$(BOOTCMUCL) -noinit -nositeinit  				\
-eval '(in-package :cl-user)'						\
-eval '(setf lisp::*enable-package-locked-errors* nil)'			\
-eval '(intl::install)'							\
-eval '$(call SET_TARGET_SEARCH_LIST, "$(XCOMPILERDIR)/")'		\
-eval '(load "target:tools/setup" :if-source-newer :load-source)'	\
-eval '(comf "target:tools/setup" :load t)'				\
-eval '(setq *gc-verbose* nil *interactive* nil)'			\
-eval '(load "$(XCOMPILERDIR)/cross.lisp")'				\
-eval '(remf ext::*herald-items* :python)'				\
-eval '(ext:save-lisp "$@" :purify nil)'				\
-eval '(ext:quit)'
# Strangeness 1: the -batch command line option breaks the build!
# Strangeness 2: if :purify is t, the compiler in the core file doesn't work

xlisp: xcompiler
	$(BOOTCMUCL) -core $(CROSSCORE)

xcompile-world: $(KERNELDIR)/world.snapshot

$(KERNELDIR)/world.snapshot: $(CROSSCORE)
	$(MAKE) sanity
	$(MAKE) clean-world
	$(BINDIR)/create-target.sh $(KERNELDIR)
	$(BOOTCMUCL)							\
		-core $(CROSSCORE)					\
		-noinit	-nositeinit					\
		-eval $(call XSETENV, "$(KERNELDIR)/")			\
		-eval $(LOAD_BOOTFILE)					\
		-eval '(load "target:tools/worldcom")'			\
		-eval '(ext:save-lisp "$@" :purify nil)'		\
		-eval '(ext:quit)'

xcompile-compiler: $(COMPILERDIR)/compiler.snapshot

$(COMPILERDIR)/compiler.snapshot: $(KERNELDIR)/world.snapshot
	$(MAKE) sanity
	$(MAKE) clean-compiler
	$(BINDIR)/create-target.sh $(COMPILERDIR)
	$(BOOTCMUCL)							\
		-core $<						\
		-noinit	-nositeinit					\
		-eval $(call XSETENV, "$(COMPILERDIR)/")		\
		-eval $(LOAD_BOOTFILE)					\
		-eval '(load "target:tools/comcom")'			\
		-eval '(ext:save-lisp "$@" :purify nil)'		\
		-eval '(ext:quit)'

run-xcompiler: xcompiler
	$(MAKE) sanity
	$(BOOTCMUCL)					\
		-core $(CROSSCORE)			\
		-noinit					\
		-eval $(call XSETENV, "$(KERNELDIR)/")	\
		-eval $(SETUP_CROSS_COMPILER)		\
		-eval $(LOAD_BOOTFILE)

MOVECORE=cd $(1) &&\
	mv lisp.core $(2)			\
	|| mv lisp-sse2.core $(2)		\
	|| mv lisp-x87.core $(2)

genesis: $(KERNELCORE)

#$(CROSSCORE)
#	$(MAKE) xcompile-world
#	$(MAKE) xdump-world
#		-eval '(load "target:tools/comcom")'			\
#	        -eval '(comf "target:compiler/generic/new-genesis")'	\

$(KERNELCORE): $(KERNELDIR)/world.snapshot
	$(BOOTCMUCL)							\
		-core $(CROSSCORE)					\
		-noinit							\
		-eval $(call XSETENV, "$(KERNELDIR)/")			\
		-eval '(load "target:tools/worldbuild")'		\
		-eval '(quit)'

compiler: $(COMPILERCORE)

$(COMPILERCORE): $(KERNELCORE) $(RUNTIME) $(COMPILERDIR)/compiler.snapshot
	echo $(LOAD_COMPILER) | $(RUNTIME) -core $(KERNELCORE)
	$(call MOVECORE,$(COMPILERDIR)/lisp,$@)

compile-pcl: $(PCLDIR)/pcl.stamp

$(PCLDIR)/pcl.stamp: $(COMPILERCORE)
	$(MAKE) sanity
	$(MAKE) clean-pcl
	$(BINDIR)/create-target.sh $(PCLDIR)
	$(RUNTIME)							\
		-core $(COMPILERCORE)					\
		-noinit	-nositeinit					\
		-eval '$(call SET_TARGET_SEARCH_LIST,"$(PCLDIR)/")'	\
		-eval $(SETUP2)						\
		-eval $(COMPILE_PCL)					\
		-eval '(ext:quit)'
	touch $@

pcl: $(PCLCORE)

$(PCLCORE): $(PCLDIR)/pcl.stamp
	echo $(LOAD_PCL) | $(RUNTIME) -core $(KERNELCORE)
	$(call MOVECORE,$(PCLDIR)/lisp,$@)

runtime: $(RUNTIME)

$(RUNTIME): $(KERNELDIR)/lisp ;

$(KERNELDIR)/lisp: $(KERNELCORE)
	$(MAKE) -C $(KERNELDIR)/lisp

.PHONY: $(KERNELDIR)/lisp

world: $(LISPCORE)

$(LISPCORE): $(PCLCORE)
	cp $< $@

compile-world2: $(KERNELCORE2)

$(KERNELCORE2): $(COMPILERCORE) 
	$(MAKE) sanity
	$(MAKE) clean-stage2
	$(BINDIR)/create-target.sh $(STAGE2DIR)
	$(RUNTIME)							 \
	 	-core $(COMPILERCORE)					 \
		-noinit							 \
-eval '(in-package :cl-user)'						 \
-eval '(intl::install)'							 \
-eval '$(call SET_TARGET_SEARCH_LIST, "$(STAGE2DIR)/")'			 \
-eval '(load "target:setenv")'						 \
-eval '(pushnew :no-clx *features*)'					 \
-eval '(pushnew :no-clm *features*)'					 \
-eval '(pushnew :no-hemlock *features*)'				 \
-eval '(load "target:code/exports")'					 \
-eval '(load "target:tools/setup" :if-source-newer :load-source)'	 \
-eval '(comf "target:tools/setup" :load t)'				 \
-eval '(setq *gc-verbose* nil *interactive* nil)'			 \
-eval '(load "target:tools/worldcom")'					 \
-eval '(load "target:tools/comcom")'					 \
-eval '(load "target:tools/pclcom")'					 \
-eval '(load "target:tools/worldbuild")'				 \
-eval '(ext:quit)'							

runtime2: $(RUNTIME2)

$(RUNTIME2): $(STAGE2DIR)/lisp ;

$(STAGE2DIR)/lisp: $(KERNELCORE2)
	$(MAKE) -C $(STAGE2DIR)/lisp

.PHONY: $(STAGE2DIR)/lisp

stage2: $(LISPCORE2)

$(LISPCORE2): $(KERNELCORE2) $(RUNTIME2)
	echo $(LOAD_PCL2) | $(RUNTIME2) -core $(KERNELCORE2)
	$(call MOVECORE,$(STAGE2DIR)/lisp,$@)

cross-build:
	bin/create-target.sh xcross
	bin/create-target.sh xtarget
	cp src/tools/cross-scripts/cross-x86-x86.lisp xtarget/cross.lisp
	bin/cross-build-world.sh xtarget xcross xtarget/cross.lisp $(BOOTCMUCL)
	bin/rebuild-lisp.sh xtarget
	bin/load-world.sh -p xtarget "newlisp"

sanity:
	@if [ `echo $(TOPDIR) | egrep -c '^/'` -ne 1 ]; then		\
	    echo "ERROR: TOPDIR must be an absolute path: $(TOPDIR)";	\
	    exit 1;							\
	fi
	@if [ ! -r $(TOPDIR)/src/hemlock/abbrev.lisp ] ; then		\
	    echo "ERROR: No cmucl source tree available at: $(TOPDIR)";	\
	    exit 1;							\
	fi
	@faslfiles=`find -L $(TOPDIR)/src/ -name  "*.sse2f"` ;		\
	if [ -n "$$faslfiles" ] ; then					\
	    echo ERROR: Source tree contains fasl files: "$$faslfiles";	\
	    exit 1;							\
	fi

sanity-clean:
	find -L $(TOPDIR)/src/ \( -name "*.sse2f" -o -name "*.bytef" \) \
	-exec rm -iv {} \;

clean: sanity-clean
	rm -rf $(BUILDDIR)

clean-xcompiler: sanity-clean
	rm -rf $(XCOMPILERDIR)

clean-world: sanity-clean
	rm -rf $(KERNELDIR)

clean-compiler: sanity-clean
	rm -rf $(COMPILERDIR)

clean-pcl: sanity-clean
	rm -rf $(PCLDIR)

clean-stage2: sanity-clean
	rm -rf $(STAGE2DIR)

rebuild-xcompiler: sanity-clean clean-xcompiler xcompiler

