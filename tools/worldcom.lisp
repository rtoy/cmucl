;;; -*- Package: USER -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/tools/worldcom.lisp,v 1.52 1993/05/12 15:59:17 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; This file contains noise to compile the lisp world.
;;; 

(in-package "USER")

(with-compiler-log-file
    ("target:compile-lisp.log"
     :optimize '(optimize (speed 2) (space 2) (inhibit-warnings 2)
			  (debug-info #-small 2 #+small .5)
			  (safety #-small 1 #+small 0))
     :optimize-interface '(optimize-interface (safety #-small 2 #+small 1)
					      #+small (debug-info .5))
     :context-declarations
     '(((:or :external (:and (:match "%") (:match "SET"))
	     (:member lisp::%put lisp::%rplaca lisp::%rplacd lisp::%puthash))
	(declare (optimize-interface (safety 2) #+small (debug-info 1))
		 #+small (optimize (debug-info 1))))
       ((:or (:and :external :macro)
	     (:match "$PARSE-"))
	(declare (optimize (safety 2))))
       ((:and :external (:match "LIST"))
	(declare (optimize (safety 1))))))
(let ((*byte-compile-top-level* nil))

;;; these guys need to be first.
(comf "target:code/struct") ; For structures.
(comf "target:code/sysmacs")

;;; Assembly files.
(when (c:backend-featurep :pmax)
  (comf "target:assembly/mips/assem-rtns" :assem t)
  (comf "target:assembly/mips/array" :assem t)
  (comf "target:assembly/mips/arith" :assem t)
  (comf "target:assembly/mips/alloc" :assem t))

(when (c:backend-featurep :sparc)
  (comf "target:assembly/sparc/assem-rtns" :assem t)
  (comf "target:assembly/sparc/array" :assem t)
  (comf "target:assembly/sparc/bit-bash" :assem t)
  (comf "target:assembly/sparc/arith" :assem t)
  (comf "target:assembly/sparc/alloc" :assem t))

(when (c:backend-featurep :rt)
  (comf "target:assembly/rt/assem-rtns" :assem t)
  (comf "target:assembly/rt/array" :assem t)
  (comf "target:assembly/rt/arith" :assem t)
  (comf "target:assembly/rt/alloc" :assem t))

(when (c:backend-featurep :hppa)
  (comf "target:assembly/hppa/assem-rtns" :assem t)
  (comf "target:assembly/hppa/array" :assem t)
  (comf "target:assembly/hppa/arith" :assem t)
  (comf "target:assembly/hppa/alloc" :assem t))

(when (c:backend-featurep :x86)
  (comf "target:assembly/x86/assem-rtns" :assem t)
  (comf "target:assembly/x86/array" :assem t)
  (comf "target:assembly/x86/arith" :assem t)
  (comf "target:assembly/x86/alloc" :assem t))

;;; these guys can supposedly come in any order, but not really.
;;; some are put at the end so macros don't run interpreted and stuff.

(comf "target:code/kernel")
(comf "target:code/lispinit")
(comf "target:code/fdefinition")

(comf "target:code/error")

;;; prevent deftypes from taking effect at compile time so that we don't
;;; install interpreted type expanders causing the compiler to infinitely
;;; recurse.
(defvar *original-%deftype* #'lisp::%deftype)
(setf (fdefinition 'lisp::%deftype) #'list)
(comf "target:code/typedefs")

(with-compilation-unit
  (:optimize '(optimize (safety 2) (debug 2)))
  (comf "target:code/class"))

(comf "target:code/type")
(comf "target:compiler/generic/vm-type")
(comf "target:code/type-init")
(comf "target:code/pred")
(setf (fdefinition 'lisp::%deftype) *original-%deftype*)

(comf "target:code/alieneval")
(comf "target:code/c-call")
(comf "target:code/sap")

(comf "target:code/bit-bash")
(comf "target:code/byte-interp")
(comf "target:code/array")
(comf "target:code/hash")

(with-compilation-unit
  (:optimize '(optimize (safety 1)))
  (comf "target:code/list")
  (comf "target:code/seq")) ; seq must come after list

(comf "target:code/string")
(comf "target:code/mipsstrops")

(comf "target:code/unix")
(when (c:backend-featurep :mach)
  (comf "target:code/mach")
  (comf "target:code/mach-os"))
(when (c:backend-featurep :sunos)
  (comf "target:code/sunos-os"))

(when (c:backend-featurep :pmax)
  (comf "target:code/pmax-vm"))
(when (c:backend-featurep :sparc)
  (comf "target:code/sparc-vm"))
(when (c:backend-featurep :rt)
  (comf "target:code/rt-vm"))
(when (c:backend-featurep :hppa)
  (comf "target:code/hppa-vm"))
(when (c:backend-featurep :x86)
  (comf "target:code/x86-vm"))

(comf "target:code/symbol")
(comf "target:code/bignum")
(comf "target:code/numbers")
(comf "target:code/float-trap")
(comf "target:code/float")
(comf "target:code/irrat")

(comf "target:code/type-boot")

(comf "target:compiler/proclaim")

(comf "target:code/char")
(comf "target:code/misc")
(comf "target:code/extensions")
(comf "target:code/commandline")

(comf "target:code/room")
(comf "target:code/gc")
(comf "target:code/purify")
(comf "target:code/save")

(comf "target:code/stream")
(comf "target:code/print")
(comf "target:code/pprint")
(comf "target:code/format")

(comf "target:code/package")
(comf "target:code/reader")
(comf "target:code/sharpm")
(comf "target:code/backq")

(comf "target:code/serve-event")
(comf "target:code/fd-stream")
(comf "target:code/pathname")
(comf "target:code/filesys")
(comf "target:code/load")
(comf "target:code/module")

(comf "target:code/eval")

(comf "target:code/signal")
(comf "target:code/interr")
(comf "target:code/debug-info")
(comf "target:code/debug-int")
(comf "target:code/debug")

(comf "target:code/query")
(comf "target:code/rand")
(comf "target:code/ntrace")
(comf "target:code/profile")
(comf "target:code/sort")
(comf "target:code/time")
(comf "target:code/weak")
(comf "target:code/final")

;;; Later so that miscellaneous structures are defined (not crucial, but nice.)
(comf "target:code/describe")
(comf "target:code/tty-inspect")

(comf "target:code/format-time")
(comf "target:code/parse-time")
(comf "target:code/run-program")

(comf "target:code/loop")

(comf "target:code/foreign")
(comf "target:code/internet")
(comf "target:code/wire")
(comf "target:code/remote")

(comf "target:code/setf-funs")
(comf "target:code/exports" :proceed t)

;;; Compile basic macros that we assume are already in the compilation
;;; environment.  We inhibit compile-time definition to prevent these functions
;;; from becoming interpreted.  In some cases, this is necessary for
;;; compilation to work at all, since the expander functions are lazily
;;; converted: we could go into an infinite recursion trying to convert the
;;; definition of a macro which uses itself.
;;;
(let ((c:*compile-time-define-macros* nil))
  (comf "target:code/defstruct")
  (comf "target:code/defmacro")
  (comf "target:compiler/globaldb")
  ;; We can't compile anything after macros, 'cause it breaks the running lisp.
  (comf "target:code/macros"))

); let *byte-compile-top-level*

); with-compiler-log-file
