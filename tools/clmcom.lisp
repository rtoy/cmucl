;;; File for compiling the Motif toolkit and related interface
;;; stuff.
;;;

(in-package "USER")

(pushnew :motif-toolkit *features*)

(with-compilation-unit
    (:optimize '(optimize (speed 3) (safety 1) (ext:inhibit-warnings 3)))

 (comf "target:motif/lisp/initial" :load t)
 (comf "target:motif/lisp/internals" :load t)
 (comf "target:motif/lisp/transport" :load t)
 (comf "target:motif/lisp/events" :load t)
 (comf "target:motif/lisp/conversion" :load t))

(with-compilation-unit
    (:optimize '(optimize (speed 2) (ext:inhibit-warnings 2)))

  (comf "target:motif/lisp/interface-glue" :load t)
  (comf "target:motif/lisp/xt-types" :load t)
  (comf "target:motif/lisp/string-base" :load t)
  (comf "target:motif/lisp/prototypes" :load t)
  (comf "target:motif/lisp/interface-build" :load t)
  (comf "target:motif/lisp/callbacks" :load t)
  (comf "target:motif/lisp/widgets" :load t)
  (comf "target:motif/lisp/main" :load t))

(xt::build-toolkit-interface)

(with-compilation-unit
    ()
  (comf "target:interface/initial" :load t)
  (comf "target:interface/interface" :load t)
  (comf "target:interface/inspect" :load t)
  ;; We don't want to fall into the Motif debugger while compiling.
  ;; It may be that the motifd server hasn't been (re)compiled yet.
  (let ((interface:*interface-style* :tty))
    (comf "target:interface/debug" :load t)))
