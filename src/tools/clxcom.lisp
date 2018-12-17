;;; -*- Package: USER -*-
;;;
;;; **********************************************************************
;;;
(ext:file-comment
  "$Header: src/tools/clxcom.lisp $")
;;;
;;; **********************************************************************
;;;
(in-package "CL-USER")

#+bootstrap
(unless (find-package "OLD-XLIB")
  (when (find-package "XLIB")
    (rename-package (find-package "XLIB") "OLD-XLIB"))
  
  (make-package "XLIB" :use '("COMMON-LISP")))

#+(and (not pcl) (not no-pcl-clx))
(progn
  (load "target:pcl/pclload")
  #+gencgc (gc :full t)
  #-gencgc (ext:purify))

(pushnew :clx-ansi-common-lisp *features*)

;; I (rtoy) think we need this so the condition accessors are defined.
;; setup.lisp sets this to NIL, and the Pierre Mai's build scripts
;; load setup.lisp before clxcom.lisp.
#+pcl
(setf conditions::*make-condition-accessor-methods* t)

(with-compiler-log-file
    ("target:compile-clx.log"
     :optimize
     '(optimize (debug #-small 2 #+small .5) 
                (speed 2) (inhibit-warnings 2)
                (safety #-small 1 #+small 0))
     :optimize-interface
     '(optimize-interface (debug .5))
     :context-declarations
     '(((:and :external :global)
	(declare (optimize-interface (safety 2) (debug 1))))
       ((:and :external :macro)
	(declare (optimize (safety 2))))
       (:macro (declare (optimize (speed 0))))))
    (let ((c::*suppress-values-declaration* t))
      (comf "target:clx/package" :load t)
;      (comf "target:clx/defsystem" :load t)
      (comf "target:clx/depdefs" :load t)
      (comf "target:clx/clx" :load t)
      (comf "target:clx/dependent" :load t)
      (comf "target:clx/macros")	; these are just macros
      (load "target:clx/macros")
      (comf "target:clx/bufmac")	; these are just macros
      (load "target:clx/bufmac")
      (comf "target:clx/buffer" :load t)
      (comf "target:clx/display" :load t)
      (comf "target:clx/gcontext" :load t)
      (comf "target:clx/input" :load t)
      (comf "target:clx/requests" :load t)
      (comf "target:clx/fonts" :load t)
      (comf "target:clx/graphics" :load t)
      (comf "target:clx/text" :load t)
      (comf "target:clx/attributes" :load t)
      (comf "target:clx/translate" :load t)
      (comf "target:clx/keysyms" :load t)
      (comf "target:clx/manager" :load t)
      (comf "target:clx/image" :load t)
      (comf "target:clx/resource" :load t)
      (comf "target:clx/extensions/shape" :load t)
      (comf "target:clx/extensions/big-requests" :load t)
      (comf "target:clx/extensions/xvidmode" :load t)
      (comf "target:clx/extensions/xrender" :load t)
      (comf "target:clx/extensions/glx" :load t)
      (comf "target:clx/extensions/gl" :load t)
      (comf "target:clx/extensions/dpms" :load t)
      (comf "target:clx/extensions/xtest" :load t)
      (comf "target:clx/extensions/screensaver" :load t)
      (comf "target:clx/extensions/randr" :load t)
      (comf "target:clx/extensions/xinerama" :load t)
      (comf "target:clx/extensions/dbe" :load t)
      (comf "target:clx/extensions/xc-misc" :load t)
      (comf "target:clx/extensions/dri2" :load t)
      (comf "target:clx/extensions/composite" :load t)
      )
    (comf "target:code/clx-ext")
    (comf "target:hemlock/charmacs" :load t)
    (comf "target:hemlock/key-event" :load t)
    (comf "target:hemlock/keysym-defs" :load t)
    (comf "target:clx/provide")

    #+nil
    (comf "target:code/inspect"))

(cat-if-anything-changed
 "target:clx/clx-library"
 "target:clx/package"
 "target:clx/depdefs"
 "target:clx/clx"
 "target:clx/dependent"
 "target:clx/macros"
 "target:clx/bufmac"
 "target:clx/buffer"
 "target:clx/display"
 "target:clx/gcontext"
 "target:clx/input"
 "target:clx/requests"
 "target:clx/fonts"
 "target:clx/graphics"
 "target:clx/text"
 "target:clx/attributes"
 "target:clx/translate"
 "target:clx/keysyms"
 "target:clx/manager"
 "target:clx/image"
 "target:clx/resource"
 "target:clx/extensions/shape"
 "target:clx/extensions/big-requests"
 "target:clx/extensions/xvidmode"
 "target:clx/extensions/xrender"
 "target:clx/extensions/glx"
 "target:clx/extensions/gl"
 "target:clx/extensions/dpms"
 "target:clx/extensions/screensaver"
 "target:clx/extensions/xinerama"
 "target:clx/extensions/xtest"
 "target:code/clx-ext"
 "target:hemlock/charmacs"
 "target:hemlock/key-event"
 "target:hemlock/keysym-defs"
 "target:clx/provide")
