(in-package "USER")

;;; Hide CLOS from CLX, so objects stay implemented as structures.
;;;
(when (find-package "CLOS")
  (rename-package (find-package "CLOS") "NO-CLOS-HERE"))
(when (find-package "PCL")
  (rename-package (find-package "PCL") "NO-PCL-HERE"))

#+bootstrap
(unless (find-package "OLD-XLIB")
  (when (find-package "XLIB")
    (rename-package (find-package "XLIB") "OLD-XLIB"))
  
  (make-package "XLIB" :use '("LISP")))

(with-compiler-log-file
    ("target:compile-clx.log"
     :optimize
     '(optimize (debug-info #-small 2 #+small .5) 
		(speed 2) (inhibit-warnings 2)
		(safety #-small 1 #+small 0))
     :optimize-interface
     '(optimize-interface (debug-info .5))
     :context-declarations
     '(((:and :external :global)
	(declare (optimize-interface (safety 2) (debug-info 1))))
       ((:and :external :macro)
	(declare (optimize (safety 2))))))
  (let ((c::*suppress-values-declaration* t))
    (comf "target:clx/package" :load t)
    (comf "target:clx/defsystem" :load t)
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
    (comf "target:clx/resource" :load t))
  (comf "target:code/clx-ext"))
