;; Need to add a new ef macro id for OCTETS-TO-STRING-COUNTED.
;;
;; 2010-09 (probably) needs to be cross-compiled from 2010-08 (aka
;; 20b-pre1).  Use something like
;;
;; src/tools/cross-build-world.sh -crl -B src/bootfiles/20a/boot-2010-08-1.lisp target/ cross src/tools/cross-scripts/cross-x86-x86.lisp <20b/bin/lisp>

(in-package "STREAM")

(ext:without-package-locks

  (handler-bind ((error (lambda (c)
			  (declare (ignore c))
			  (invoke-restart 'kernel::continue))))
    (vm::defenum (:prefix "+EF-" :suffix "+" :start 1)
      str				; string length
      cin				; input a character
      cout				; output a character
      sin				; input string
      sout				; output string
      os				; octets to string
      so				; string to octets
      en				; encode
      de				; decode
      flush				; flush state
      copy-state			; copy state
      osc
      max)))