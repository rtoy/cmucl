;; Define source-location/class info type so that code/error.lisp can
;; be compiled.
(in-package c)
(define-info-type source-location class (or form-numbers null) nil)
