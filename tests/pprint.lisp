;; Tests for pprinter

(defpackage :pprint-tests
  (:use :cl :lisp-unit))

(in-package "PPRINT-TESTS")

(define-test pprint.with-float-traps-masked
    (:tag :issues)
  (assert-equal 
"
(WITH-FLOAT-TRAPS-MASKED (:UNDERFLOW)
  (PRINT \"Hello\"))"
     (with-output-to-string (s)
       (pprint '(ext:with-float-traps-masked (:underflow)
                 (print "Hello"))
               s))))

(define-test pprint.with-float-traps-enabled
    (:tag :issues)
  (assert-equal 
"
(WITH-FLOAT-TRAPS-ENABLED (:UNDERFLOW)
  (PRINT \"Hello\"))"
     (with-output-to-string (s)
       (pprint '(ext:with-float-traps-enabled (:underflow)
                 (print "Hello"))
               s))))

(define-test pprint.handler-case.1
    (:tag :issues)
  ;; Just an expression
  (assert-equal 
   "
(HANDLER-CASE (SIGNAL CONDITION))"
    (with-output-to-string (s)
      (pprint '(handler-case (signal condition))
              s))))

(define-test pprint.handler-case.2
    (:tag :issues)
  ;; One error clause
  (assert-equal 
   "
(HANDLER-CASE (SIGNAL CONDITION)
  (WARNING NIL
    \"Lots of smoke, but no fire.\"))"
    (with-output-to-string (s)
      (pprint '(handler-case (signal condition)
                (warning () "Lots of smoke, but no fire."))
              s))))

(define-test pprint.handler-case.3
    (:tag :issues)
  ;; More than one error clause
  (assert-equal 
   "
(HANDLER-CASE (SIGNAL CONDITION)
  (WARNING NIL
    \"Lots of smoke, but no fire.\")
  ((OR ARITHMETIC-ERROR CONTROL-ERROR CELL-ERROR STREAM-ERROR) (CONDITION)
    (FORMAT NIL \"~S looks especially bad.\" CONDITION)))"
   (with-output-to-string (s)
     (pprint '(handler-case (signal condition)
               (warning () "Lots of smoke, but no fire.")
               ((or arithmetic-error control-error cell-error stream-error)
                (condition)
                (format nil "~S looks especially bad." condition)))
             s))))

(define-test pprint.handler-case.4
    (:tag :issues)
  ;; An expression and a no-error clause
  (assert-equal 
   "
(HANDLER-CASE (SIGNAL CONDITION)
  (:NO-ERROR NIL
    (FORMAT NIL \"Nothing bad happened.\")))"
   (with-output-to-string (s)
     (pprint '(handler-case (signal condition)
               (:no-error ()
                (format nil "Nothing bad happened.")))
             s))))


(define-test pprint.handler-case.5
    (:tag :issues)
  ;; One error clause and a no-error clause
  (assert-equal 
   "
(HANDLER-CASE (SIGNAL CONDITION)
  (WARNING NIL
    \"Lots of smoke, but no fire.\")
  (:NO-ERROR NIL
    (FORMAT NIL \"Nothing bad happened.\")))"
   (with-output-to-string (s)
     (pprint '(handler-case (signal condition)
               (warning () "Lots of smoke, but no fire.")
               (:no-error ()
                (format nil "Nothing bad happened.")))
             s))))

(define-test pprint.handler-case.6
    (:tag :issues)
  ;; More than one error clause and a no-error clause
  (assert-equal 
   "
(HANDLER-CASE (SIGNAL CONDITION)
  (WARNING NIL
    \"Lots of smoke, but no fire.\")
  ((OR ARITHMETIC-ERROR CONTROL-ERROR CELL-ERROR STREAM-ERROR) (CONDITION)
    (FORMAT NIL \"~S looks especially bad.\" CONDITION))
  (:NO-ERROR NIL
    (FORMAT NIL \"Nothing bad happened.\")))"
   (with-output-to-string (s)
     (pprint '(handler-case (signal condition)
               (warning () "Lots of smoke, but no fire.")
               ((or arithmetic-error control-error cell-error stream-error)
                (condition)
                (format nil "~S looks especially bad." condition))
               (:no-error ()
                (format nil "Nothing bad happened.")))
             s))))

(define-test pprint.assemble
    (:tag :issues)
  (assert-equal
   "
(NEW-ASSEM:ASSEMBLE (C:*CODE-SEGMENT* 'X86::XOROSHIRO-UPDATE)
    X86::XOROSHIRO-UPDATE
  (PUSH (CONS 'X86::XOROSHIRO-UPDATE X86::XOROSHIRO-UPDATE)
        C::*ASSEMBLER-ROUTINES*))"
   (with-output-to-string (s)
     (pprint '(new-assem:assemble (c::*code-segment* 'vm::xoroshiro-update)
               vm::xoroshiro-update
               (push (cons 'vm::xoroshiro-update vm::xoroshiro-update) c::*assembler-routines*))
             s))))
