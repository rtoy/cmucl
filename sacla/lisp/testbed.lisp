;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: testbed.lisp,v 1.4 2004/09/02 06:59:43 yuji Exp $
;; 
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;;  * Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;;  * Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in
;;    the documentation and/or other materials provided with the
;;    distribution.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(defpackage "TESTBED"
  (:nicknames "TB")
  (:shadow "DEFMACRO" "DEFINE-CONDITION")
  (:use "COMMON-LISP"))

(in-package "TESTBED")

(defun shadow-cl-symbol (name &optional type)
  (multiple-value-bind (cl-symbol status) (find-symbol name "CL")
    (when (eq status :external)
      (let ((symbol (progn (shadow name) (intern name "TB"))))
	;; type
	(when (and (not (eq type :condition))
                   (or (member cl-symbol
                               '(not and mod satisfies eql not values member or))
		       (subtypep cl-symbol t))
		   (not (member cl-symbol '(error))))
	  (eval `(deftype ,symbol (&rest rest)
		  (if rest
		      (cons ',cl-symbol rest)
		      ',cl-symbol))))
	;; function
	(when (and (not (eq type :function)) (fboundp cl-symbol)
                   (not (fboundp symbol)))
	  (setf (symbol-function symbol) (symbol-function cl-symbol)))
	;; variable
	(when (and (not (eq type :variable)) (boundp cl-symbol)
                   (not (boundp symbol)))
	  (setf (symbol-value symbol) (symbol-value cl-symbol)))
	;; (setf name)
	(when (and (not (eq type :setf)) (fboundp `(setf ,cl-symbol))
		   (not (fboundp `(setf ,symbol))))
	  (setf (fdefinition `(setf ,symbol)) (fdefinition `(setf ,cl-symbol)))))
      t)))

(defvar *testbed-compile* nil)

(cl:defmacro defmacro (symbol &rest rest)
  "testbed::defmacro"
  (let ((name (symbol-name symbol)))
    (shadow-cl-symbol name :function)
    (cond
      ((string= name "DEFINE-CONDITION")
       `(cl:defmacro new-define-condition ,@rest))
      (t `(progn
           (cl:defmacro ,(intern name) ,@rest)
           #-cmu
           (when *testbed-compile*
             (compile ',(intern name))) ;; cmucl 18e fails
           )))))

(cl:defmacro define-condition (symbol parent-types &rest rest)
  (let ((name (symbol-name symbol)))
    (shadow-cl-symbol name :condition)
    (if (fboundp 'new-define-condition)
        `(new-define-condition ,(intern name) ,parent-types ,@rest)
        (progn
          (setq parent-types (cond
                               ((null parent-types) (list (intern "CONDITION")))
                               (t parent-types)))
          `(cl:define-condition ,(intern name) ,parent-types ,@rest)))))

(defmacro defvar (symbol &rest rest)
  "testbed::defvar"
  (let ((name (symbol-name symbol)))
    (shadow-cl-symbol name :variable)
    `(cl:defvar ,(intern name) ,@rest)))

(defmacro defun (function-name &rest rest)
  "testbed::defun"
  (if (symbolp function-name)
      (let ((name (symbol-name function-name)))
	(shadow-cl-symbol name :function)
	`(progn
	   (cl:defun ,(intern name) ,@rest)
	   (when *testbed-compile*
	     (compile ',(intern name)))))
      `(cl:defun (setf ,(cadr function-name)) ,@rest)))

(defmacro defsetf (symbol &rest rest)
  "testbed::defsetf"
  (let ((name (symbol-name symbol)))
    (shadow-cl-symbol name :setf)
    (fmakunbound `(setf ,(intern name)))
    `(cl:defsetf ,(intern name) ,@rest)))

(defmacro define-setf-expander (symbol &rest rest)
  "testbed::define-setf-expander"
  (let ((name (symbol-name symbol)))
    (shadow-cl-symbol name :setf)
    (fmakunbound `(setf ,(intern name)))
    `(cl:define-setf-expander ,(intern name) ,@rest)))

(defun clone-package-system ()
  (error "clone-package-system is not defined in testbed.lisp."))

(defun ld (name)
  (cond
    ((string= name "cons")
     ;;(shadow '("CONS" "CONSP" "CAR" "CDR"))
     )
    ((string= name "hash-table")
     (shadow '("HASH-TABLE" "HASH-TABLE-P" "HASH-TABLE-COUNT"
	       "HASH-TABLE-REHASH-SIZE" "HASH-TABLE-REHASH-THRESHOLD"
	       "HASH-TABLE-SIZE" "HASH-TABLE-TEST") "TB"))
    ((string= name "package")		; needs hash-table
     (shadow '("PACKAGE" "PACKAGEP" "MAKE-PACKAGE" "FIND-PACKAGE"
	       "SHADOWING-IMPORT" "IMPORT" "USE-PACKAGE" "*PACKAGE*"
	       "DELETE-PACKAGE" "EXPORT") "TB")
     (defun symbol-package (symbol)
       (get symbol 'symbol-package))

     (defsetf symbol-package (symbol) (new-package)
       `(setf (get ,symbol 'symbol-package) ,new-package))
     )
    ((string= name "loop")		; needs hash-table and package
     )
    ((string= name "condition")
     (shadow '("CONDITION"
               "BREAK" "ASSERT"
               "CERROR" "ERROR" "SIGNAL" "WARN"
               
               "TYPE-ERROR-DATUM" "TYPE-ERROR-EXPECTED-TYPE"
               "PACKAGE-ERROR-PACKAGE" "PRINT-NOT-READABLE-OBJECT"
               "FILE-ERROR-PATHNAME" "STREAM-ERROR-STREAM"
               "CELL-ERROR-NAME" "UNBOUND-SLOT-INSTANCE"
               "ARITHMETIC-ERROR-OPERATION" "ARITHMETIC-ERROR-OPERANDS"
               "SIMPLE-CONDITION-FORMAT-CONTROL"
               "SIMPLE-CONDITION-FORMAT-ARGUMENTS"
               
	       "CHECK-TYPE" "HANDLER-BIND" "HANDLER-CASE"
	       "INVOKE-RESTART" "RESTART" "MAKE-RESTART"
	       "RESTART-NAME" "RESTART-FUNCTION" "RESTART-REPORT-FUNCTION"
	       "RESTART-INTERACTIVE-FUNCTION" "RESTART-TEST-FUNCTION"
	       "RESTART-CASE" "RESTART-BIND"
	       "ABORT" "MUFFLE-WARNING" "CONTINUE" "STORE-VALUE" "USE-VALUE"
	       )
	     "TB")

     (eval `(cl:define-condition ,(intern "CONDITION") (cl:condition) ()))
     
     )
    ((string= name "reader")
     (shadow '("READTABLE" "READTABLEP" "READTABLE-CASE" "*READTABLE*"
               "READER-ERROR") "TB"))
    ((string= name "printer")		; needs reader
     (shadow '("PRINT-OBJECT")))
    ((string= name "sequence")
     (shadow '("SEARCH")))
    ((string= name "clos")
     )
    )
  (let ((*package* (find-package "TESTBED")))
    (funcall #'load (concatenate 'string cl-user::*sacla-lisp-dir* "/" name))
    
    (cond
      ((string= name "package")
       (format t "~%Cloning the package system!~%")
       (clone-package-system))
      ((string= name "loop")
       )))
  )

(defun test (name)
  (let ((tests (with-open-file (in (concatenate 'string
                                                cl-user::*sacla-lisp-tests-dir*
                                                "/" name ".lisp"))
                 (loop for sexp = (read in nil)
                       while sexp
                       collect sexp))))
    (format t "Testing ~d tests in ~S~%" (length tests) name)
    (do* ((count 1 (1+ count))
          (failed 0)
          (skipped 0)
          (err nil nil)
          (tests tests (cdr tests)))
         ((null tests)
          (format t "~%All = ~d~%OK = ~d~%Skipped = ~d~%Failed = ~d~%"
                  (1- count) (- count 1 skipped failed) skipped failed)
          (return (zerop failed)))
      (format t "~d " count)
      (case (handler-case (eval (first tests)) (error (e) (setq err e) nil))
        ((nil)
         (format t "Failed : ~S~%" (first tests))
         (when err (print err))
         (incf failed))
        (skipped
         (format t "Skipped ")
         (incf skipped))))))
