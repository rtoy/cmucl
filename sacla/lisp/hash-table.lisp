;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: hash-table.lisp,v 1.14 2004/09/02 06:59:43 yuji Exp $
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

(defun %print-hash-table (hash-table stream depth)
  (declare (ignore depth))
  (format stream "#<~A hash table (sumire), ~D entr~@:P>"
	  (symbol-name (hash-table-test hash-table))
	  (hash-table-count hash-table)))

;;(defun prime ()
;;  (do ((primes (list 3))
;;	 (n 5 (+ n 2)))
;;	((> n 10000))
;;    (dolist (prime primes (progn (push n primes) (print n)))
;;	(when (zerop (rem n prime))
;;	  (return)))))
;;
;;(defun prime-p (n)
;;  (do ((i 2 (1+ i)))
;;	((= i n) t)
;;    (when (zerop (rem n i))
;;	(return nil))))

(defun touch-up-size (size)
  (let ((primes '(211 307 401 503 601 701 809 907 1009 1259 1511 2003 3001
		  4001 5003 6007 7001 8009 9001 10007 12007 14009 16001 18013
		  20011 30011 40009 50021 60013 70001 80021 90001 100003)))
    (dolist (prime primes)
      (when (> prime size)
	(return-from touch-up-size prime))))
  (setq size (ceiling size))
  (when (zerop (rem size 2)) (incf size))
  (when (zerop (rem size 3)) (incf size 2))
  (when (zerop (rem size 7)) (incf size 4))
  size)

(defun calculate-rehash-count (size rehash-threshold)
  (floor (* size (max 0.2 rehash-threshold))))

(defstruct (hash-table
            (:constructor %make-hash-table)
	    (:print-function %print-hash-table))
  ""
  (count 0 :type (integer 0 *))
  (size (required-argument) :type (integer 0 *))
  (rehash-size (required-argument)
	       :type (or (integer 1 *) (float (1.0) *)) :read-only t)
  (rehash-threshold (required-argument) :type (real 0 1) :read-only t)
  (test (required-argument) :type symbol :read-only t)
  (test-function (required-argument) :type function :read-only t)
  (hash-function (required-argument) :type function :read-only t)
  (buckets (required-argument) :type vector)
  (rehash-count (required-argument) :type (integer 0 *)))

(defun make-hash-table (&key (test 'eql)
			     (size 67)
			     (rehash-size 1.5)
			     (rehash-threshold 0.5))
  (cond
   ((eq test #'eq)     (setq test 'eq))
   ((eq test #'eql)    (setq test 'eql))
   ((eq test #'equal)  (setq test 'equal))
   ((eq test #'equalp) (setq test 'equalp)))
  (let* ((hash-function (ecase test
			  (eq #'eq-hash)
			  (eql #'eql-hash)
			  (equal #'equal-hash)
			  (equalp #'equalp-hash)))
	 (size (touch-up-size size))
	 (buckets (make-array size :initial-element nil))
	 (rehash-count (calculate-rehash-count size rehash-threshold))
	 (hash-table (%make-hash-table :size size
				       :rehash-size rehash-size
				       :rehash-threshold rehash-threshold
				       :rehash-count rehash-count
				       :buckets buckets
				       :test test
				       :test-function (symbol-function test)
				       :hash-function hash-function)))
    hash-table))

(defun gethash (key hash-table &optional default)
  (let* ((hash (funcall (hash-table-hash-function hash-table) key))
	 (size (hash-table-size hash-table))
	 (test-function (hash-table-test-function hash-table))
	 (chain (aref (hash-table-buckets hash-table) (rem hash size))))
    (do ((plist chain (cddr plist)))
	((atom plist) (values default nil))
      (when (funcall test-function (car plist) key)
	(return (values (cadr plist) t))))))

(defun puthash (key value hash-table)
  (let* ((hash (funcall (hash-table-hash-function hash-table) key))
	 (size (hash-table-size hash-table))
	 (test-function (hash-table-test-function hash-table))
	 (buckets (hash-table-buckets hash-table))
	 (index (rem hash size))
	 (chain (aref buckets index)))
    (do ((plist chain (cddr plist)))
	((atom plist) (progn
			(setf (aref buckets index) (cons key (cons value chain)))
			(incf (hash-table-count hash-table))))
      (when (funcall test-function (car plist) key)
	(rplaca (cdr plist) value)
	(return))))
  value)

(defun rehash-hash-table (hash-table)
  (let* ((old-size (hash-table-size hash-table))
	 (old-buckets (hash-table-buckets hash-table))
	 (rehash-threshold (hash-table-rehash-threshold hash-table))
	 (rehash-size (hash-table-rehash-size hash-table))
	 (count (hash-table-count hash-table))
	 (size (touch-up-size (max (funcall (if (integerp rehash-size) #'+ #'*)
					    old-size rehash-size)
				   (/ count (max 0.5 rehash-threshold)))))
	 (buckets (make-array size :initial-element nil)))
    (setf (hash-table-count   hash-table) 0
	  (hash-table-size    hash-table) size
	  (hash-table-buckets hash-table) buckets
	  (hash-table-rehash-count hash-table) (calculate-rehash-count
						size rehash-threshold))
    (dotimes (i old-size)
      (do ((chain (aref old-buckets i) (cddr chain)))
	  ((atom chain))
	(puthash (car chain) (cadr chain) hash-table))))
    hash-table)

(defun (setf gethash) (value key hash-table &optional default)
  (declare (ignore default))
  (when (>= (hash-table-count hash-table) (hash-table-rehash-count hash-table))
    (rehash-hash-table hash-table))
  (puthash key value hash-table)
  value)

(defun remhash (key hash-table)
  (let* ((hash (funcall (hash-table-hash-function hash-table) key))
	 (size (hash-table-size hash-table))
	 (test-function (hash-table-test-function hash-table))
	 (buckets (hash-table-buckets hash-table))
	 (index (rem hash size))
	 (chain (aref buckets index)))
    (do ((plist chain (cddr plist))
	 (last nil (cdr plist)))
	((atom plist) nil)
      (when (funcall test-function (car plist) key)
	(if last
	    (rplacd last (cddr plist))
	  (setf (aref buckets index) (cddr plist)))
	(decf (hash-table-count hash-table))
	(return t)))))


(defun clrhash (hash-table)
  (let ((buckets (hash-table-buckets hash-table))
	(size (hash-table-size hash-table)))
    (dotimes (i size)
      (setf (elt buckets i) nil))
    (setf (hash-table-count hash-table) 0)
    hash-table))

(defun hash-table-iterator-1 (table)
  (let* ((index 0)
	 (size (hash-table-size table))
	 (chain (aref (hash-table-buckets table) 0)))
    #'(lambda ()
	(block iterator
	  (loop
	   (when chain (return))
	   (incf index)
	   (when (= index size) (return-from iterator nil))
	   (setq chain (aref (hash-table-buckets table) index)))
	  (multiple-value-prog1 (values t (first chain) (second chain))
	    (setq chain (cddr chain)))))))

(defun hash-table-iterator (hash-table-list)
  (let ((tables (%list hash-table-list)))
    (cond
      ((null tables) (constantly nil))
      ((null (rest tables)) (hash-table-iterator-1 (car tables)))
      (t (let ((iterator (hash-table-iterator-1 (pop tables))))
	   #'(lambda ()
	       (loop 
		(multiple-value-bind (more key value) (funcall iterator)
		  (cond
		    (more (return (values more key value)))
		    (tables (setq iterator (hash-table-iterator-1 (pop tables))))
		    (t (return nil)))))))))))

(defmacro with-hash-table-iterator ((name hash-table-form) &body body)
  (let ((iterator (gensym)))
    `(let ((,iterator (hash-table-iterator ,hash-table-form)))
      (declare (ignorable ,iterator))
      (macrolet ((,name () '(funcall ,iterator)))
	,@body))))


(defun maphash (function hash-table)
  (with-hash-table-iterator (next-entry hash-table)
    (loop (multiple-value-bind (more key value) (next-entry)
	    (unless more (return nil))
	    (funcall function key value)))))


(defun eq-hash (key)
  (sxhash key))
(defun eql-hash (key)
  (sxhash key))

(defun equal-hash (key)
  (sxhash key))

(defun equalp-hash (key)
  (typecase key
    (character (sxhash (char-upcase key)))
    (float (sxhash (rationalize key)))
    (cons 10)
    (array 20)
    (hash-table (logand (equalp-hash (hash-table-count key))
			(equalp-hash (hash-table-test key))))
    (structure-object (sxhash (class-of key)))
    (t (sxhash key))))

