;;; -*- Log: code.log; Mode: Lisp; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    BACKQUOTE: Code Spice Lispified by Lee Schumacher.
;;;
(in-package 'lisp)


;;; The flags passed back by BACKQUOTIFY can be interpreted as follows:
;;;
;;;   |`,|: [a] => a
;;;    NIL: [a] => a		;the NIL flag is used only when a is NIL
;;;      T: [a] => a		;the T flag is used when a is self-evaluating
;;;  QUOTE: [a] => (QUOTE a)
;;; APPEND: [a] => (APPEND . a)
;;;  NCONC: [a] => (NCONC . a) 
;;;   LIST: [a] => (LIST . a)
;;;  LIST*: [a] => (LIST* . a)
;;;
;;; The flags are combined according to the following set of rules:
;;;  ([a] means that a should be converted according to the previous table)
;;;
;;;   \ car  ||    otherwise    |    QUOTE or     |     |`,@|      |     |`,.|
;;;cdr \     ||                 |    T or NIL     |                |		 
;;;================================================================================
;;;  |`,|    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC  (a [d])
;;;  NIL     || LIST    ([a])   | QUOTE    (a)    | <hair>    a    | <hair>    a
;;;QUOTE or T|| LIST* ([a] [d]) | QUOTE  (a . d)  | APPEND (a [d]) | NCONC (a [d])
;;; APPEND   || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a . d) | NCONC (a [d])
;;; NCONC    || LIST* ([a] [d]) | LIST* ([a] [d]) | APPEND (a [d]) | NCONC (a . d)
;;;  LIST    || LIST  ([a] . d) | LIST  ([a] . d) | APPEND (a [d]) | NCONC (a [d])
;;;  LIST*   || LIST* ([a] . d) | LIST* ([a] . d) | APPEND (a [d]) | NCONC  (a [d])
;;;
;;;<hair> involves starting over again pretending you had read ".,a)" instead
;;; of ",@a)"

(defvar *backquote-count* 0  "How deep we are into backquotes")
(defvar *bq-comma-flag* '(|,|))
(defvar *bq-at-flag* '(|,@|))
(defvar *bq-dot-flag* '(|,.|))
(defvar *bq-vector-flag* '(|bqv|))


;; This is the actual character macro.
(defun backquote-macro (stream ignore)
  (declare (ignore ignore))
  (let ((*backquote-count* (1+ *backquote-count*)))
    (multiple-value-bind (flag thing)
			 (backquotify (read stream t nil t))
      (if (eq flag *bq-at-flag*)
	  (error ",@ after backquote in ~S" thing))
      (if (eq flag *bq-dot-flag*)
	  (error ",. after backquote in ~S" thing))
      (values (backquotify-1 flag thing) 'list))))

(defun comma-macro (stream ignore)
  (declare (ignore ignore))
  (unless (> *backquote-count* 0)
    (when *read-suppress*
      (return-from comma-macro nil))
    (error "Comma not inside a backquote."))
  (let ((c (read-char stream))
	(*backquote-count* (1- *backquote-count*)))
    (values
     (cond ((char= c #\@)
	    (cons *bq-at-flag* (read stream t nil t)))
	   ((char= c #\.)
	    (cons *bq-dot-flag* (read stream t nil t)))
	   (t (unread-char c stream)
	      (cons *bq-comma-flag* (read stream t nil t))))
     'list)))

;;; This does the expansion from table 2.
(defun backquotify (code)
  (cond ((atom code)
	 (cond ((null code) (values nil nil))
	       ((or (numberp code)
		    (eq code t))
		;; Keywords are self evaluating. Install after packages.
		(values t code))
	       (t (values 'quote code))))
	((or (eq (car code) *bq-at-flag*)
	     (eq (car code) *bq-dot-flag*))
	 (values (car code) (cdr code)))
	((eq (car code) *bq-comma-flag*)
	 (comma (cdr code)))
	((eq (car code) *bq-vector-flag*)
	 (multiple-value-bind (dflag d) (backquotify (cdr code))
	   (values 'vector (backquotify-1 dflag d))))
	(t (multiple-value-bind (aflag a) (backquotify (car code))
	     (multiple-value-bind (dflag d) (backquotify (cdr code))
	       (if (eq dflag *bq-at-flag*)
		   ;; get the errors later.
		   (error ",@ after dot in ~S" code))
	       (if (eq dflag *bq-dot-flag*)
		   (error ",. after dot in ~S" code))
	       (cond
		((eq aflag *bq-at-flag*)
		 (if (null dflag)
		     (comma a)
		     (values 'append
			     (cond ((eq dflag 'append)
				    (cons a d ))
				   (t (list a (backquotify-1 dflag d)))))))
		((eq aflag *bq-dot-flag*)
		 (if (null dflag)
		     (comma a)
		     (values 'nconc
			     (cond ((eq dflag 'nconc)
				    (cons a d))
				   (t (list a (backquotify-1 dflag d)))))))
		((null dflag)
		 (if (memq aflag '(quote t nil))
		     (values 'quote (list a))
		     (values 'list (list (backquotify-1 aflag a)))))
		((memq dflag '(quote t))
		 (if (memq aflag '(quote t nil))
		     (values 'quote (cons a d ))
		     (values 'list* (list (backquotify-1 aflag a)
					  (backquotify-1 dflag d)))))
		(t (setq a (backquotify-1 aflag a))
		   (if (memq dflag '(list list*))
		       (values dflag (cons a d))
		       (values 'list*
			       (list a (backquotify-1 dflag d)))))))))))

;;; This handles the <hair> cases 
(defun comma (code)
  (cond ((atom code)
	 (cond ((null code)
		(values nil nil))
	       ((or (numberp code) (eq code 't))
		(values t code))
	       (t (values *bq-comma-flag* code))))
	((eq (car code) 'quote)
	 (values (car code) (cadr code)))
	((memq (car code) '(append list list* nconc))
	 (values (car code) (cdr code)))
	((eq (car code) 'cons)
	 (values 'list* (cdr code)))
	(t (values *bq-comma-flag* code))))

;;; This handles table 1.
(defun backquotify-1 (flag thing)
  (cond ((or (eq flag *bq-comma-flag*)
	     (memq flag '(t nil)))
	 thing)
	((eq flag 'quote)
	 (list  'quote thing))
	((eq flag 'list*)
	 (cond ((null (cddr thing))
		(cons 'cons thing))
	       (t (cons 'list* thing))))
	((eq flag 'vector)
	 (list 'apply '#'vector thing))
	(t (cons (cdr
		  (assq flag
			`((cons . cons) (list . list)
			  (append . append) (nconc . nconc))))
		 thing))))




(defun backq-init ()
  (let ((*readtable* std-lisp-readtable))
    (set-macro-character #\` #'backquote-macro)
    (set-macro-character #\, #'comma-macro)))
