;;; -*- Log: code.log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the CMU Common Lisp project at
;;; Carnegie Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of CMU Common Lisp, please contact
;;; Scott Fahlman or slisp-group@cs.cmu.edu.
;;;
(ext:file-comment
  "$Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/code/sharpm.lisp,v 1.4 1991/02/08 13:35:39 ram Exp $")
;;;
;;; **********************************************************************
;;;
;;; Spice Lisp Interim Sharp Macro
;;; Written by David Dill
;;; Runs in the standard Spice Lisp environment.
;;; This uses the special std-lisp-readtable, which is internal to READER.LISP
;;;
;;; ****************************************************************
(in-package 'lisp)


;;; declared in READ.LISP

(proclaim '(special *read-suppress* std-lisp-readtable *bq-vector-flag*))


(defun sharp-backslash (stream backslash ignore)
  (declare (ignore ignore))
  (unread-char backslash stream)
  (let* ((*readtable* std-lisp-readtable)
	 (charstring (read-extended-token stream)))
    (declare (simple-string charstring))
    (cond (*read-suppress* nil)
	  ((= (the fixnum (length charstring)) 1)
	   (char charstring 0))
	  ((name-char charstring))
	  (t
	   (error "Meaningless character name: ~S" charstring)))))



(defun sharp-quote (stream ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  ;; 4th arg tells read that this is a recrusive call.
  `(function ,(read stream () () t)))

(defun sharp-left-paren (stream ignore length)
  (declare (ignore ignore))
  (declare (special *backquote-count*))
  (let* ((list (read-list stream nil))
	 (listlength (length list)))
    (declare (list list)
	     (fixnum listlength))
    (cond (*read-suppress*)
	  ((zerop *backquote-count*)
	   (if length
	       (cond ((> listlength (the fixnum length))
		      (error
		       "Vector longer than specified length: #~S~S"
		       length list))
		     (t
		      (fill (the simple-vector
				 (replace (the simple-vector (make-array length))
					  list))
			    (car (last list))
			    :start listlength)))
	       (coerce list 'vector)))
	  (t (cons *bq-vector-flag* list)))))

(defun sharp-star (stream ignore numarg)
  (declare (ignore ignore))
  (multiple-value-bind (bstring escape-appearedp)
		       (read-extended-token stream)
    (declare (simple-string bstring))
    (cond (*read-suppress*)
	  (escape-appearedp
	   (error "Escape character appeared after #*"))
	  ((and numarg (zerop (length bstring)) (not (zerop numarg)))
	   (error "You have to give a little bit for non-zero #* bit-vectors."))
	  ((or (null numarg) (>= (the fixnum numarg) (length bstring)))
	   (let* ((len1 (length bstring))
		  (last1 (1- len1))
		  (len2 (or numarg len1))
		  (bvec (make-array len2 :element-type 'bit
				    :initial-element 0)))
	     (declare (fixnum len1 last1 len2))
	     (do ((i 0 (1+ i))
		  (char ()))
		 ((= i len2))
	       (declare (fixnum i))
	       (setq char (elt bstring (if (< i len1) i last1)))
	       (setf (elt bvec i)
		     (cond ((char= char #\0) 0)
			   ((char= char #\1) 1)
			   (t
			    (error "Illegal element given for ~
					  bitvector #~A*~A"
				    numarg bstring)))))
	     bvec))
	  (t
	   (error "Bit vector is longer than specified length #~A*~A"
		   numarg bstring)))))


(defun sharp-colon (stream ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  (when *read-suppress*
	(read stream () () t)
	(return-from sharp-colon nil))
  (let ((token (read-extended-token stream)))
    (declare (simple-string token))
    (cond (*read-suppress*)
	  ((find #\: token)
	   (error "Symbol following #: contains a #\: ~S" token))
	  ((eql (length token) 0)
	   (let ((ch (read-char stream nil nil t)))
	     (if ch
		 (error "Illegal terminating character after a colon, ~S." ch)
		 (error "Illegal terminating character after a colon."))))
	  (T (make-symbol token)))))

(defun sharp-dot (stream ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  (let ((token (read stream () () t)))
    (unless *read-suppress*  (eval token))))

(defun sharp-comma (stream ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  (let ((token (read stream () () t)))
    (unless *read-suppress*  (eval token))))

(defun sharp-R (stream ignore radix)
  (declare (ignore ignore))
  (multiple-value-bind (token escape-appearedp)
		       (read-extended-token stream)
    (declare (simple-string token))
    (when *read-suppress* (return-from sharp-R nil))
    (let ((numval 0) (denval 0) (resttok 0) (toklength (length token))
	  (sign 1))
      (declare (fixnum toklength))
      (if escape-appearedp
	  (error "Escape character appears in number."))
      ;;look for leading sign
      (let ((firstchar (elt token 0)))
	(cond ((char= firstchar #\-)
	       (setq sign -1)
	       (setq resttok 1))
	      ((char= firstchar #\+)
	       (setq resttok 1))))
      ;;read numerator
      (do ((position resttok (1+ position))
	   (dig ()))
	  ((or (>= position toklength)
	       (not (setq dig (digit-char-p (elt token position) radix))))
	   (setq resttok position))
	(setq numval (+ (* numval radix) dig)))
      ;;see if we're at the end.
      (cond ((>= resttok toklength)
	     ;;just return numerator -- that's all there is.
	     (* numval sign))
	    ((char= (elt token resttok) #\/)
	     ;;it's a ratio.
	     (do ((position (1+ resttok) (1+ position))
		  (dig ())
		  (retval ()))
		 ((cond ((>= position toklength)
			 (setq retval (/ (* numval sign) denval)))
			((not (setq dig (digit-char-p (elt token position)
						     radix)))
			 ;;there's bogus stuff at the end
			 (error
				 "Illegal digits ~S for radix ~S" token radix)
			 (setq retval (/ (* numval sign) denval)))
			;;continue looping
			(t nil))
		  retval)
	       (setq denval (+ (* denval radix) dig))))
	    ;;it's bogus
	    (t (error
		       "Illegal digits ~S for radix ~S" token radix)))))))

(defun sharp-B (stream ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  (sharp-r stream nil 2))

(defun sharp-O (stream ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  (sharp-r stream nil 8))

(defun sharp-X (stream ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  (sharp-r stream nil 16))

(defun sharp-A (stream ignore dimensions)
  (declare (ignore ignore))
  (when *read-suppress*
    (read stream () () t)
    (return-from sharp-A nil))
  (unless dimensions (error "No dimensions argument to #A."))
  (unless (and (integerp dimensions) (>= dimensions 0))
    (error "Dimensions argument to #A not a non-negative integer: ~S"
	   dimensions))
  (if (> dimensions 0)
      (let ((dlist (make-list dimensions))
	    (init-list
	     (if (char= (read-char stream t) #\( #|)|#)
		 (read-list stream nil)
		 (error "Array values must be a list."))))
	(do ((dl dlist (cdr dl))
	     (il init-list (car il)))
	    ;; I think the nreverse is causing the problem.
	    ((null dl))
	    (if (listp il)
		(rplaca dl (length il))
		(error
		 "Initial contents for #A is inconsistent with ~
		 dimensions: #~SA~S" dimensions init-list)))
	(make-array dlist :initial-contents init-list))
      (make-array nil :initial-element (read stream t nil t))))


(defun sharp-S (stream ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  ;;this needs to know about defstruct implementation
  (when *read-suppress*
	(read stream () () t)
	(return-from sharp-S nil))
  (let ((body
	 (if (char= (read-char stream t) #\( )
	     (read-list stream nil)
	     (error "Non-list following #S"))))
    (cond ((listp body)
	   (unless (symbolp (car body))
	     (error "Structure type is not a symbol: ~S" (car body)))
	   (let ((defstruct (info type defined-structure-info (car body))))
	     (unless defstruct
	       (error "~S is not a defined structure type." (car body)))
	     (unless (c::dd-constructor defstruct)
	       (error "The ~S structure does not have a default constructor." (car body)))
	     (do ((arg (cdr body) (cddr arg))
		  (res ()))
		 ((endp arg) (apply (c::dd-constructor defstruct) res))
	       (push (cadr arg) res)
	       (push (intern (string (car arg)) *keyword-package*) res))))
	  (t (error "Non-list following #S: ~S" body))))))

(defmacro int-subst-array (new old array rank var-list)
  (if (> rank (array-rank array))
      (let ((new-list (nreverse var-list)))
       `(if (eq ,old (aref ,array ,@new-list))
	    (setf (aref ,array ,@new-list) ,new)))
       (let ((newvar (gensym)))
	   `(dotimes (,newvar (array-dimension ,array ,rank))
	      (int-subst-array ,new ,old ,array (1+ ,rank)
			       (push ,newvar ,var-list))))))

(defmacro subst-array (new old array)
  `(int-subst-array ,new ,old ,array 0 nil))

(defvar sharp-cons-table ()
  "Holds the cons cells seen already by circle-subst")

;; This function is the same as nsubst, except that it checks for circular
;; lists. the first arg is an alist of the things to be replaced assoc'd with
;; the things to replace them.
(defun circle-subst (old-new-alist tree)
  (cond ((and (atom tree)
	      (not (and (arrayp tree)
			(eq (array-element-type tree) t))))
	 (let ((pair (assq tree old-new-alist)))
	   (if pair (cdr pair) tree)))
	((null (gethash tree sharp-cons-table))
	 (setf (gethash tree sharp-cons-table) t)
	 (cond ((simple-vector-p tree)
		(do ((i 0 (1+ i))
		     (len (length tree)))
		    ((>= i len))
		  (declare (fixnum i len))
		  (setf (svref tree i)
			(circle-subst old-new-alist (svref tree i))))
		tree)
	       ((arrayp tree)
		(with-array-data ((data tree) (start) (end))
		  (declare (fixnum start end))
		  (do ((i start (1+ i)))
		      ((>= i end))
		    (setf (aref data i)
			  (circle-subst old-new-alist (aref data i)))))
		tree)
	       (T (let ((a (circle-subst old-new-alist (car tree)))
			(d (circle-subst old-new-alist (cdr tree))))
		    (if (eq a (car tree))
			tree
			(rplaca tree a))
		    (if (eq d (cdr tree))
			tree
			(rplacd tree d)))
		  tree)))
	(t tree)))

;; Sharp-equal works as follows.  When a label is assigned
;; (ie when #= is called) a symbol (ref) is gensym'd and
;; a cons cell whose car is the label, and cdr is the symbol
;; is put on the sharp-sharp alist.  When sharp-sharp encounters
;; a reference to a label it returns the symbol assoc'd with the label.
;; When an object has been read then a cons cell whose car is the symbol
;; and cdr is the object is pushed onto the sharp-sharp-alist.  Then
;; for each cons cell on the sharp-sharp-alist, the current object is searched
;; and where a symbol eq to the car of the current cons cell is found,
;; the object is substituted in.
(defun sharp-equal (stream ignore label &aux (ref (gensym)))
  (declare (ignore ignore))
  (declare (special sharp-equal-alist sharp-sharp-alist))
  (when *read-suppress* (return-from sharp-equal (values)))
  (unless (integerp label)
	  (error "non-integer label #~S=" label))
  (push (cons label ref) sharp-sharp-alist)
  (let ((obj (read stream () () t)))
    (push (cons ref obj) sharp-equal-alist)
    (clrhash sharp-cons-table)
    (circle-subst sharp-equal-alist obj)))

(defun sharp-sharp (ignore1 ignore2 label)
  (declare (ignore ignore1 ignore2))
  (declare (special sharp-equal-alist sharp-sharp-alist))
  (when *read-suppress* (return-from sharp-sharp nil))
  (if (integerp label)
      (let ((pair (assoc label sharp-sharp-alist)))
	(if pair
	    (let ((ret-obj (cdr (assoc (cdr pair) sharp-equal-alist))))
	      (if ret-obj ret-obj
		  (cdr pair)))
	    (error "Object is not labelled #~S#" label)))
      (error "Non-integer label #~S#" label)))

(defun sharp-plus (stream ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  (cond (*read-suppress*
	 (read stream () () t)
	 (values))
	((featurep (let ((*package* *keyword-package*))
		     (read stream () () t)))
	 (read stream () () t))
	(t (let ((*read-suppress* t))
	     (read stream () () t)
	     (values)))))

(defun sharp-minus (stream ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  (cond (*read-suppress*
	 (read stream () () t)
	 (values))
	((not (featurep (let ((*package* *keyword-package*))
			  (read stream () () t))))
	 (read stream () () t))
	(t (let ((*read-suppress* t))      
	     (read stream () () t)
	     (values)))))

(defun sharp-C (stream ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  ;;next thing better be a list of two numbers.
  (let ((cnum (read stream () () t)))
    (when *read-suppress* (return-from sharp-c nil))
    (if (= (length cnum) 2)
	(complex (car cnum) (cadr cnum))
	(error "Illegal complex number format" cnum))))

(defun sharp-vertical-bar (stream ignore1 ignore2)
  (declare (ignore ignore1 ignore2))
  (prepare-for-fast-read-char stream
    (do ((level 1)
	 (prev (fast-read-char) char)
	 (char (fast-read-char) (fast-read-char)))
	(())
      (cond ((and (char= prev #\|) (char= char #\#))
	     (setq level (1- level))
	     (when (zerop level)
	       (done-with-fast-read-char)
	       (return (values)))
	     (setq char (fast-read-char)))
	    ((and (char= prev #\#) (char= char #\|))
	     (setq char (fast-read-char))
	     (setq level (1+ level)))))))

(defun sharp-illegal (ignore1 sub-char ignore2)
  (declare (ignore ignore1 ignore2))
  (error "Illegal sharp character ~S" sub-char))


(defun sharp-init ()
  (declare (special std-lisp-readtable))
  (setq sharp-cons-table (make-hash-table :size 50))
  (let ((*readtable* std-lisp-readtable))
    (make-dispatch-macro-character #\#)
    (set-dispatch-macro-character #\# #\\ #'sharp-backslash)
    (set-dispatch-macro-character #\# #\' #'sharp-quote)
    (set-dispatch-macro-character #\# #\( #'sharp-left-paren)
    (set-dispatch-macro-character #\# #\* #'sharp-star)
    (set-dispatch-macro-character #\# #\: #'sharp-colon)
    (set-dispatch-macro-character #\# #\. #'sharp-dot)
    (set-dispatch-macro-character #\# #\, #'sharp-comma)
    (set-dispatch-macro-character #\# #\R #'sharp-R)
    (set-dispatch-macro-character #\# #\r #'sharp-R)
    (set-dispatch-macro-character #\# #\B #'sharp-B)
    (set-dispatch-macro-character #\# #\b #'sharp-B)
    (set-dispatch-macro-character #\# #\O #'sharp-O)
    (set-dispatch-macro-character #\# #\o #'sharp-O)
    (set-dispatch-macro-character #\# #\X #'sharp-X)
    (set-dispatch-macro-character #\# #\x #'sharp-X)
    (set-dispatch-macro-character #\# #\A #'sharp-A)
    (set-dispatch-macro-character #\# #\a #'sharp-A)
    (set-dispatch-macro-character #\# #\S #'sharp-S)
    (set-dispatch-macro-character #\# #\s #'sharp-S)
    (set-dispatch-macro-character #\# #\= #'sharp-equal)
    (set-dispatch-macro-character #\# #\# #'sharp-sharp)
    (set-dispatch-macro-character #\# #\+ #'sharp-plus)
    (set-dispatch-macro-character #\# #\- #'sharp-minus)
    (set-dispatch-macro-character #\# #\C #'sharp-C)
    (set-dispatch-macro-character #\# #\c #'sharp-C)
    (set-dispatch-macro-character #\# #\| #'sharp-vertical-bar)
    (set-dispatch-macro-character #\# #\tab #'sharp-illegal)
    (set-dispatch-macro-character #\# #\  #'sharp-illegal)
    (set-dispatch-macro-character #\# #\) #'sharp-illegal)
    (set-dispatch-macro-character #\# #\< #'sharp-illegal)
    (set-dispatch-macro-character #\# #\form #'sharp-illegal)
    (set-dispatch-macro-character #\# #\return #'sharp-illegal)))
