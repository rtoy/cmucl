;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: cons.lisp,v 1.4 2004/02/20 07:23:42 yuji Exp $
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

(defun atom (object)
  "Return true if OBJECT is of type atom; otherwise, return false."
  (not (consp object)))

(defun caar (list)
  "Return the car of the car of LIST."
  (car (car list)))

(defun cadr (list)
  "Return the car of the cdr of LIST."
  (car (cdr list)))

(defun cdar (list)
  "Return the cdr of the car of LIST."
  (cdr (car list)))

(defun cddr (list)
  "Return the cdr of the cdr of LIST."
  (cdr (cdr list)))

(defun caaar (list)
  "Return the car of the caar of LIST."
  (car (caar list)))

(defun caadr (list)
  "Return the car of the cadr of LIST."
  (car (cadr list)))

(defun cadar (list)
  "Return the car of the cdar of LIST."
  (car (cdar list)))

(defun caddr (list)
  "Return the car of the cddr of LIST."
  (car (cddr list)))

(defun cdaar (list)
  "Return the cdr of the caar of LIST."
  (cdr (caar list)))

(defun cdadr (list)
  "Return the cdr of the cadr of LIST."
  (cdr (cadr list)))

(defun cddar (list)
  "Return the cdr of the cdar of LIST."
  (cdr (cdar list)))

(defun cdddr (list)
  "Return the cdr of the cddr of LIST."
  (cdr (cddr list)))

(defun caaaar (list)
  "Return the car of the caaar of LIST."
  (car (caaar list)))

(defun caaadr (list)
  "Return the car of the caadr of LIST."
  (car (caadr list)))

(defun caadar (list)
  "Return the car of the cadar of LIST."
  (car (cadar list)))

(defun caaddr (list)
  "Return the car of the caddr of LIST."
  (car (caddr list)))

(defun cadaar (list)
  "Return the car of the cdaar of LIST."
  (car (cdaar list)))

(defun cadadr (list)
  "Return the car of the cdadr of LIST."
  (car (cdadr list)))

(defun caddar (list)
  "Return the car of the cddar of LIST."
  (car (cddar list)))

(defun cadddr (list)
  "Return the car of the cdddr of LIST."
  (car (cdddr list)))

(defun cdaaar (list)
  "Return the cdr of the caaar of LIST."
  (cdr (caaar list)))

(defun cdaadr (list)
  "Return the cdr of the caadr of LIST."
  (cdr (caadr list)))

(defun cdadar (list)
  "Return the cdr of the cadar of LIST."
  (cdr (cadar list)))

(defun cdaddr (list)
  "Return the cdr of the caddr of LIST."
  (cdr (caddr list)))

(defun cddaar (list)
  "Return the cdr of the cdaar of LIST."
  (cdr (cdaar list)))

(defun cddadr (list)
  "Return the cdr of the cdadr of LIST."
  (cdr (cdadr list)))

(defun cdddar (list)
  "Return the cdr of the cddar of LIST."
  (cdr (cddar list)))

(defun cddddr (list)
  "Return the cdr of the cdddr of LIST."
  (cdr (cdddr list)))


(defsetf caar (x) (v)
  `(progn
     (rplaca (car ,x) ,v)
     ,v))

(defsetf cadr (x) (v)
  `(progn
     (rplaca (cdr ,x) ,v)
     ,v))

(defsetf cdar (x) (v)
  `(progn
     (rplacd (car ,x) ,v)
     ,v))

(defsetf cddr (x) (v)
  `(progn
     (rplacd (cdr ,x) ,v)
     ,v))

(defsetf caaar (x) (v)
  `(progn
     (rplaca (caar ,x) ,v)
     ,v))

(defsetf caadr (x) (v)
  `(progn
     (rplaca (cadr ,x) ,v)
     ,v))

(defsetf cadar (x) (v)
  `(progn
     (rplaca (cdar ,x) ,v)
     ,v))

(defsetf caddr (x) (v)
  `(progn
     (rplaca (cddr ,x) ,v)
     ,v))

(defsetf cdaar (x) (v)
  `(progn
     (rplacd (caar ,x) ,v)
     ,v))

(defsetf cdadr (x) (v)
  `(progn
     (rplacd (cadr ,x) ,v)
     ,v))

(defsetf cddar (x) (v)
  `(progn
     (rplacd (cdar ,x) ,v)
     ,v))

(defsetf cdddr (x) (v)
  `(progn
     (rplacd (cddr ,x) ,v)
     ,v))

(defsetf caaaar (x) (v)
  `(progn
     (rplaca (caaar ,x) ,v)
     ,v))

(defsetf caaadr (x) (v)
  `(progn
     (rplaca (caadr ,x) ,v)
     ,v))

(defsetf caadar (x) (v)
  `(progn
     (rplaca (cadar ,x) ,v)
     ,v))

(defsetf caaddr (x) (v)
  `(progn
     (rplaca (caddr ,x) ,v)
     ,v))

(defsetf cadaar (x) (v)
  `(progn
     (rplaca (cdaar ,x) ,v)
     ,v))

(defsetf cadadr (x) (v)
  `(progn
     (rplaca (cdadr ,x) ,v)
     ,v))

(defsetf caddar (x) (v)
  `(progn
     (rplaca (cddar ,x) ,v)
     ,v))

(defsetf cadddr (x) (v)
  `(progn
     (rplaca (cdddr ,x) ,v)
     ,v))

(defsetf cdaaar (x) (v)
  `(progn
     (rplacd (caaar ,x) ,v)
     ,v))

(defsetf cdaadr (x) (v)
  `(progn
     (rplacd (caadr ,x) ,v)
     ,v))

(defsetf cdadar (x) (v)
  `(progn
     (rplacd (cadar ,x) ,v)
     ,v))

(defsetf cdaddr (x) (v)
  `(progn
     (rplacd (caddr ,x) ,v)
     ,v))

(defsetf cddaar (x) (v)
  `(progn
     (rplacd (cdaar ,x) ,v)
     ,v))

(defsetf cddadr (x) (v)
  `(progn
     (rplacd (cdadr ,x) ,v)
     ,v))

(defsetf cdddar (x) (v)
  `(progn
     (rplacd (cddar ,x) ,v)
     ,v))

(defsetf cddddr (x) (v)
  `(progn
     (rplacd (cdddr ,x) ,v)
     ,v))

(defun null (object)
  "Return t if OBJECT is the empty list; otherwise, return nil."
  (eq object nil))

(defun endp (list)
  "Return true if LIST is the empty list.  Returns false if LIST is a cons."
  (check-type list list)
  (null list))

(defun listp (object)
  "Return true if OBJECT is of type list; otherwise, return false."
  (or (null object) (consp object)))


(defun first (list)
  "Return the 1st element in LIST or NIL if LIST is empty."
  (car list))
(defun second (list)
  "Return the 2nd element in LIST or NIL if there is no 2nd element."
  (cadr list))
(defun third (list)
  "Returns the 3rd element in LIST or NIL if there is no 3rd element."
  (caddr list))
(defun fourth (list)
  "Return the 4th element in LIST or NIL if there is no 4th element."
  (cadddr list))
(defun fifth (list)
  "Return the 5th element in LIST or NIL if there is no 5th element."
  (car (cddddr list)))
(defun sixth (list)
  "Return the 6th element in LIST or NIL if there is no 6th element."
  (cadr (cddddr list)))
(defun seventh (list)
  "Return the 7th element in LIST or NIL if there is no 7th element."
  (caddr (cddddr list)))
(defun eighth (list)
  "Return the 8th element in LIST or NIL if there is no 8th element."
  (cadddr (cddddr list)))
(defun ninth (list)
  "Return the 9th element in LIST or NIL if there is no 9th element."
  (car (cddddr (cddddr list))))
(defun tenth (list)
  "Return the 10th element in LIST or NIL if there is no 10th element."
  (cadr (cddddr (cddddr list))))

(defun rest (list)
  "Perform the same operation as cdr."
  (cdr list))


(defsetf first (x) (v)
  `(setf (car ,x) ,v))

(defsetf second (x) (v)
  `(setf (cadr ,x) ,v))

(defsetf third (x) (v)
  `(setf (caddr ,x) ,v))

(defsetf fourth (x) (v)
  `(setf (cadddr ,x) ,v))

(defsetf fifth (x) (v)
  `(setf (car (cddddr ,x)) ,v))

(defsetf sixth (x) (v)
  `(setf (cadr (cddddr ,x)) ,v))

(defsetf seventh (x) (v)
  `(setf (caddr (cddddr ,x)) ,v))

(defsetf eighth (x) (v)
  `(setf (cadddr (cddddr ,x)) ,v))

(defsetf ninth (x) (v)
  `(setf (car (cddddr (cddddr ,x))) ,v))

(defsetf tenth (x) (v)
  `(setf (cadr (cddddr (cddddr ,x))) ,v))

(defsetf rest (x) (v)
  `(setf (cdr ,x) ,v))


(defun nthcdr (n list)
  "Return the tail of LIST that would be obtained by calling cdr N times."
  (check-type n (integer 0))
  (do ((i n (1- i))
       (result list (cdr result)))
      ((zerop i) result)))

(defun nth (n list)
  "Return the Nth element in LIST, where the car is the zero-th element."
  (car (nthcdr n list)))

(defsetf nth (n list) (v)
  `(setf (car (nthcdr ,n ,list)) ,v))


(defun copy-list (list)
  "Return a copy of LIST which is either a proper list or a dotted list."
  (unless (null list)
    (let ((result (cons (car list) nil)))
      (do ((x (cdr list) (cdr x))
	   (splice result (cdr (rplacd splice (cons (car x) nil)))))
	  ((atom x) (rplacd splice x)))
      result)))

(defun list (&rest args)
  "Return ARGS which is a list of supplied arguments."
  (copy-list args))

(defun list* (arg &rest others)
  "Return a list of the arguments with the last cons being a dotted pair."
  (cond ((null others) arg)
	((null (cdr others)) (cons arg (car others)))
	(t (let ((others (copy-list others)))
	     (do ((x others (cdr x)))
		 ((null (cddr x)) (rplacd x (cadr x))))
	     (cons arg others)))))

(defun list-length (list)
  "Return the length of the given LIST, or nil if the LIST is circular."
  (do ((n 0 (+ n 2))			;Counter.
       (fast list (cddr fast))		;Fast pointer: leaps by 2.
       (slow list (cdr slow)))		;Slow pointer: leaps by 1.
      (nil)
    ;; If fast pointer hits the end, return the count.
    (when (endp fast) (return n))
    (when (endp (cdr fast)) (return (+ n 1)))
    ;; If fast pointer eventually equals slow pointer,
    ;;  then we must be stuck in a circular list.
    ;; (A deeper property is the converse: if we are
    ;;  stuck in a circular list, then eventually the
    ;;  fast pointer will equal the slow pointer.
    ;;  That fact justifies this implementation.)
    (when (and (eq fast slow) (> n 0)) (return nil))))

(defun make-list (size &key initial-element)
  "Return a list of SIZE length, every element of which is INITIAL-ELEMENT."
  (check-type size (integer 0))
  (do ((i size (1- i))
       (list '() (cons initial-element list)))
      ((zerop i) list)))

(defun last (list &optional (n 1))
  "Returns the last N conses (not the last N elements) of LIST."
  (check-type n (integer 0))
  (do ((l list (cdr l))
       (r list)
       (i 0 (1+ i)))
      ((atom l) r)
    (if (>= i n) (pop r))))

(defun butlast (list &optional (n 1))
  "Return a copy of LIST from which the last N conses have been omitted."
  (check-type n (integer 0))
  (if (null list)
      nil
    (let ((length (do ((p (cdr list) (cdr p))
		       (i 1 (1+ i)))
		      ((atom p) i))))
      (do* ((here list (cdr here))
	    (result (list nil))
	    (splice result)
	    (count (- length n) (1- count)))
	  ((<= count 0) (cdr result))
	(setq splice (cdr (rplacd splice (list (car here)))))))))

(defun nbutlast (list &optional (n 1))
  "Modify LIST to remove the last N conses."
  (check-type n (integer 0))
  (if (null list)
      nil
    (let ((length (do ((p (cdr list) (cdr p))
		       (i 1 (1+ i)))
		      ((atom p) i))))
      (unless (<= length n)
	(do ((1st (cdr list) (cdr 1st))
	     (2nd list 1st)
	     (count (- length n 1) (1- count)))
	    ((zerop count) (rplacd 2nd ()) list))))))

(defun nconc (&rest lists)
  "Concatenate LISTS by changing them."
  (setq lists (do ((p lists (cdr p)))
		  ((or (car p) (null p)) p)))
  (do* ((top (car lists))
	(splice top)
	(here (cdr lists) (cdr here)))
      ((null here) top)
    (rplacd (last splice) (car here))
    (when (car here)
      (setq splice (car here)))))

(defun append (&rest lists)
  "Concatenate LISTS by copying them."
  (setq lists (do ((p lists (cdr p)))
		  ((or (car p) (null p)) p)))
  (cond
   ((null lists) '())
   ((null (cdr lists)) (car lists))
   (t (let* ((top  (list (caar lists)))
	     (splice top))
	(do ((x (cdar lists) (cdr x)))
	    ((atom x))
	  (setq splice (cdr (rplacd splice (list (car x))))))
	(do ((p (cdr lists) (cdr p)))
	    ((null (cdr p)) (rplacd splice (car p)) top)
	  (do ((x (car p) (cdr x)))
	      ((atom x))
	    (setq splice (cdr (rplacd splice (list (car x)))))))))))

(defun revappend (list tail)
  "Return (nconc (reverse list) tail)."
  (do ((top list (cdr top))
       (result tail (cons (car top) result)))
      ((endp top) result)))

(defun nreconc (list tail)
  "Return (nconc (nreverse list) tail)."
  (do ((1st (cdr list) (if (atom 1st) 1st (cdr 1st)))
       (2nd list 1st)
       (3rd tail 2nd))
      ((atom 2nd) 3rd)
    (rplacd 2nd 3rd)))

(defun tailp (object list)
  "Return true if OBJECT is the same as some tail of LIST, otherwise false."
  (if (null list)
      (null object)
    (do ((list list (cdr list)))
	((atom (cdr list)) (or (eql object list) (eql object (cdr list))))
      (if (eql object list)
	  (return t)))))

(defun ldiff (list object)
  "Return a copy of LIST before the part which is the same as OBJECT."
  (unless (eql list object)
    (do* ((result (list (car list)))
	  (splice result)
	  (list (cdr list) (cdr list)))
	((atom list) (when (eql list object) (rplacd splice nil)) result)
      (if (eql list object)
	  (return result)
	(setq splice (cdr (rplacd splice (list (car list)))))))))

(defun acons (key datum alist)
  "Construct a new alist by adding the pair (key . datum) to alist."
  (cons (cons key datum) alist))

(defun assoc (item alist &key key (test #'eql) test-not)
  "Return the cons in ALIST whose car is equal (by TEST) to ITEM."
  (when test-not
    (setq test (complement test-not)))
  (dolist (pair alist nil)
    (when (and pair (funcall test item (apply-key key (car pair)))
      (return pair)))))

(defun assoc-if (predicate alist &key key)
  "Return the cons in ALIST whose car satisfies PREDICATE."
  (dolist (pair alist nil)
    (when (and pair (funcall predicate (apply-key key (car pair))))
      (return pair))))

(defun assoc-if-not (predicate alist &key key)
  "Return the cons in ALIST whose car does not satisfy PREDICATE."
  (assoc-if (complement predicate) alist :key key))

(defun rassoc (item alist &key key (test #'eql) test-not)
  "Return the cons in ALIST whose cdr is equal (by TEST) to ITEM."
  (when test-not
    (setq test (complement test-not)))
  (dolist (pair alist nil)
    (when (and pair (funcall test item (apply-key key (cdr pair))))
      (return pair))))

(defun rassoc-if (predicate alist &key key)
  "Return the cons in ALIST whose cdr satisfies PREDICATE."
  (dolist (pair alist nil)
    (when (and pair (funcall predicate (apply-key key (cdr pair))))
      (return pair))))

(defun rassoc-if-not (predicate alist &key key)
  "Return the cons in ALIST whose cdr does not satisfy PREDICATE."
  (rassoc-if (complement predicate) alist :key key))

(defun copy-alist (alist)
  "Return a copy of ALIST."
  (if (null alist)
      nil
    (let* ((new-alist (list (cons (caar alist) (cdar alist))))
	   (tail new-alist))
      (dolist (pair (cdr alist))
	(rplacd tail (list (cons (car pair) (cdr pair))))
	(setq tail (cdr tail)))
      new-alist)))

(defun pairlis (keys data &optional (alist '()))
  "Construct an association list from keys and data (adding to alist)"
  (do ((x keys (cdr x))
       (y data (cdr y)))
      ((endp x) alist)
    (setq alist (acons (car x) (car y) alist))))


(defun sublis (alist tree &key key (test #'eql) test-not)
  "Substitute data of ALIST for subtrees matching keys of ALIST."
  (when test-not
    (setq test (complement test-not)))
  (labels ((sub (subtree)
		(let ((assoc (assoc (apply-key key subtree) alist :test test)))
		  (cond
		   (assoc (cdr assoc))
		   ((atom subtree) subtree)
		   (t (let ((car (sub (car subtree)))
			    (cdr (sub (cdr subtree))))
			(if (and (eq car (car subtree)) (eq cdr (cdr subtree)))
			    subtree
			  (cons car cdr))))))))
    (sub tree)))

(defun nsublis (alist tree &key key (test #'eql) test-not)
  "Substitute data of ALIST for subtrees matching keys of ALIST destructively."
  (when test-not
    (setq test (complement test-not)))
  (labels ((sub (subtree)
		(let ((assoc (assoc (apply-key key subtree) alist :test test)))
		  (cond
		   (assoc (cdr assoc))
		   ((atom subtree) subtree)
		   (t
		    (rplaca subtree (sub (car subtree)))
		    (rplacd subtree (sub (cdr subtree)))
		    subtree)))))
    (sub tree)))

(defun copy-tree (tree)
  "Create a copy of TREE (a structure of conses)."
  (if (consp tree)
      (cons (copy-tree (car tree)) (copy-tree (cdr tree)))
    tree))

(defun subst (new old tree &key key (test #'eql) test-not)
  "Substitute NEW for subtrees matching OLD."
  (when test-not
    (setq test (complement test-not)))
  (labels ((sub (subtree)
		(cond
		 ((funcall test old (apply-key key subtree)) new)
		 ((atom subtree) subtree)
		 (t (let ((car (sub (car subtree)))
			  (cdr (sub (cdr subtree))))
		      (if (and (eq car (car subtree)) (eq cdr (cdr subtree)))
			  subtree
			(cons car cdr)))))))
    (sub tree)))

(defun nsubst (new old tree &key key (test #'eql) test-not)
  "Substitute NEW for subtrees matching OLD destructively."
  (when test-not
    (setq test (complement test-not)))
  (labels ((sub (subtree)
		(cond
		 ((funcall test old (apply-key key subtree)) new)
		 ((atom subtree) subtree)
		 (t (rplaca subtree (sub (car subtree)))
		    (rplacd subtree (sub (cdr subtree)))
		    subtree))))
    (sub tree)))

(defun subst-if (new predicate tree &key key)
  "Substitute NEW for subtrees for which PREDICATE is true."
  (labels ((sub (subtree)
		(cond
		 ((funcall predicate (apply-key key subtree)) new)
		 ((atom subtree) subtree)
		 (t (let ((car (sub (car subtree)))
			  (cdr (sub (cdr subtree))))
		      (if (and (eq car (car subtree)) (eq cdr (cdr subtree)))
			  subtree
			(cons car cdr)))))))
    (sub tree)))

(defun subst-if-not (new predicate tree &key key)
  "Substitute NEW for subtrees for which PREDICATE is false."
  (subst-if new (complement predicate) tree :key key))

(defun nsubst-if (new predicate tree &key key)
  "Substitute NEW for subtrees for which PREDICATE is true destructively."
  (labels ((sub (subtree)
		(cond
		 ((funcall predicate (apply-key key subtree)) new)
		 ((atom subtree) subtree)
		 (t (rplaca subtree (sub (car subtree)))
		    (rplacd subtree (sub (cdr subtree)))
		    subtree))))
    (sub tree)))

(defun nsubst-if-not (new predicate tree &key key)
  "Substitute NEW for subtrees for which PREDICATE is false destructively."
  (nsubst-if new (complement predicate) tree :key key))

(defun tree-equal (a b &key (test #'eql) test-not)
  "Test whether two trees are of the same shape and have the same leaves."
  (when test-not
    (setq test (complement test-not)))
  (labels ((teq (a b)
		(if (atom a)
		    (and (atom b) (funcall test a b))
		  (and (consp b)
		       (teq (car a) (car b))
		       (teq (cdr a) (cdr b))))))
    (teq a b)))



(defmacro push (item place &environment env)
  "Prepend item to the list in PLACE, store the list in PLACE, and returns it."
  (if (symbolp place)
      `(setq ,place (cons ,item ,place))
    (multiple-value-bind (temporary-vars values stores setter getter)
	(get-setf-expansion place env)
      (let ((item-var (gensym)))
	`(let* ((,item-var ,item)
		,@(mapcar #'list temporary-vars values)
		(,(car stores) (cons ,item-var ,getter)))
	   ,setter)))))

(defmacro pop (place &environment env)
  "Return the car of the list in PLACE, storing the cdr of it back into PLACE."
  (if (symbolp place)
      `(prog1 (car ,place) (setq ,place (cdr ,place)))
    (multiple-value-bind (temporary-vars values stores setter getter)
	(get-setf-expansion place env)
      (let ((list-var (gensym)))
	`(let* (,@(mapcar #'list temporary-vars values)
		(,list-var ,getter)
		(,(car stores) (cdr ,list-var)))
	   ,setter
	   (car ,list-var))))))



(defun member (item list &key key (test #'eql) test-not)
  "Return the tail of LIST beginning with an element equal to ITEM."
  (when test-not
    (setq test (complement test-not)))
  (do ((here list (cdr here)))
      ((or (null here) (funcall test item (apply-key key (car here)))) here)))

(defun member-if (predicate list &key key)
  "Return the tail of LIST beginning with an element satisfying PREDICATE."
  (do ((here list (cdr here)))
      ((or (endp here) (funcall predicate (apply-key key (car here)))) here)))

(defun member-if-not (predicate list &key key)
  "Return the tail of LIST beginning with an element not satisfying PREDICATE."
  (member-if (complement predicate) list :key key))

(defun adjoin (item list &key key (test #'eql) test-not)
  "Add ITEM to LIST unless it is already a member."
  (when test-not
    (setq test (complement test-not)))
  (if (member (apply-key key item) list :key key :test test)
      list
    (cons item list)))

(defun intersection (list-1 list-2 &key key (test #'eql) test-not)
  "Return the intersection of LIST-1 and LIST-2."
  (when test-not
    (setq test (complement test-not)))
  (let (result)
    (dolist (element list-1)
      (when (member (apply-key key element) list-2 :key key :test test)
	(push element result)))
    result))

(defun nintersection (list-1 list-2 &key key (test #'eql) test-not)
  "Return the intersection of LIST-1 and LIST-2 destructively modifying LIST-1."
  (when test-not
    (setq test (complement test-not)))
  (let* ((result (list nil))
	 (splice result))
    (do ((list list-1 (cdr list)))
	((endp list) (rplacd splice nil) (cdr result))
      (when (member (apply-key key (car list)) list-2 :key key :test test)
	(setq splice (cdr (rplacd splice list)))))))

(defun union (list-1 list-2 &key key (test #'eql) test-not)
  "Return the union of LIST-1 and LIST-2."
  (when test-not
    (setq test (complement test-not)))
  (let ((result list-2))
    (dolist (element list-1)
      (unless (member (apply-key key element) list-2 :key key :test test)
	(push element result)))
    result))
  
(defun nunion (list-1 list-2 &key key (test #'eql) test-not)
  "Return the union of LIST-1 and LIST-2 destructively modifying them."
  (when test-not
    (setq test (complement test-not)))
  (do* ((result list-2)
	(list-1 list-1)
	tmp)
      ((endp list-1) result)
    (if (member (apply-key key (car list-1)) list-2 :key key :test test)
	(setq list-1 (cdr list-1))
      (setq tmp (cdr list-1)
	    result (rplacd list-1 result)
	    list-1 tmp))))

(defun subsetp (list-1 list-2 &key key (test #'eql) test-not)
  "Return T if every element in LIST-1 is also in LIST-2."
  (when test-not
    (setq test (complement test-not)))
  (dolist (element list-1 t)
    (unless (member (apply-key key element) list-2 :key key :test test)
      (return nil))))

(defun set-difference (list-1 list-2 &key key (test #'eql) test-not)
  "Return the elements of LIST-1 which are not in LIST-2."
  (when test-not
    (setq test (complement test-not)))
  (let ((result nil))
    (dolist (element list-1)
      (unless (member (apply-key key element) list-2 :key key :test test)
	(push element result)))
    result))

(defun nset-difference (list-1 list-2 &key key (test #'eql) test-not)
  "Return the elements of LIST-1 which are not in LIST-2, modifying LIST-1."
  (when test-not
    (setq test (complement test-not)))
  (do* ((result nil)
	(list-1 list-1)
	tmp)
      ((endp list-1) result)
    (if (member (apply-key key (car list-1)) list-2 :key key :test test)
	(setq list-1 (cdr list-1))
      (setq tmp (cdr list-1)
	    result (rplacd list-1 result)
	    list-1 tmp))))

(defun set-exclusive-or (list-1 list-2 &key key (test #'eql) test-not)
  "Return a list of elements that appear in exactly one of LIST-1 and LIST-2."
  (when test-not
    (setq test (complement test-not)))
  (let ((result nil))
    (dolist (element list-1)
      (unless (member (apply-key key element) list-2 :key key :test test)
	(push element result)))
    (dolist (element list-2)
      (unless (member (apply-key key element) list-1 :key key :test test)
	(push element result)))
    result))

(defun nset-exclusive-or (list-1 list-2 &key key (test #'eql) test-not)
  "The destructive version of set-exclusive-or."
  (when test-not
    (setq test (complement test-not)))
  (do* ((head-1 (cons nil list-1))
	(head-2 (cons nil list-2))
	(p-1 head-1))
      ((or (endp (cdr p-1)) (endp (cdr head-2)))
       (progn (rplacd (last p-1) (cdr head-2))
	      (cdr head-1)))
    (do ((p-2 head-2 (cdr p-2)))
	((endp (cdr p-2)) (setq p-1 (cdr p-1)))
      (when (funcall test (apply-key key (cadr p-1)) (apply-key key (cadr p-2)))
	(rplacd p-1 (cddr p-1))
	(rplacd p-2 (cddr p-2))
	(return)))))

(defmacro pushnew (item place &rest keys &environment env)
  "Test if ITEM is the same as any element of the list in PLACE. If not, prepend it to the list, then the new list is stored in PLACE."
  (if (symbolp place)
      `(setq ,place (adjoin ,item ,place ,@keys))
    (multiple-value-bind (temporary-vars values stores setter getter)
	(get-setf-expansion place env)
      (let ((item-var (gensym)))
	`(let* ((,item-var ,item)
		,@(mapcar #'list temporary-vars values)
		(,(car stores) (adjoin ,item-var ,getter ,@keys)))
	   ,setter)))))
  

(defun mapc (function list &rest more-lists)
  "Apply FUNCTION to successive elements of lists, return LIST."
  (do* ((lists (cons list more-lists))
	(args (make-list (length lists))))
      ((do ((l lists (cdr l))
	    (a args (cdr a)))
	   ((or (null l) (endp (car l))) l)
	 (rplaca a (caar l))
	 (rplaca l (cdar l)))
       list)
    (apply function args)))

(defun mapcar (function list &rest more-lists)
  "Apply FUNCTION to successive elements of lists, return list of results."
  (do* ((lists (cons list more-lists))
	(len (length lists))
	(args (make-list len) (make-list len))
	(result (list nil))
	(splice result))
       ((do ((l lists (cdr l))
	     (a args (cdr a)))
	    ((or (null l) (endp (car l))) l)
	  (rplaca a (caar l))
	  (rplaca l (cdar l)))
	(cdr result))
    (setq splice (cdr (rplacd splice (list (apply function args)))))))

(defun mapcan (function list &rest more-lists)
  "Apply FUNCTION to successive elements of lists, return nconc of results."
  (apply #'nconc (apply #'mapcar function list more-lists)))

(defun mapl (function list &rest more-lists)
  "Apply FUNCTION to successive sublists of list, return LIST."
  (do* ((lists (cons list more-lists)))
      ((member nil lists) list)
    (apply function lists)
    (do ((l lists (cdr l)))
	((endp l))
      (rplaca l (cdar l)))))

(defun maplist (function list &rest more-lists)
  "Apply FUNCTION to successive sublists of list, return list of results."
  (do* ((lists (cons list more-lists))
	(result (list nil))
	(splice result))
      ((member nil lists) (cdr result))
    (setq splice (cdr (rplacd splice (list (apply function lists)))))    
    (do ((l lists (cdr l)))
	((endp l))
      (rplaca l (cdar l)))))

(defun mapcon (function list &rest more-lists)
  "Apply FUNCTION to successive sublists of lists, return nconc of results."
  (apply #'nconc (apply #'maplist function list more-lists)))



(defun get-properties (plist indicator-list)
  "Look up any of several property list entries all at once."
  (do ((plist plist (cddr plist)))
      ((endp plist) (values nil nil nil))
    (when (member (car plist) indicator-list)
      (return (values (car plist) (cadr plist) plist)))))

(defun getf (plist indicator &optional (default ()))
  "Find a property on PLIST whose property indicator is identical to INDICATOR, and returns its corresponding property value."
  (do ((plist plist (cddr plist)))
      ((endp plist) default)
    (when (eq indicator (car plist))
      (return (cadr plist)))))



(define-setf-expander getf (place indicator
				  &optional (default nil default-supplied)
				  &environment env)
  (multiple-value-bind (temporary-vars values stores setter getter)
      (get-setf-expansion place env)
    (let ((value-var (gensym))
	  (indicator-var (gensym))
	  (place-var (gensym))
	  (tail-var (gensym))
	  (default-var (when default-supplied (gensym))))
      (values
       `(,@temporary-vars ,indicator-var ,@(when default-supplied
					     `(,default-var)))
       `(,@values ,indicator ,@(when default-supplied `(,default)))
       `(,value-var)
       `(let ((,place-var ,getter))
	  (multiple-value-bind (,(gensym) ,(gensym) ,tail-var)
	      (get-properties ,place-var (list ,indicator-var))
	    (if ,tail-var
		(rplaca (cdr ,tail-var) ,value-var)
	      (let ((,(car stores) (cons ,indicator-var
					 (cons ,value-var ,place-var))))
		,setter
		,value-var))
	    ,value-var))
       `(getf ,getter ,indicator-var ,@(when default-supplied
					 `(,default-var)))))))

(defmacro remf (place indicator &environment env)
  "Remove from the property list stored in PLACE a property with a property indicator identical to INDICATOR."
  (multiple-value-bind (temporary-vars values stores setter getter)
      (get-setf-expansion place env)
    (let ((indicator-var (gensym))
	  (plist-var (gensym))
	  (splice-var (gensym)))
      `(do* (,@(mapcar #'list temporary-vars values)
	       (,indicator-var ,indicator)
	       (,plist-var (cons nil ,getter))
	       (,splice-var ,plist-var (cddr ,splice-var))
	       ,(car stores))
	   ((endp ,splice-var) nil)
	 (when (eq ,indicator-var (cadr ,splice-var))
	   (rplacd ,splice-var (cdddr ,splice-var))
	   (setq ,(car stores) (cdr ,plist-var))
	   ,setter
	   (return t))))))

