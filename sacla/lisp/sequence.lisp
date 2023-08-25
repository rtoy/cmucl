;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: sequence.lisp,v 1.42 2004/02/20 07:23:42 yuji Exp $
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


(defun length (sequence)
  "Return the number of elements in SEQUENCE."
  (cond ;; can't use etypecase for setf
   ((typep sequence 'list)
    (let ((length (list-length sequence)))
      (or length (error-circular-list sequence))))
   ((typep sequence 'vector)
    (if (array-has-fill-pointer-p sequence)
	(fill-pointer sequence)
      (array-dimension sequence 0)))
   (t
    (error 'type-error :datum sequence :expected-type 'sequence))))

(defun check-sequence-access (sequence index)
  (check-type sequence proper-sequence)
  (check-type index (integer 0))
  (unless (< index (length sequence))
    (error-index-too-large sequence index)))

(defun check-subsequence (sequence start end)
  (check-type sequence proper-sequence)
  (check-type start (integer 0))
  (check-type end (integer 0)))

(defun elt (sequence index)
  "Return the element of SEQUENCE specified by INDEX."
  (check-sequence-access sequence index)
  (if (consp sequence)
      (nth index sequence)
    (aref sequence index)))

(defsetf elt (sequence index) (value)
  "Set the element of SEQUENCE specified by INDEX."
  (let ((seq (gensym))
	(idx (gensym)))
    `(let ((,seq ,sequence)
	   (,idx ,index))
       (check-sequence-access ,seq ,idx)
       (if (consp ,seq)
	   (progn (rplaca (nthcdr ,idx ,seq) ,value) ,value)
	 (setf (aref ,seq ,idx) ,value)))))

(defun reverse (sequence)
  "Return a new sequence containing the same elements but in reverse order."
  (check-type sequence proper-sequence)
  (cond
   ((null sequence) nil)
   ((consp sequence) (do ((x sequence (cdr x))
			  (result nil (cons (car x) result)))
			 ((null x) result)))
   (t (let* ((length (length sequence))
	     (result (make-array length
				 :element-type (array-element-type sequence))))
	(do ((i 0 (1+ i))
	     (j (1- length) (1- j)))
	    ((>= i length) result)
	  (setf (aref result i) (aref sequence j)))))))

(defun nreverse (sequence)
  "Modyfy SEQUENCE so that the elements are in reverse order."
  (check-type sequence proper-sequence)
  (cond
   ((null sequence) nil)
   ((consp sequence) (do ((1st (cdr sequence) (cdr 1st))
			  (2nd sequence 1st)
			  (3rd '() 2nd))
			 ((null 2nd) 3rd)
		       (rplacd 2nd 3rd)))
    (t (let ((length (length sequence)))
	 (do ((i 0 (1+ i))
	      (j (1- length) (1- j)))
	     ((>= i j) sequence)
	   (rotatef (aref sequence i) (aref sequence j)))))))

(defun count-if (predicate sequence &key from-end (start 0) end key)
  "Count the number of elements in SEQUENCE which satisfy PREDICATE."
  (let ((count 0))
    (do-subsequence (element sequence start end from-end count)
       (when (funcall predicate (apply-key key element))
	 (incf count)))))

(defun count-if-not (predicate sequence &key from-end (start 0) end key)
  "Count the number of elements in SEQUENCE which do not satisfy PREDICATE."
  (count-if (complement predicate)
	    sequence :from-end from-end :start start :end end :key key))

(defun count (item sequence
		   &key from-end (start 0) end key (test #'eql) test-not)
  "Count the number of ITEM in SEQUENCE bounded by START and END."
  (when test-not (setq test (complement test-not)))
  (count-if #'(lambda (arg) (funcall test item arg))
	    sequence :from-end from-end :start start :end end :key key))

(defun find-if (predicate sequence &key from-end (start 0) end key)
  "Return the first element in SEQUENCE satisfying PREDICATE."
  (do-subsequence (element sequence start end from-end nil)
     (when (funcall predicate (apply-key key element))
       (return element))))

(defun find-if-not (predicate sequence &key from-end (start 0) end key)
  "Return the first element in SEQUENCE not satisfying PREDICATE."
  (find-if (complement predicate) sequence
	   :from-end from-end :start start :end end :key key))

(defun find (item sequence
		  &key from-end (test #'eql) test-not (start 0) end key)
  "Return the first element in SEQUENCE satisfying TEST or TEST-NOT."
  (when test-not (setq test (complement test-not)))
  (find-if #'(lambda (arg) (funcall test item arg))
	   sequence :from-end from-end :start start :end end :key key))

(defun position-if (predicate sequence &key from-end (start 0) end key)
  "Return the position of an element in SEQUENCE satisfying PREDICATE."
  (unless end (setq end (length sequence)))
  (let ((i (if from-end (1- end) start))
	(step (if from-end -1 1)))
    (do-subsequence (element sequence start end from-end nil)
       (when (funcall predicate (apply-key key element))
	 (return i))
       (incf i step))))

(defun position-if-not (predicate sequence &key from-end (start 0) end key)
  "Return the position of an element in SEQUENCE not satisfying PREDICATE."
  (position-if (complement predicate) sequence
	       :from-end from-end :start start :end end :key key))

(defun position (item sequence
		  &key from-end (test #'eql) test-not (start 0) end key)
  "Return the position of an element in SEQUENCE equal to ITEM by TEST."
  (when test-not (setq test (complement test-not)))
  (position-if #'(lambda (arg) (funcall test item arg))
	       sequence :from-end from-end :start start :end end :key key))

(defun make-iterator (sequence start end length from-end)
  (check-subsequence sequence start end)
  (if (listp sequence)
      (let* ((head (if from-end
		       (nthcdr (- length end) (reverse sequence))
		     (nthcdr start sequence)))
	     (x head))
	(values #'(lambda () (prog1 (car x) (setq x (cdr x))))
		#'(lambda () (setq x head))))
    (let* ((from (if from-end (1- end) start))
	   (i from)
	   (step (if from-end -1 1)))
      (values #'(lambda () (prog1 (aref sequence i) (setq i (+ i step))))
	      #'(lambda () (setq i from))))))

(defun search (sequence1 sequence2 &key from-end (test #'eql) test-not key
			  (start1 0) (start2 0) end1 end2)
  "Return the first position in SEQUENCE2 that matches SEQUENCE1."
  (when test-not (setq test (complement test-not)))
  (let* ((length1 (length sequence1))
	 (end1 (or end1 length1))
	 (end2 (or end2 (length sequence2)))
	 (width1 (- end1 start1))
	 (last-match nil))
    (multiple-value-bind (get1 reset1)
	(make-iterator sequence1 start1 end1 length1 nil)
      (etypecase sequence2
	(null (when (zerop length1) 0))
	(cons (do ((x (nthcdr start2 sequence2) (cdr x))
		   (i start2 (1+ i)))
		  ((> i (- end2 width1)) (when from-end last-match))
		(funcall reset1)
		(do ((xx x (cdr xx))
		     (j 0 (1+ j)))
		    ((>= j width1) (if from-end
				       (setq last-match i)
				     (return-from search i)))
		  (unless (funcall test (apply-key key (funcall get1))
				        (apply-key key (car xx)))
		    (return)))))
	(vector (do ((i start2 (1+ i)))
		    ((> i (- end2 width1)) (when from-end last-match))
		  (funcall reset1)
		  (do ((ii i (1+ ii))
		       (j 0 (1+ j)))
		      ((>= j width1) (if from-end
					 (setq last-match i)
				       (return-from search i)))
		    (unless (funcall test (apply-key key (funcall get1))
				          (apply-key key (aref sequence2 ii)))
		      (return)))))))))

(defun mismatch (sequence1 sequence2 &key from-end (test #'eql) test-not key
			   (start1 0) (start2 0) end1 end2)
  "Return the first position where SEQUENCE1 and SEQUENCE2 differ."
  (when test-not (setq test (complement test-not)))
  (let* ((length1 (length sequence1))
	 (length2 (length sequence2))
	 (end1 (or end1 length1))
	 (end2 (or end2 length2))
	 (width1 (- end1 start1))
	 (width2 (- end2 start2))
	 (width (min width1 width2))
	 (s1 (if from-end (- end1 width) start1))
	 (e1 (if from-end end1 (+ start1 width))))
    (multiple-value-bind (get2 reset2)
	(make-iterator sequence2 start2 end2 length2 from-end)
      (declare (ignore reset2))
      (let ((i1 (if from-end (1- end1) start1))
	    (step (if from-end -1 1)))
	(do-subsequence (element1 sequence1 s1 e1 from-end
                         (cond ((= width1 width2) nil)
			       ((< width1 width2) (if from-end 0 end1))
			       (t (if from-end
				      (- end1 width2)
				    (+ start1 width2)))))
            (unless (funcall test (apply-key key element1)
			     (apply-key key (funcall get2)))
	      (return (if from-end (1+ i1) i1)))
	    (incf i1 step))))))

(defun replace (sequence1 sequence2 &key (start1 0) end1 (start2 0) end2)
  "Modify SEQUENCE1 destructively by replacing elements with those of SUBSEQUENCE2."
  (let* ((length2 (length sequence2))
	 (end1 (or end1 (length sequence1)))
	 (end2 (or end2 length2))
	 (width1 (- end1 start1))
	 (width2 (- end2 start2))
	 (width (min width1 width2))
	 (from-end nil))
    (when (< start2 start1 (+ start2 width))
      (setq sequence2 (copy-seq sequence2)))
    (multiple-value-bind (get2 reset2)
	(make-iterator sequence2 start2 end2 length2 from-end)
      (declare (ignore reset2))
      (do-subsequence (element1 sequence1 start1 (+ start1 width) from-end)
         (setf element1 (funcall get2)))
      sequence1)))


(defun subseq (sequence start &optional end)
  "Return a copy of the subsequence of SEQUENCE bounded by START and END."
  (unless end (setq end (length sequence)))
  (check-subsequence sequence start end)
  (etypecase sequence
    (list (do* ((x (nthcdr start sequence) (cdr x))
		(i start (1+ i))
		(result (list nil))
		(splice result))
	      ((>= i end) (cdr result))
	    (setq splice (cdr (rplacd splice (list (car x)))))))
    (vector (let* ((width (- end start))
		   (result (make-array width
				       :element-type
				       (array-element-type sequence))))
	      (do ((i 0 (1+ i))
		   (j start (1+ j)))
		  ((>= i width) result)
		(setf (aref result i) (aref sequence j)))))))

(defsetf subseq (sequence start &optional (end nil)) (new-subsequence)
  "Replace destructively the subsequence of SEQUENCE with NEW-SUBSEQUENCE."
  `(progn
     (check-type ,new-subsequence sequence)
     (replace ,sequence ,new-subsequence :start1 ,start :end1 ,end)
     ,new-subsequence))

(defun copy-seq (sequence)
  "Create a copy of SEQUENCE."
  (subseq sequence 0))

(defun nsubstitute-if (newitem predicate sequence &key from-end
			       (start 0) end count key)
  "Modify SEQUENCE substituting NEWITEM for elements satisfying PREDICATE."
  (when (or (null count) (plusp count))
    (do-subsequence (element sequence start end from-end)
       (when (funcall predicate (apply-key key element))
	 (setf element newitem)
	 (when (and count (zerop (setq count (1- count))))
	   (return)))))
  sequence)

(defun nsubstitute (newitem olditem sequence &key from-end (test #'eql) test-not
			    (start 0) end count key)
  "Modify SEQUENCE substituting NEWITEM for elements euqal to OLDITEM."
  (when test-not (setq test (complement test-not)))
  (nsubstitute-if newitem #'(lambda (item) (funcall test olditem item))
		  sequence :from-end from-end :start start :end end
		  :count count :key key))

(defun nsubstitute-if-not (newitem predicate sequence &key from-end
				   (start 0) end count key)
  "Modify SEQUENCE substituting NEWITEM for elements not satisfying PREDICATE."
  (nsubstitute-if newitem (complement predicate) sequence :from-end from-end
		  :start start :end end :count count :key key))

(defun substitute (newitem olditem sequence &key from-end (test #'eql) test-not
			   (start 0) end count key)
  "Return a copy of SEQUENCE with elements euqal to OLDITEM replaced with NEWITEM."
  (nsubstitute newitem olditem (copy-seq sequence) :from-end from-end :test test
	       :test-not test-not :start start :end end :count count :key key))
  
(defun substitute-if (newitem predicate sequence &key from-end (start 0) end
			      count key)
  "Return a copy of SEQUENCE with elements satisfying PREDICATE replaced with NEWITEM."
  (nsubstitute-if newitem predicate (copy-seq sequence) :from-end from-end
		  :start start :end end :count count :key key))

(defun substitute-if-not (newitem predicate sequence &key from-end (start 0) end
				  count key)
  "Return a copy of SEQUENCE with elements not satisfying PREDICATE replaced with NEWITEM."
  (nsubstitute-if-not newitem predicate (copy-seq sequence) :from-end from-end
		      :start start :end end :count count :key key))

(defun fill (sequence item &key (start 0) end)
  "Replace the elements of SEQUENCE bounded by START and END with ITEM."
  (nsubstitute-if item (constantly t) sequence :start start :end end))

(defun concatenate (result-type &rest sequences)
  "Return a sequence of RESULT-TYPE that have all the elements of SEQUENCES."
  (cond
   ((subtypep result-type 'list)
    (let* ((list (list nil))
	   (splice list))
      (dolist (seq sequences (cdr list))
	(do-subsequence (element seq 0)
           (setq splice (cdr (rplacd splice (cons element nil))))))))
   ((subtypep result-type 'vector)
    (let ((vector (make-sequence result-type
				 (apply #'+ (mapcar #'length sequences))))
	  (i 0))
      (dolist (seq sequences vector)
	(do-subsequence (element seq 0)
           (setf (aref vector i) element)
	   (incf i)))))
   (t
    (error 'type-error
	   :datum result-type
	   :expected-type '(or null sequence)))))

(defun merge-lists (list1 list2 predicate key)
  (let* ((list (list nil))
	 (splice list))
    (do ((x1 list1)
	 (x2 list2))
	((or (endp x1) (endp x2)) (rplacd splice (or x1 x2)) (cdr list))
      (if (funcall predicate (apply-key key (car x2))
		             (apply-key key (car x1)))
	  (setq splice (cdr (rplacd splice x2))
		x2 (cdr x2))
	(setq splice (cdr (rplacd splice x1))
	      x1 (cdr x1))))))

(defun merge (result-type sequence1 sequence2 predicate &key key)
  "Merge SEQUENCE1 with SEQUENCE2 destructively according to an order determined by the PREDICATE."
  (let ((merged-list (merge-lists (coerce sequence1 'list)
				  (coerce sequence2 'list) predicate key)))
    (cond ((subtypep result-type 'list) merged-list)
	  ((subtypep result-type 'vector) (coerce merged-list result-type))
	  (t (error 'type-error
		    :datum result-type
		    :expected-type '(or null sequence))))))

(defun quicksort-vector (vector predicate key)
  (labels ((quicksort (left right)
              (if (<= right left)
		  vector
		(let ((v (partition left right)))
		  (quicksort left (1- v))
		  (quicksort (1+ v) right))))
	   (partition (left right)
	      (let ((pivot (apply-key key (aref vector right)))
		    (l left)
		    (r (1- right)))
		(loop (loop (unless (funcall predicate
					     (apply-key key (aref vector l))
					     pivot)
			      (return))
			    (incf l))
		      (loop (when (or (>= l r)
				      (funcall predicate
					       (apply-key key (aref vector r))
					       pivot))
			      (return))
			    (decf r))
		      (when (>= l r)
			(return))
		      (rotatef (aref vector l) (aref vector r)))
		(rotatef (aref vector l) (aref vector right))
		l)))
    (quicksort 0 (1- (length vector)))))

(defun sort (sequence predicate &key key)
  "Sort SEQUENCE destructively according to the order determined by PREDICATE."
  (if (vectorp sequence)
      (quicksort-vector sequence predicate key)
    (let ((vector (quicksort-vector (make-array (length sequence)
						 :initial-contents sequence)
				    predicate key)))
      (do ((x sequence (cdr x))
	   (i 0 (1+ i)))
	  ((endp x) sequence)
	(rplaca x (aref vector i))))))

(defun mergesort-list (list predicate key)
  (labels ((mergesort (list length)
	      (if (<= length 1)
		  list
		(let* ((length1 (floor (/ length 2)))
		       (length2 (- length length1))
		       (list1 list)
		       (last1 (nthcdr (1- length1) list))
		       (list2 (cdr last1)))
		  (rplacd last1 nil)
		  (merge 'list
			 (mergesort list1 length1) (mergesort list2 length2)
			 predicate :key key)))))
    (mergesort list (length list))))

(defun stable-sort (sequence predicate &key key)
  "Sort SEQUENCE destructively guaranteeing the stability of equal elements' order."
  (if (listp sequence)
      (mergesort-list sequence predicate key)
    (let ((list (mergesort-list (coerce sequence 'list) predicate key)))
      (do ((x list (cdr x))
	   (i 0 (1+ i)))
	  ((endp x) sequence)
	(setf (aref sequence i) (car x))))))

(defun list-delete-if (test list start end count key)
  (let* ((head (cons nil list))
	 (splice head))
    (do ((i 0 (1+ i))
	 (x list (cdr x)))
	((endp x) (rplacd splice nil) (cdr head))
      (when (and count (<= count 0))
	(rplacd splice x)
	(return (cdr head)))
      (if (and (<= start i) (or (null end) (< i end))
	       (funcall test (apply-key key (car x))))
	  (when count (decf count))
	(setq splice (cdr (rplacd splice x)))))))

(defun vector-delete-if (test vector start end count key)
  (let* ((length (length vector))
	 (end (or end length))
	 (count (or count length))
	 (i 0))
    (do* ((j 0 (1+ j))
	  element)
         ((>= j length))
      (setq element (aref vector j))
      (if (and (<= start j) (< j end)
	       (plusp count)
	       (funcall test (apply-key key element)))
	  (when count (decf count))
          (progn
            (setf (aref vector i) element)
            (incf i))))
    (cond
      ((array-has-fill-pointer-p vector)
       (setf (fill-pointer vector) i)
       vector)
      ((adjustable-array-p vector) (adjust-array vector i))
      (t (subseq vector 0 i))))) 

(defun delete-if (predicate sequence &key from-end (start 0) end count key)
  "Modify SEQUENCE by deleting elements satisfying PREDICATE."
  (if from-end
      (let ((length (length sequence)))
	(nreverse (delete-if predicate (nreverse sequence)
			     :start (- length (or end length))
			     :end (- length start)
			     :count count :key key)))
    (etypecase sequence
      (null nil)
      (cons (list-delete-if predicate sequence start end count key))
      (vector (vector-delete-if predicate sequence start end count key)))))

(defun delete (item sequence &key from-end (test #'eql) test-not (start 0) end
		    count key)
  "Modify SEQUENCE by deleting elements equal to ITEM."
  (when test-not (setq test (complement test-not)))
  (delete-if #'(lambda (arg) (funcall test item arg)) sequence
	     :from-end from-end :start start :end end :count count :key key))

(defun delete-if-not (predicate sequence &key from-end (start 0) end count key)
  "Modify SEQUENCE by deleting elements not satisfying PREDICATE."
  (delete-if (complement predicate) sequence :from-end from-end
	     :start start :end end :count count :key key))

(defun remove-if (predicate sequence &key from-end (start 0) end count key)
  "Return a copy of SEQUENCE with elements satisfying PREDICATE removed."
  (delete-if predicate (copy-seq sequence) :from-end from-end :start start :end end
	     :count count :key key))

(defun remove (item sequence &key from-end (test #'eql) test-not (start 0)
		    end count key)
  "Return a copy of SEQUENCE with elements equal to ITEM removed."
  (when test-not (setq test (complement test-not)))
  (remove-if #'(lambda (arg) (funcall test item arg)) sequence
	     :from-end from-end :start start :end end :count count :key key))

(defun remove-if-not (predicate sequence &key from-end (start 0) end count key)
  "Return a copy of SEQUENCE with elements not satisfying PREDICATE removed."
  (remove-if (complement predicate) sequence :from-end from-end
	     :start start :end end :count count :key key))


(defun list-delete-duplicates (test list start end key)
  (check-type list proper-list)
  (let* ((head (cons nil list))
	 (splice head)
	 (tail (when end (nthcdr end list))))
    (flet ((list-member (list)
	      (do ((x (cdr list) (cdr x))
		   (item (car list)))
		  ((eq x tail) nil)
		(when (funcall test (apply-key key item) (apply-key key (car x)))
		  (return t)))))
      (do ((i 0 (1+ i))
	   (x list (cdr x)))
	  ((endp x) (rplacd splice nil) (cdr head))
	(unless (and (<= start i) (or (null end) (< i end)) (list-member x))
	  (setq splice (cdr (rplacd splice x))))))))

(defun vector-delete-duplicates (test vector start end key)
  (let* ((length (length vector))
	 (end (or end length))
	 (i 0))
    (flet ((vector-member (item j)
             (do ((k (1+ j) (1+ k)))
                 ((>= k end) nil)
               (when (funcall test (apply-key key item)
                              (apply-key key (aref vector k)))
                 (return t)))))
      (do* ((j 0 (1+ j))
	    element)
           ((>= j length))
	(setq element (aref vector j))
	(unless (and (<= start j) (< j end) (vector-member element j))
	  (setf (aref vector i) element)
	  (incf i)))
      (cond
        ((array-has-fill-pointer-p vector)
         (setf (fill-pointer vector) i)
         vector)
        ((adjustable-array-p vector) (adjust-array vector i))
        (t (subseq vector 0 i))))))

(defun delete-duplicates (sequence &key from-end (test #'eql) test-not
				   (start 0) end key)
  "Modify SEQUENCE deleting redundant elements."
  (when test-not (setq test (complement test-not)))
  (if from-end
      (let ((length (length sequence)))
	(nreverse (delete-duplicates (nreverse sequence) :test test :key key
				     :start (- length (or end length))
				     :end (- length start))))
    (etypecase sequence
      (null nil)
      (cons (list-delete-duplicates     test sequence start end key))
      (vector (vector-delete-duplicates test sequence start end key)))))

(defun remove-duplicates (sequence &key from-end (test #'eql) test-not
				   (start 0) end key)
  "Return a copy of SEQUENCE with redundant elements removed."
  (delete-duplicates (copy-seq sequence) :from-end from-end :key key
		     :test test :test-not test-not :start start :end end))

(defun reduce (function sequence &key key from-end (start 0) end
			(initial-value nil initial-value-supplied))
  "Use a binary operation FUNCTION to combine the elements of SEQUENCE."
  (unless end (setq end (length sequence)))
  (check-subsequence sequence start end)
  (if (= start end)
      (if initial-value-supplied initial-value (funcall function))
    (let ((fun (if from-end #'(lambda (a b) (funcall function b a)) function))
	  (value (if initial-value-supplied
		     initial-value
		   (apply-key key (if from-end
				      (elt sequence (decf end))
				    (prog1 (elt sequence start)
				      (incf start)))))))
      (do-subsequence (element sequence start end from-end value)
         (setq value (funcall fun value (apply-key key element)))))))

(defmacro do-sequences ((var sequences &optional (result nil)) &body body)
  (let ((seq-list (gensym))
	(i        (gensym))
	(min      (gensym)))
    `(let* ((,seq-list (copy-seq ,sequences))
	    (,var (make-list (list-length ,seq-list) :initial-element nil))
	    (,min (if ,seq-list (reduce #'min ,seq-list :key #'length) 0)))
       (dotimes (,i ,min ,result)
	 (do* ((src ,seq-list (cdr src))
	       (seq (car src) (car src))
	       (dest ,var (cdr dest)))
	     ((null src))
	   (rplaca dest (if (consp seq)
			    (progn
			      (rplaca src (cdr seq))
			      (car seq))
			  (aref seq ,i))))
	 ,@body))))

(defun map-into (result-sequence function &rest sequences)
  "Modify RESULT-SEQUENCE, applying FUNCTION to the elements of SEQUENCES."
  (etypecase result-sequence
    (null nil)
    (cons (let ((x result-sequence))
	    (do-sequences (args sequences result-sequence)
               (when (endp x) (return result-sequence))
	       (rplaca x (apply function args))
	       (setq x (cdr x)))))
    (vector (let ((i 0)
		  (length (array-dimension result-sequence 0)))
	      (do-sequences (args sequences)
		 (when (= i length) (return))
		 (setf (aref result-sequence i) (apply function args))
		 (setq i (1+ i)))
	      (when (array-has-fill-pointer-p result-sequence)
		(setf (fill-pointer result-sequence) i))
	      result-sequence))))

(defun map (result-type function sequence &rest more-sequences)
  "Apply FUNCTION to the successive elements of SEQUENCE and MORE-SEQUENCES."
  (if (null result-type)
      (do-sequences (args (cons sequence more-sequences) nil)
         (apply function args))
    (let* ((sequences (cons sequence more-sequences))
	   (seq (make-sequence result-type
			       (reduce #'min sequences :key #'length))))
      (apply #'map-into seq function sequences))))


(defun every (predicate sequence &rest more-sequences)
  "Return true if and only if every invocation of PREDICATE on SEQUENCE returns true."
  (do-sequences (args (cons sequence more-sequences) t)
     (unless (apply predicate args)
       (return nil))))

(defun some (predicate sequence &rest more-sequences)
  "Return true if and only if some invocation of PREDICATE on SEQUENCE returns true."
  (do-sequences (args (cons sequence more-sequences) nil)
     (when (apply predicate args)
       (return t))))

(defun notevery (predicate sequence &rest more-sequences)
  "Return true if and only if some invocation of PREDICATE on SEQUENCE returns false."
  (not (apply #'every predicate sequence more-sequences)))

(defun notany (predicate sequence &rest more-sequences)
  "Return true if and only if every invocation of PREDICATE on SEQUENCE returns false."
  (not (apply #'some predicate sequence more-sequences)))
