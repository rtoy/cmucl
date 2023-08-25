;; Copyright (C) 2002-2004, Yuji Minejima <ggb01164@nifty.ne.jp>
;; ALL RIGHTS RESERVED.
;;
;; $Id: string.lisp,v 1.5 2004/02/20 07:23:42 yuji Exp $
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


(defun stringp (object)
  "Return true if OBJECT is of type string; otherwise, return false."
  (and (vectorp object)
       (subtypep (array-element-type object) 'character)))

(defun simple-string-p (object)
  "Return true if OBJECT is of type simple-string; otherwise, returns false."
  (and (stringp object)
       (typep object 'simple-array)))


(defun string (x)
  "Return a string described by X; X can be a string, a symbol, or a character."
  (etypecase x
    (string x)
    (symbol (symbol-name x))
    (character (make-array 1 :element-type 'character :initial-element x))))

(defun string-upcase (string &key (start 0) end)
  "Return a copy of STRING upcasing all lowercase chars between START and END."
  (unless (stringp string)
    (setq string (string string)))
  (unless end
    (setq end (length string)))
  (let ((str (make-array (length string)
			 :element-type 'character
			 :initial-contents string)))
    (do ((i start (1+ i)))
	((>= i end) str)
      (setf (schar str i) (char-upcase (schar str i))))))

(defun string-downcase (string &key (start 0) end)
 "Return a copy of STRING downcasing all uppercase chars between START and END."
  (unless (stringp string)
    (setq string (string string)))
  (unless end
    (setq end (length string)))
  (let ((str (make-array (length string)
			 :element-type 'character
			 :initial-contents string)))
    (do ((i start (1+ i)))
	((>= i end) str)
      (setf (schar str i) (char-downcase (schar str i))))))


(defun string-capitalize (string &key (start 0) end)
  "Return a copy of STRING capitalizing all words between START and END."
  (unless (stringp string)
    (setq string (string string)))
  (unless end
    (setq end (length string)))
  (let ((str (make-array (length string)
			 :element-type 'character
			 :initial-contents string))
	(in-a-word nil))
    (do ((i start (1+ i))
	 c)
	((>= i end) str)
      (setq c (schar str i))
      (if (alphanumericp c)
	  (if in-a-word
	      (setq c (char-downcase c))
	    (setq c (char-upcase c)
		  in-a-word t))
	(when in-a-word
	  (setq in-a-word nil)))
      (setf (schar str i) c))))


(defun nstring-upcase (string &key (start 0) end)
  "Modify STRING to make all lowercase chars between START and END uppercase."
  (unless end
    (setq end (length string)))
  (do ((i start (1+ i)))
      ((>= i end) string)
    (setf (char string i) (char-upcase (char string i)))))


(defun nstring-downcase (string &key (start 0) end)
  "Modify STRING to make all uppercase chars between START and END lowercase."
  (unless end
    (setq end (length string)))
  (do ((i start (1+ i)))
      ((>= i end) string)
    (setf (char string i) (char-downcase (char string i)))))

(defun nstring-capitalize (string &key (start 0) end)
  "Modify STRING capitalizing all words between START and END."
  (unless end
    (setq end (length string)))
  (do ((i start (1+ i))
       (in-a-word nil)
       c)
      ((>= i end) string)
    (setq c (char string i))
    (if (alphanumericp c)
	(if in-a-word
	    (setq c (char-downcase c))
	  (setq c (char-upcase c)
		in-a-word t))
      (when in-a-word
	(setq in-a-word nil)))
    (setf (char string i) c)))


(defun string-left-trim (character-bag string)
  "Return a copy of STRING stripping chars in CHARACTER-BAG off the beginning."
  (unless (stringp string)
    (setq string (string string)))
  (dotimes (i (length string) (make-string 0 :element-type 'character))
    (unless (find (char string i) character-bag)
      (return (subseq string i)))))

(defun string-right-trim (character-bag string)
  "Return a copy of STRING stripping chars in CHARACTER-BAG off the end."
  (unless (stringp string)
    (setq string (string string)))
  (do ((i (1- (length string)) (1- i)))
      ((minusp i) (make-string 0 :element-type 'character))
    (unless (find (char string i) character-bag)
      (return (subseq string 0 (1+ i))))))

(defun string-trim (character-bag string)
  "Return a copy of STRING stripping chars in CHARACTER-BAG off the both ends."
  (string-left-trim character-bag (string-right-trim character-bag string)))



(defun string= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Return true if the specified substrings are the same in terms of char=."
  (unless (stringp string1) (setq string1 (string string1)))
  (unless (stringp string2) (setq string2 (string string2)))
  (unless end1 (setq end1 (length string1)))
  (unless end2 (setq end2 (length string2)))
  (when (= (- end1 start1) (- end2 start2))
    (do ((i1 start1 (1+ i1))
	 (i2 start2 (1+ i2)))
	((= i1 end1) t)
      (unless (char= (char string1 i1) (char string2 i2))
	(return nil)))))

(defun string-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Return true if the specified substrings are the same in terms of char-equal."
  (unless (stringp string1) (setq string1 (string string1)))
  (unless (stringp string2) (setq string2 (string string2)))
  (unless end1 (setq end1 (length string1)))
  (unless end2 (setq end2 (length string2)))
  (when (= (- end1 start1) (- end2 start2))
    (do ((i1 start1 (1+ i1))
	 (i2 start2 (1+ i2)))
	((= i1 end1) t)
      (unless (char-equal (char string1 i1) (char string2 i2))
	(return nil)))))


(defun string/= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Return true if the specified substrings are not the same in terms of char=."
  (unless (stringp string1) (setq string1 (string string1)))
  (unless (stringp string2) (setq string2 (string string2)))
  (unless end1 (setq end1 (length string1)))
  (unless end2 (setq end2 (length string2)))
  (do ((i1 start1 (1+ i1))
       (i2 start2 (1+ i2)))
      ((or (= i1 end1) (= i2 end2)) (if (and (= i1 end1) (= i2 end2)) nil i1))
    (unless (char= (char string1 i1) (char string2 i2))
      (return i1))))

(defun string-not-equal (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Return true if the specified substrs aren't the same in terms of char-equal."
  (unless (stringp string1) (setq string1 (string string1)))
  (unless (stringp string2) (setq string2 (string string2)))
  (unless end1 (setq end1 (length string1)))
  (unless end2 (setq end2 (length string2)))
  (do ((i1 start1 (1+ i1))
       (i2 start2 (1+ i2)))
      ((or (= i1 end1) (= i2 end2)) (if (and (= i1 end1) (= i2 end2)) nil i1))
    (unless (char-equal (char string1 i1) (char string2 i2))
      (return i1))))


(defun string< (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Return true if substring1 is less than substring2 in terms of char<."
  (unless (stringp string1) (setq string1 (string string1)))
  (unless (stringp string2) (setq string2 (string string2)))
  (unless end1 (setq end1 (length string1)))
  (unless end2 (setq end2 (length string2)))
  (do ((i1 start1 (1+ i1))
       (i2 start2 (1+ i2)))
      ((or (= i1 end1) (= i2 end2)) (when (and (= i1 end1) (/= i2 end2)) end1))
    (unless (char= (char string1 i1) (char string2 i2))
      (return (if (char< (char string1 i1) (char string2 i2)) i1 nil)))))

(defun string-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Return true if substring1 is less than substring2 in terms of char-lessp."
  (unless (stringp string1) (setq string1 (string string1)))
  (unless (stringp string2) (setq string2 (string string2)))
  (unless end1 (setq end1 (length string1)))
  (unless end2 (setq end2 (length string2)))
  (do ((i1 start1 (1+ i1))
       (i2 start2 (1+ i2)))
      ((or (= i1 end1) (= i2 end2)) (when (and (= i1 end1) (/= i2 end2)) end1))
    (unless (char-equal (char string1 i1) (char string2 i2))
      (return (if (char-lessp (char string1 i1) (char string2 i2)) i1 nil)))))


(defun string> (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Return true if substring1 is greater than substring2 in terms of char>."
  (unless (stringp string1) (setq string1 (string string1)))
  (unless (stringp string2) (setq string2 (string string2)))
  (unless end1 (setq end1 (length string1)))
  (unless end2 (setq end2 (length string2)))
  (do ((i1 start1 (1+ i1))
       (i2 start2 (1+ i2)))
      ((or (= i1 end1) (= i2 end2)) (when (and (/= i1 end1) (= i2 end2)) i1))
    (unless (char= (char string1 i1) (char string2 i2))
      (return (if (char> (char string1 i1) (char string2 i2)) i1 nil)))))

(defun string-greaterp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Return true if substr1 is greater than substr2 in terms of char-greaterp."
  (unless (stringp string1) (setq string1 (string string1)))
  (unless (stringp string2) (setq string2 (string string2)))
  (unless end1 (setq end1 (length string1)))
  (unless end2 (setq end2 (length string2)))
  (do ((i1 start1 (1+ i1))
       (i2 start2 (1+ i2)))
      ((or (= i1 end1) (= i2 end2)) (when (and (/= i1 end1) (= i2 end2)) i1))
    (unless (char-equal (char string1 i1) (char string2 i2))
      (return (if (char-greaterp (char string1 i1) (char string2 i2))
		  i1
		nil)))))


(defun string<= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Return true if substr1 is less than or equal to subst2 in terms of char<=."
  (unless (stringp string1) (setq string1 (string string1)))
  (unless (stringp string2) (setq string2 (string string2)))
  (unless end1 (setq end1 (length string1)))
  (unless end2 (setq end2 (length string2)))
  (do ((i1 start1 (1+ i1))
       (i2 start2 (1+ i2)))
      ((or (= i1 end1) (= i2 end2)) (when (= i1 end1) end1))
    (unless (char= (char string1 i1) (char string2 i2))
      (return (if (char<= (char string1 i1) (char string2 i2)) i1 nil)))))

(defun string-not-greaterp (string1 string2 &key
				    (start1 0) end1 (start2 0) end2)
  "Return true if substr1 is not greater than subst2 in terms of char-not-greaterp."
  (unless (stringp string1) (setq string1 (string string1)))
  (unless (stringp string2) (setq string2 (string string2)))
  (unless end1 (setq end1 (length string1)))
  (unless end2 (setq end2 (length string2)))
  (do ((i1 start1 (1+ i1))
       (i2 start2 (1+ i2)))
      ((or (= i1 end1) (= i2 end2)) (when (= i1 end1) end1))
    (unless (char-equal (char string1 i1) (char string2 i2))
      (return (if (char-not-greaterp (char string1 i1) (char string2 i2))
		  i1
		nil)))))


(defun string>= (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Return true if substr1 is greater than or equal to subst2 in terms of char>=."
  (unless (stringp string1) (setq string1 (string string1)))
  (unless (stringp string2) (setq string2 (string string2)))
  (unless end1 (setq end1 (length string1)))
  (unless end2 (setq end2 (length string2)))
  (do ((i1 start1 (1+ i1))
       (i2 start2 (1+ i2)))
      ((or (= i1 end1) (= i2 end2)) (unless (and (= i1 end1) (/= i2 end2)) i1))
    (unless (char= (char string1 i1) (char string2 i2))
      (return (if (char>= (char string1 i1) (char string2 i2)) i1 nil)))))


(defun string-not-lessp (string1 string2 &key (start1 0) end1 (start2 0) end2)
  "Return true if substr1 is not less than subst2 in terms of char-not-lessp."
  (unless (stringp string1) (setq string1 (string string1)))
  (unless (stringp string2) (setq string2 (string string2)))
  (unless end1 (setq end1 (length string1)))
  (unless end2 (setq end2 (length string2)))
  (do ((i1 start1 (1+ i1))
       (i2 start2 (1+ i2)))
      ((or (= i1 end1) (= i2 end2)) (unless (and (= i1 end1) (/= i2 end2)) i1))
    (unless (char-equal (char string1 i1) (char string2 i2))
      (return (if (char-not-lessp (char string1 i1) (char string2 i2))
		  i1
		nil)))))



(defun make-string (size &key
			 (initial-element #\Space)
			 (element-type 'character))
  "Return a simple string of SIZE, ELEMENT-TYPE initialized to INITIAL-ELEMENT."
  (make-array size :element-type element-type :initial-element initial-element))
