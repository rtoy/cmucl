;;; -*- Log: Code.Log; Package: Lisp -*-
;;;
;;; **********************************************************************
;;; This code was written as part of the Spice Lisp project at
;;; Carnegie-Mellon University, and has been placed in the public domain.
;;; If you want to use this code or any part of Spice Lisp, please contact
;;; Scott Fahlman (FAHLMAN@CMUC). 
;;; **********************************************************************
;;;
;;;    String hacking functions that are stubs for things that might
;;; be microcoded someday.
;;;
;;;    Written by Rob MacLachlan and Skef Wholey
;;;
(in-package "SYSTEM")
(export '(%sp-reverse-find-character-with-attribute))

(in-package "LISP")

;(defun %sp-byte-blt (src-string src-start dst-string dst-start dst-end)
;  "Moves bytes from Src-String into Dst-String between Dst-Start (inclusive)
;and Dst-End (exclusive) (Dst-Start - Dst-End bytes are moved).  Overlap of the
;strings does not affect the result.  This would be done on the Vax
;with MOVC3. The arguments do not need to be strings: 8-bit U-Vectors
;are also acceptable."
;  (%primitive byte-blt src-string src-start dst-string dst-start dst-end))

(defun %sp-string-compare (string1 start1 end1 string2 start2 end2)
  (declare (simple-string string1 string2))
  (declare (fixnum start1 end1 start2 end2))
  "Compares the substrings specified by String1 and String2 and returns
NIL if the strings are String=, or the lowest index of String1 in
which the two differ. If one string is longer than the other and the
shorter is a prefix of the longer, the length of the shorter + start1 is
returned. This would be done on the Vax with CMPC3. The arguments must
be simple strings."
  (let ((len1 (- end1 start1))
	(len2 (- end2 start2)))
    (declare (fixnum len1 len2))
    (cond
     ((= len1 len2)
      (do ((index1 start1 (1+ index1))
	   (index2 start2 (1+ index2)))
	  ((= index1 end1) nil)
	(declare (fixnum index1 index2))
	(if (char/= (elt string1 index1) (elt string2 index2))
	    (return index1))))
     ((> len1 len2)
      (do ((index1 start1 (1+ index1))
	   (index2 start2 (1+ index2)))
	  ((= index2 end2) index1)
	(declare (fixnum index1 index2))
	(if (char/= (elt string1 index1) (elt string2 index2))
	    (return index1))))
     (t
      (do ((index1 start1 (1+ index1))
	   (index2 start2 (1+ index2)))
	  ((= index1 end1) index1)
	(declare (fixnum index1 index2))
	(if (char/= (elt string1 index1) (elt string2 index2))
	    (return index1)))))))

(defun %sp-reverse-string-compare (string1 start1 end1 string2 start2 end2)
  (declare (simple-string string1 string2))
  (declare (fixnum start1 end1 start2 end2))
  "Like %sp-string-compare, only backwards."
  (let ((len1 (- end1 start1))
	(len2 (- end2 start2)))
    (declare (fixnum len1 len2))
    (cond
     ((= len1 len2)
      (do ((index1 (1- end1) (1- index1))
	   (index2 (1- end2) (1- index2)))
	  ((< index1 start1) nil)
	(declare (fixnum index1 index2))
	(if (char/= (elt string1 index1) (elt string2 index2))
	    (return index1))))
     ((> len1 len2)
      (do ((index1 (1- end1) (1- index1))
	   (index2 (1- end2) (1- index2)))
	  ((< index2 start2) index1)
	(declare (fixnum index1 index2))
	(if (char/= (elt string1 index1) (elt string2 index2))
	    (return index1))))
     (t
      (do ((index1 (1- end1) (1- index1))
	   (index2 (1- end2) (1- index2)))
	  ((< index1 start1) index1)
	(declare (fixnum index1 index2))
	(if (char/= (elt string1 index1) (elt string2 index2))
	    (return index1)))))))

;(defun %sp-find-character-with-attribute (string start end table mask)
;  (declare (type (simple-array (mod 256) char-code-max) table))
;  (declare (simple-string string))
;  (declare (fixnum start end))
;  "%SP-Find-Character-With-Attribute  String, Start, End, Table, Mask
;  The codes of the characters of String from Start to End are used as indices
;  into the Table, which is a U-Vector of 8-bit bytes. When the number picked
;  up from the table bitwise ANDed with Mask is non-zero, the current
;  index into the String is returned. The corresponds to SCANC on the Vax."
;  (%primitive find-character-with-attribute string start end table mask))

;;;; ### Warning ###  ### Warning ###  ### Warning ###  ### Warning ###
;;;
;;;    This will lose big if a GC can happen during its execution, since
;;; a pointer to after the header of the table is maintained.  Currently
;;; GC's happen only immediately after allocation, so we're safe.
;;;
(defun %sp-reverse-find-character-with-attribute (string start end table mask)
  "Like %SP-Find-Character-With-Attribute, only sdrawkcaB."
  (declare (fixnum start end mask))
  (incf start 8)
  (do ((table (%primitive sap+ table 8))
       (index (+ end 7) (1- index)))
      ((< index start) nil)
    (declare (fixnum index))
    (unless (eql (logand (the fixnum
			      (%primitive 8bit-system-ref table
					  (%primitive 8bit-system-ref string index)))
			 mask)
		 0)
      (return (- index 8)))))

;(defun %sp-find-character (string start end character)
;  "%SP-Find-Character  String, Start, End, Character
;  Searches String for the Character from Start to End.  If the character is
;  found, the corresponding index into String is returned, otherwise NIL is
;  returned."
;  (%primitive find-character string start end character))

(defun %sp-skip-character (string start end character)
  (declare (simple-string string))
  (declare (fixnum start end))
  "%SP-Skip-Character  String, Start, End, Character
  Returns the index of the first character between Start and End which
  is not Char=  to Character, or NIL if there is no such character."
  (do ((index start (1+ index)))
      ((= index end) nil)
    (declare (fixnum index))
    (if (char/= (char string index) character)
	(return index))))

(defun %sp-reverse-find-character (string start end character)
  (declare (simple-string string))
  (declare (fixnum start end))
  "%SP-Reverse-Find-Character  String, Start, End, Character
  Searches String for Character from End to Start.  If the character is
  found, the corresponding index into String is returned, otherwise NIL is
  returned."
  (do ((index (1- end) (1- index))
       (terminus (1- start)))
      ((= index terminus) nil)
    (declare (fixnum terminus index))
    (if (char= (char string index) character)
	(return index))))

(defun %sp-reverse-skip-character (string start end character)
  (declare (simple-string string))
  (declare (fixnum start end))
  "%SP-Skip-Character  String, Start, End, Character
  Returns the index of the last character between Start and End which
  is not Char=  to Character, or NIL if there is no such character."
  (do ((index (1- end) (1- index))
       (terminus (1- start)))
      ((= index terminus) nil)
    (declare (fixnum terminus index))
    (if (char/= (char string index) character)
	(return index))))

(defun %sp-string-search (string1 start1 end1 string2 start2 end2)
  "%SP-String-Search  String1, Start1, End1, String2, Start2, End2
		Searches for the substring of String1 specified in String2.
		Returns an index into String2 or NIL if the substring wasn't
		found."
  (do ((index2 start2 (1+ index2)))
      ((= index2 end2) nil)
    (if (do ((index1 start1 (1+ index1))
	     (index2 index2 (1+ index2)))
	    ((= index1 end1) t)
	  (if (char/= (char string1 index1) (char string2 index2))
	      (return nil)))
	(return index2))))


(defun %sp-char-upcase (character)
  "%SP-Char-Upcase  Character
  Returns a character with the same font and bits, but with an upper case
  code if it was alphabetic."
  (char-upcase character))

(defun %sp-char-downcase (character)
  "%SP-Char-Downcase  Character
  Returns a character with the same font and bits, but with a lower
  case character code if it was alphabetic."
  (char-downcase character))

(defun %sp-string-upcase (string start end)
  "Creates a new string with the chars from start to end uppercased."
  (string-upcase string :start start :end end))

(defun %sp-string-downcase (string start end)
  "Creates a new string with the chars from start to end lowercased."
  (string-downcase string :start start :end end))

;(defun %sp-sxhash-simple-string (string)
;  (%primitive sxhash-simple-string string))
