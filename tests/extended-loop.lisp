(defpackage :extended-loop-tests
  (:use :cl :lisp-unit))

(in-package "EXTENDED-LOOP-TESTS")

(define-test loop-codepoint
    (:tag :extended-loop)
  (let ((codepoints (mapcar #'(lambda (c)
				(if (characterp c)
				    (char-code c)
				    c))
			    '(#\a #\b #\greek_capital_letter_gamma
			      ;; This is a random code point that
			      ;; requires a surrogate pair in our
			      ;; UTF-16 string represntation.
			      65536
			      #\c))))
    (assert-equal codepoints
		  (loop for c being the codepoints of (lisp::codepoints-string codepoints)
			collect c))))

(define-test loop-glyph-string
    (:tag :extended-loop)
  (let* ((s (string #\Latin_Small_Letter_A_With_Diaeresis_and_macron))
	 (d (lisp::string-to-nfkd s)))
    (assert-equal (list s)
		  (loop for g being the glyphs of s collect g))
    (assert-equal (list d)
		  (loop for g being the glyphs of d collect g))))

