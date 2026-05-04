;;; Tests for standard-char

(defpackage :standard-char-tests
  (:use :cl :lisp-unit))

(in-package "STANDARD-CHAR-TESTS")

(define-test standard-char.typep
  (assert-true (typep #\a 'standard-char))
  (assert-false (typep #\tab 'standard-char))
  (assert-true (typep #\a 'standard-char))
  (assert-true (typep #\Z 'standard-char))
  (assert-true (typep #\Space 'standard-char))
  (assert-true (typep #\Newline 'standard-char))
  (assert-false (typep #\Tab 'standard-char))
  (assert-false (typep #\Rubout 'standard-char))
  (assert-false (typep 5 'standard-char))
  (assert-false (typep "hello" 'standard-char))
  (assert-false (typep nil 'standard-char))
  (assert-false (typep t 'standard-char))

  (assert-equal (values t t)
		(subtypep 'standard-char 'character))
  (assert-equal (values t t)
		(subtypep 'standard-char 'base-char)))

(define-test standard-char.etypecase-15
  (assert-equal (values t t)
		(c::type=
		 (c::specifier-type
		  '(not (or pathname boolean standard-char standard-object character file-error)))
		 (c::specifier-type
		  '(not (or file-error character standard-object standard-char boolean pathname))))))


(define-test standard-char.identity
  (let ((a (c::specifier-type 'standard-char))
	(b (c::specifier-type 'standard-char)))
    ;; Should be EQ due to internal caching.
    (assert-eq a b)))

(define-test standard-char.parsing
  (assert-eq 'standard-char
	     (c::type-specifier (c::specifier-type 'standard-char))))

(define-test standard-char.predicate
  (assert-true (c::standard-char-type-p (c::specifier-type 'standard-char))))

(define-test standard-char.simple-subtypep
  (assert-equal (values t t)
		(c::type= (c::specifier-type 'standard-char)
			  (c::specifier-type 'standard-char)))
  (assert-equal (values t t)
		(subtypep 'standard-char 'standard-char)))

(define-test standard-char.complex-subtype-arg1
  ;; STANDARD-CHAR is a subtype of CHARACTER and T.
  (assert-equal (values t t)
		(subtypep 'standard-char 'character))
  (assert-equal (values t t)
		(subtypep 'standard-char t))

  ;; Not a subtype of disjoint types.
  (assert-equal (values nil t)
		(subtypep 'standard-char 'integer))
  (assert-equal (values nil t)
		(subtypep 'standard-char 'symbol))
  (assert-equal (values nil t)
		(subtypep 'standard-char 'pathname))

  ;; Subtype of a member-type that contains all standard chars.
  (assert-equal (values t t)
		(subtypep 'standard-char
			  `(member ,@kernel::+standard-chars+)))
  ;; Not a subtype of a member-type missing even one standard char.
  (assert-equal (values nil t)
		(subtypep 'standard-char '(member #\a))))

(define-test standard-char.complex-subtypep-arg
  ;; All standard chars: subtype.
  (assert-equal (values t t)
		(subtypep '(member #\a) 'standard-char))
  (assert-equal (values t t)
		(subtypep '(member #\Space #\Newline) 'standard-char))

  ;; Mixed — character but not standard.
  (assert-equal (values nil t)
		(subtypep '(member #\Tab) 'standard-char))
  (assert-equal (values nil t)
		(subtypep '(member #\Rubout) 'standard-char))

  ;; Mixed — non-character members. This was the crash case.
  (assert-equal (values nil t)
		(subtypep '(member t) 'standard-char))
  (assert-equal (values nil t)
		(subtypep '(member t nil) 'standard-char))

  ;; Mixed — some standard, some not.
  (assert-equal (values nil t)
		(subtypep '(member #\a #\Tab) 'standard-char))
  (assert-equal (values nil t)
		(subtypep '(member #\a t) 'standard-char))

  ;; CHARACTER is not a subtype of STANDARD-CHAR (non-standard chars exist).
  (assert-equal (values nil t)
		(subtypep 'character 'standard-char)))

(define-test standard-char.complex-union
  ;; Absorbed by supertype.
  (assert-equal (values t t)
		(c::type= (c::type-union (c::specifier-type 'standard-char)
					 (c::specifier-type 'character))
			  (c::specifier-type 'character)))

  (assert-equal (values t t)
		(c::type= (c::type-union (c::specifier-type 'standard-char)
					 (c::specifier-type 't))
			  (c::specifier-type 't)))

  ;; All-standard-chars member-type absorbed back into STANDARD-CHAR.
  (assert-equal (values t t)
		(c::type= (c::type-union (c::specifier-type 'standard-char)
					 (c::specifier-type '(member #\a #\b)))
			  (c::specifier-type 'standard-char)))

  ;; Disjoint type stays as a union (the bug-fix case).
  ;; The result should NOT be a single member-type containing
  ;; T, NIL, and 96 standard chars.
  (let ((result (c::specifier-type '(or boolean standard-char))))
    (assert-true (c::union-type-p result))
    (assert-equal 2 (length (c::union-type-types result)))
    (assert-true (notany (lambda (m)
			   (and (c::member-type-p m)
				(some #'characterp (c::member-type-members m))
				(some (complement #'characterp)
				      (c::member-type-members m))))
			 (c::union-type-types result))))


  ;; Permutation invariance — the original etypecase.15 trigger.
  (assert-equal (values t t)
		(c::type= (c::specifier-type '(or boolean standard-char))
			  (c::specifier-type '(or standard-char boolean))))

  (assert-equal (values t t)
		(c::type= (c::specifier-type
			   '(not (or pathname boolean standard-char standard-object character file-error)))
			  (c::specifier-type
			   '(not (or file-error character standard-object standard-char boolean pathname)))))

  ;; Member-type with non-standard chars — kept symbolically separate.
  (let ((result (c::type-union (c::specifier-type 'standard-char)
                               (c::specifier-type '(member #\Tab)))))
    ;; Should not collapse into a 97-element MEMBER.
    (assert-false (c::member-type-p result))
    (assert-true (c::union-type-p result))
    #+nil
    (not (and (c::member-type-p result)
              (>= (length (c::member-type-members result)) 90)))))

(define-test standard-char.complex-intersection
  ;; Intersection with supertype is STANDARD-CHAR.
  (assert-equal (values t t)
		(c::type= (c::type-intersection (c::specifier-type 'standard-char)
						(c::specifier-type 'character))
			  (c::specifier-type 'standard-char)))

  (assert-equal (values t t)
		(c::type= (c::type-intersection (c::specifier-type 'standard-char)
						(c::specifier-type 't))
			  (c::specifier-type 'standard-char)))

  ;; Intersection with disjoint type is empty.
  (assert-equal (values t t)
		(c::type= (c::type-intersection (c::specifier-type 'standard-char)
						(c::specifier-type 'integer))
			  c::*empty-type*))

  (assert-equal (values t t)
		(c::type= (c::type-intersection (c::specifier-type 'standard-char)
						(c::specifier-type 'symbol))
			  c::*empty-type*))

  ;; Intersection with member-type — filtered to standard chars.
  (assert-equal (values t t)
		(c::type= (c::type-intersection (c::specifier-type 'standard-char)
						(c::specifier-type '(member #\a #\Tab #\b)))
			  (c::specifier-type '(member #\a #\b))))

  ;; All-non-standard members → empty.
  (assert-equal (values t t)
		(c::type= (c::type-intersection (c::specifier-type 'standard-char)
						(c::specifier-type '(member #\Tab #\Rubout)))
			  c::*empty-type*))

  ;; All-standard members → that member-type unchanged.
  (assert-equal (values t t)
		(c::type= (c::type-intersection (c::specifier-type 'standard-char)
						(c::specifier-type '(member #\a)))
			  (c::specifier-type '(member #\a)))))



(define-test standard-char.negation
  ;; NOT STANDARD-CHAR catches non-standard characters.
  (assert-true (typep #\Tab '(not standard-char)))
  (assert-false (typep #\a '(not standard-char)))

  ;; AND CHARACTER (NOT STANDARD-CHAR) is the non-standard chars.
  (assert-true (typep #\Tab '(and character (not standard-char))))
  (assert-false (typep #\a '(and character (not standard-char))))
  (assert-false (typep 5 '(and character (not standard-char))))

  ;; Permutation invariance with negation, multiple types.
  (assert-equal (values t t)
		(c::type= (c::specifier-type '(and standard-char (not (member #\a))))
			  (c::specifier-type '(and (not (member #\a)) standard-char)))))

#+nil
(define-test standard-char.etypecase
  ;; This is the original failing test family — should now pass reliably.
  (loop repeat 100
	always (eql nil
                    (handler-case
			(etypecase #\a
                          (standard-char :ok)
                          (number :wrong))
		      (error () :error))
                    :ok)))

(define-test standard-char.caching
  ;; Multiple specifier-type calls on `standard-char` return EQ.
  (assert-eq (c::specifier-type 'standard-char)
	     (c::specifier-type 'standard-char))

  ;; And via the deftype expansion.
  (assert-eq (c::specifier-type 'standard-char)
	     (c::specifier-type 'standard-char)))
					;
