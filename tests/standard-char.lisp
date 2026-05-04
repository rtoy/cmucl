;;; Tests for standard-char

(defpackage :standard-char-tests
  (:use :cl :lisp-unit))

(in-package "STANDARD-CHAR-TESTS")

;; For the following tests, we generally want to use
;; kernel::type-intersection and kernel::type-union directly to make
;; sure we test the intersection and union methods for standard-char.

(define-test standard-char.typep
    (:tag :issues)
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
    (:tag :issues)
  (assert-equal (values t t)
		(c::type=
		 (c::specifier-type
		  '(not (or pathname boolean standard-char standard-object character file-error)))
		 (c::specifier-type
		  '(not (or file-error character standard-object standard-char boolean pathname))))))

(define-test standard-char.identity
    (:tag :issues)
  (let ((a (c::specifier-type 'standard-char))
	(b (c::specifier-type 'standard-char)))
    ;; Should be EQ due to internal caching.
    (assert-eq a b)))

(define-test standard-char.parsing
    (:tag :issues)
  (assert-eq 'standard-char
	     (c::type-specifier (c::specifier-type 'standard-char))))

(define-test standard-char.predicate
    (:tag :issues)
  (assert-true (c::standard-char-type-p (c::specifier-type 'standard-char))))

(define-test standard-char.simple-subtypep
    (:tag :issues)
  (assert-equal (values t t)
		(c::type= (c::specifier-type 'standard-char)
			  (c::specifier-type 'standard-char)))
  (assert-equal (values t t)
		(subtypep 'standard-char 'standard-char)))

(define-test standard-char.complex-subtype-arg1
    (:tag :issues)
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
    (:tag :issues)
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
    (:tag :issues)
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
    (:tag :issues)
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
    (:tag :issues)
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

(define-test standard-char.etypecase
    (:tag :issues)
  (let ((*random-state* (make-random-state)))
    ;; Test etypecase with standard-char works correctly using random
    ;; characters.  To make this repeatable, use a fixed random-state,
    ;; otherwise, it becomes hard to debug
    (dotimes (k 200)
      (let* ((ch (code-char (random char-code-limit)))
	     (expected (if (standard-char-p ch)
			   :is-standard :is-other))
	     (actual (handler-case
			 (etypecase ch
			   (standard-char :is-standard)
			   (character :is-other))
		       (error ()
			 :error))))
	(assert-eql expected actual ch)))))

(define-test standard-char.caching
    (:tag :issues)
  ;; Multiple specifier-type calls on `standard-char` return EQ.
  (assert-eq (c::specifier-type 'standard-char)
	     (c::specifier-type 'standard-char))

  ;; And via the deftype expansion.
  (assert-eq (c::specifier-type 'standard-char)
	     (c::specifier-type 'standard-char)))
					;

(define-test standard-char.intersection-character-both-orderings
    (:tag :issues)
  ;; Standard-char intersect character = standard-char, regardless of argument order.
  (assert-equality #'kernel::type=
    (kernel::specifier-type 'standard-char)
    (kernel::type-intersection (kernel::specifier-type 'standard-char)
                               (kernel::specifier-type 'character)))
  (assert-equality #'kernel::type=
    (kernel::specifier-type 'standard-char)
    (kernel::type-intersection (kernel::specifier-type 'character)
                               (kernel::specifier-type 'standard-char))))

(define-test standard-char.intersection-disjoint-both-orderings
    (:tag :issues)
  (assert-equality #'kernel::type=
    kernel::*empty-type*
    (kernel::type-intersection (kernel::specifier-type 'standard-char)
                               (kernel::specifier-type 'integer)))
  (assert-equality #'kernel::type=
    kernel::*empty-type*
    (kernel::type-intersection (kernel::specifier-type 'integer)
                               (kernel::specifier-type 'standard-char))))

(define-test standard-char.intersection-member-both-orderings
    (:tag :issues)
  ;; Filter member-type to standard chars only.
  (assert-equality #'kernel::type=
    (kernel::specifier-type '(member #\a #\b))
    (kernel::type-intersection (kernel::specifier-type 'standard-char)
                               (kernel::specifier-type '(member #\a #\Tab #\b))))
  (assert-equality #'kernel::type=
    (kernel::specifier-type '(member #\a #\b))
    (kernel::type-intersection (kernel::specifier-type '(member #\a #\Tab #\b))
                               (kernel::specifier-type 'standard-char))))

(define-test standard-char.union-character-both-orderings
    (:tag :issues)
  ;; Standard-char union character = character.
  (assert-equality #'kernel::type=
    (kernel::specifier-type 'character)
    (kernel::type-union (kernel::specifier-type 'standard-char)
                        (kernel::specifier-type 'character)))
  (assert-equality #'kernel::type=
    (kernel::specifier-type 'character)
    (kernel::type-union (kernel::specifier-type 'character)
                        (kernel::specifier-type 'standard-char))))

(define-test standard-char.union-member-of-standard-both-orderings
    (:tag :issues)
  ;; Standard-char absorbs all-standard member-type.
  (assert-equality #'kernel::type=
    (kernel::specifier-type 'standard-char)
    (kernel::type-union (kernel::specifier-type 'standard-char)
                        (kernel::specifier-type '(member #\a #\b))))
  (assert-equality #'kernel::type=
    (kernel::specifier-type 'standard-char)
    (kernel::type-union (kernel::specifier-type '(member #\a #\b))
                        (kernel::specifier-type 'standard-char))))

(define-test standard-char.union-disjoint-stays-symbolic-both-orderings
    (:tag :issues)
  ;; (or boolean standard-char) and reverse — both should stay symbolic
  ;; rather than collapsing into a giant member-type.
  (let ((r1 (kernel::specifier-type '(or boolean standard-char)))
        (r2 (kernel::specifier-type '(or standard-char boolean))))
    (assert-true (kernel::union-type-p r1))
    (assert-true (kernel::union-type-p r2))
    (assert-equality #'kernel::type= r1 r2)
    ;; Neither should contain a member-type with both characters
    ;; and non-characters.
    (dolist (m (kernel::union-type-types r1))
      (assert-false (and (kernel::member-type-p m)
                         (some #'characterp (kernel::member-type-members m))
                         (some (complement #'characterp)
                               (kernel::member-type-members m)))))))

(define-test standard-char.subtypep-bidirectional
    (:tag :issues)
  ;; arg1 path: standard-char subset of X?
  (assert-equal (values t t) (subtypep 'standard-char 'character))
  (assert-equal (values nil t) (subtypep 'standard-char 'integer))
  ;; arg2 path: X subset of standard-char?
  (assert-equal (values nil t) (subtypep 'character 'standard-char))
  (assert-equal (values nil t) (subtypep 'integer 'standard-char))
  ;; Both reflexively
  (assert-equal (values t t) (subtypep 'standard-char 'standard-char)))

(defun assert-commutative-union (type-a-spec type-b-spec)
  "Assert that union(A, B) and union(B, A) produce type= results."
  (assert-equality #'kernel::type=
    (kernel::type-union (kernel::specifier-type type-a-spec)
                        (kernel::specifier-type type-b-spec))
    (kernel::type-union (kernel::specifier-type type-b-spec)
                        (kernel::specifier-type type-a-spec))))

(defun assert-commutative-intersection (type-a-spec type-b-spec)
  (assert-equality #'kernel::type=
    (kernel::type-intersection (kernel::specifier-type type-a-spec)
                               (kernel::specifier-type type-b-spec))
    (kernel::type-intersection (kernel::specifier-type type-b-spec)
                               (kernel::specifier-type type-a-spec))))

(define-test standard-char.commutativity
    (:tag :issues)
  (assert-commutative-union 'standard-char 'character)
  (assert-commutative-union 'standard-char 'integer)
  (assert-commutative-union 'standard-char '(member #\a #\b))
  (assert-commutative-union 'standard-char '(member #\Tab))
  (assert-commutative-union 'standard-char 'boolean)
  (assert-commutative-union 'standard-char '(not character))
  (assert-commutative-union 'standard-char 't)
  (assert-commutative-intersection 'standard-char 'character)
  (assert-commutative-intersection 'standard-char 'integer)
  (assert-commutative-intersection 'standard-char '(member #\a #\b))
  (assert-commutative-intersection 'standard-char '(member #\Tab))
  (assert-commutative-intersection 'standard-char 'boolean)
  (assert-commutative-intersection 'standard-char '(not character))
  (assert-commutative-intersection 'standard-char 't))
