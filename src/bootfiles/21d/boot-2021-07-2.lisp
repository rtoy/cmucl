;; Bootstrap file for x86 to choose the non-negated forms of the
;; condition flag for conditional jumps.
;;
;; Use bin/build.sh -B boot-2021-07-2 to build this.

#+x86
(in-package :x86)

#+x86
(ext:without-package-locks
  (handler-bind
      ((error
	 (lambda (c)
	   (declare (ignore c))
	   (invoke-restart 'continue))))
    (defconstant conditions
      '((:o . 0)
	(:no . 1)
	(:b . 2) (:nae . 2) (:c . 2)
	(:ae . 3) (:nb . 3) (:nc . 3)
	(:e . 4) (:eq . 4) (:z . 4)
	(:ne . 5) (:nz . 5)
	(:be . 6) (:na . 6)
	(:a . 7) (:nbe . 7)
	(:s . 8)
	(:ns . 9)
	(:p . 10) (:pe . 10)
	(:np . 11) (:po . 11)
	(:l . 12) (:nge . 12)
	(:ge . 13) (:nl . 13)
	(:le . 14) (:ng . 14)
	(:g . 15) (:nle . 15)))))
