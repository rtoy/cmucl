;; Tests for RNG

(defpackage :rng-tests
  (:use :cl :lisp-unit))

(in-package "RNG-TESTS")

(defun 64-bit-rng-state (rng)
  (let ((state (kernel::random-state-state rng)))
    (flet ((convert (x)
	     (multiple-value-bind (hi lo)
		 (kernel:double-float-bits x)
	       (logior (ash (ldb (byte 32 0) hi) 32)
		       lo))))
      (values (convert (aref state 0)) (convert (aref state 1))))))

(defun 64-bit-value (rng)
  (logior (ash (kernel::random-chunk rng) 32)
	  (kernel::random-chunk rng)))

(defvar *test-state*)
  
(define-test rng.state
  (let ((s (kernel::random-state-state *random-state*)))
    #+random-xoroshiro
    (assert-true (typep s '(simple-array double-float (2))))
    #+random-mt19937
    (assert-true (typep s '(simple-array (unsigned-byte 32) (627))))))

#+random-xoroshiro
(define-test rng.initial-state
  (setf *test-state*
	(kernel::make-random-object :state (kernel::init-random-state #x12345678)
				    :rand 0
				    :cached-p nil))
  (multiple-value-bind (s0 s1)
      (64-bit-rng-state *test-state*)
    (assert-equal #x38f1dc39d1906b6f s0)
    (assert-equal #xdfe4142236dd9517 s1)
    (assert-equal 0 (kernel::random-state-rand *test-state*))
    (assert-equal nil (kernel::random-state-cached-p *test-state*))))


#+random-xoroshiro
(define-test rng.values-test
  (assert-equal (list #x38f1dc39d1906b6f #xdfe4142236dd9517)
		(multiple-value-list (64-bit-rng-state *test-state*)))
  (assert-equal 0 (kernel::random-state-rand *test-state*))
  (assert-equal nil (kernel::random-state-cached-p *test-state*))

  (dolist (item #-x86
                '((#x18d5f05c086e0086 (#x228f4926843b364d #x74dfe78e715c81be))
		  (#x976f30b4f597b80b (#x5b6bd4558bd96a68 #x567b7f35650aea8f))
		  (#xb1e7538af0e454f7 (#x13e5253e242fac52 #xed380e70d10ab60e))
		  (#x011d33aef53a6260 (#x9d0764952ca00d8a #x5251a5cfedd2b4ef))
		  (#xef590a651a72c279 (#xba4ef2b425bda963 #x172b965cf56c15ac))
		  (#xd17a89111b29bf0f (#x458277a5e5f0a21b #xd1bccfad6564e8d))
		  (#x529e44a0bc46f0a8 (#x2becb68d5a7194c7 #x3a6ec964899bb5f3))
		  (#x665b7ff1e40d4aba (#xededfd481d0a19fe #x3ea213411827fe9d))
		  (#x2c9010893532189b (#xd7bb59bcd8fba26f #x52de763d34fee090))
		  (#x2a99cffa0dfa82ff (#xf96e892c62d6ff2e #xc0542ff85652f81e)))
                #+x86
                '((#x41db14eb317141fe (#x16dfbf3d760d0fa4 #xe9bfcf1ce2b9037c))
                  (#xaa4ee6e025dfec01 (#xb237e99a3c7ad367 #x96819b1fec0e0432))
                  (#xea080e50cb948fa5 (#xcc0fd8226093e0bc #x0e9aeaa496ce50ba))
                  (#x647f057cff408a6e (#xd273573bfa97bfde #xcbb600d852a650de))
                  (#x232ac586565d037e (#x75dc686d99e39c57 #x063de00338aafc75))
                  (#xdf2da206813da6d6 (#x9616cabb961ebc4a #x292c044e7c310dd4))
                  (#x00d17cb1b38c852f (#xca593a661127a754 #x45f633d7e759debd))
                  (#xd7a1f881fc34e641 (#xe00fd868db5d20d3 #xcfcf3d31f5e1363e))
                  (#x64853747af628d30 (#xa24296c5ebb11935 #xd782dda5f81cab25))
                  (#xda40653710b7293d (#xfb4be9d4941ff086 #x75b6420eb8096c02))))
    (destructuring-bind (value state)
	item
      (assert-equal value (64-bit-value *test-state*))
      (assert-equal state (multiple-value-list (64-bit-rng-state *test-state*))))))

#+random-xoroshiro
(define-test rng.jump
  (setf *test-state*
	(kernel::make-random-object :state (kernel::init-random-state #x12345678)
				    :rand 0
				    :cached-p nil))
  (dolist (result
           #-x86 '((#x291ddf8e6f6a7b67 #x1f9018a12f9e031f)
		   (#x88a7aa12158558d0 #xe264d785ab1472d9)
		   (#x207e16f73c51e7ba #x999c8a0a9a8d87c0)
		   (#x28f8959d3bcf5ff1 #x38091e563ab6eb98))
           #+x86 '((#x19a22191480b0a4e #x43b3d7ee592dd4cf)
                   (#x76cb87035d0b6e99 #xb6827bcf2ef8267c)
                   (#x5125201dbdf76860 #x8984c075043869e2)
                   (#x2c06f0667255309f #xa48cbe2e60fc1d65)
                   ))
    (kernel:random-state-jump *test-state*)
    (assert-equal result (multiple-value-list
			  (64-bit-rng-state *test-state*)))))

