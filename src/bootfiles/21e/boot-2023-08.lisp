;; Build with -B boot-2023-08 from the 2023-08 snapshot.  We're moving
;; *SOFTWARE-VERSION* from the LISP package to the SYSTEM package.
(ext:without-package-locks
    (unintern 'lisp::*software-version* "LISP"))

#+(or random-mt19937 random-xoroshiro)
(in-package "C")
#+(or random-mt19937 random-xoroshiro)
(deftransform random ((num &optional state)
		      ((integer 1 #.(expt 2 32)) &optional *))
  _N"use inline (unsigned-byte 32) operations"
  (let* ((num-type (continuation-type num))
	 (num-high (cond ((numeric-type-p num-type)
			  (numeric-type-high num-type))
			 ((union-type-p num-type)
			  ;; Find the maximum of the union type.  We
			  ;; know this works because if we're in this
			  ;; routine, NUM must be a subtype of
			  ;; (INTEGER 1 2^32), so each member of the
			  ;; union must be a subtype too.
			  (reduce #'max (union-type-types num-type)
				  :key #'numeric-type-high))
			 (t
			  (give-up)))))
    ;; Rather than doing (rem (random-chunk) num-high), we do,
    ;; essentially, (rem (* num-high (random-chunk)) #x100000000).  I
    ;; (rtoy) believe this approach doesn't have the bias issue with
    ;; doing rem.  This method works by treating (random-chunk) as if
    ;; it were a 32-bit fraction between 0 and 1, exclusive.  Multiply
    ;; this by num-high to get a random number between 0 and num-high,
    ;; This should have no bias.
    (cond ((constant-continuation-p num)
	   (if (= num-high (expt 2 32))
	       '(random-chunk (or state *random-state*))
	       '(values (bignum::%multiply 
			 (random-chunk (or state *random-state*))
			 num))))
	  ((< num-high (expt 2 32))
	   '(values (bignum::%multiply (random-chunk (or state *random-state*))
		     num)))
	  ((= num-high (expt 2 32))
	   '(if (= num (expt 2 32))
		(random-chunk (or state *random-state*))
		(values (bignum::%multiply (random-chunk (or state *random-state*))
					   num))))
	  (t
	   (error (intl:gettext "Shouldn't happen"))))))
