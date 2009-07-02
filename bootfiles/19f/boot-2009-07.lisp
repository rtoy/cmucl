;; Bootstrap shrink-vector changes.  (The previous derive-type
;; optimizer for shrink-vector didn't handle union types.)

(in-package "C")

(without-package-locks
(defoptimizer (lisp::shrink-vector derive-type) ((vector new-size))
  ;; The result of shrink-vector is another vector of the same type as
  ;; the input.  If the size is a known constant, we use it, otherwise
  ;; just make the dimension unknown.
  (let* ((dim (if (constant-continuation-p new-size)
		  `(,(continuation-value new-size))
		  '(*)))
	 (vector-type (continuation-type vector))
	 (results (mapcar #'(lambda (type)
			      (let* ((new-type (kernel::copy-array-type type)))
				(setf (kernel:array-type-dimensions new-type) dim)
				new-type))
			  (if (typep vector-type 'union-type)
			      (union-type-types vector-type)
			      (list vector-type)))))
    (if (rest results)
	(make-union-type results)
	(first results))))
)
