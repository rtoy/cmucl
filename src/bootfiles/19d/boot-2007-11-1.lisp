;; Bootstrap file for bref cleanup

(in-package "EXTENSIONS")
(without-package-locks
(dolist (symbol '(bref buffer-sap endian-swap-value vector-elt-width))
  (unintern symbol)))

(in-package "KERNEL")
(without-package-locks
(export 'simple-stream-buffer)
(deftype simple-stream-buffer ()
  '(or sys:system-area-pointer (simple-unboxed-array (*))))
)
