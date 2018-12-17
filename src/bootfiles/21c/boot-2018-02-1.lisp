;; Get rid of incorrect symbol c::%unary-fround and just inherit
;; %unary-fround from the kernel package.
(ext:without-package-locks
  (unintern 'c::%unary-fround "C"))
(export 'kernel::%unary-fround "KERNEL")
