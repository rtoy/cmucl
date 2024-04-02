;; Cross-compile script to change the default random number generator
;; from MT19937 to xoroshiro128+.

;; The cross-script is basically the default platform script, but we
;; remove :random-mt19937 and add :random-xoroshiro to the backend
;; features.

#+x86
(load "src/bootfiles/21c/boot-21c-cross-x86.lisp")

#+sparc
(load "src/bootfiles/21c/boot-21c-cross-sparc.lisp")

