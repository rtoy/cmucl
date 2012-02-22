;; Re-enable this on sparc.  It makes the simple FFT butterfly 2.5
;; times faster.
#+sparc
(pushnew :complex-fp-vops *features*)
