;; Bootstrap file for x86 to add two-arg->= and two-arg-<= to static
;; functions list.
;;
;; Use bin/build.sh -B boot-2021-07-3 to build this (along with any
;; other bootfiles that are still needed).
(in-package :x86)
#+x86
(ext:without-package-locks
  (defparameter static-functions
    '(length
      two-arg-+ two-arg-- two-arg-* two-arg-/ two-arg-< two-arg-> two-arg-= eql
      %negate two-arg-and two-arg-ior two-arg-xor two-arg-gcd two-arg-lcm
      two-arg-<= two-arg->= two-arg-/=
      )))
