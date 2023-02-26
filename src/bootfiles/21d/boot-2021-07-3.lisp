(in-package :x86)

(ext:without-package-locks
  (defparameter static-functions
    '(length
      two-arg-+ two-arg-- two-arg-* two-arg-/ two-arg-< two-arg-> two-arg-= eql
      %negate two-arg-and two-arg-ior two-arg-xor two-arg-gcd two-arg-lcm
      two-arg-<= two-arg->=)))
