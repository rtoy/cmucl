(in-package "VM")
(defconstant fixnum-tag-bits (1- lowtag-bits)
  "Number of tag bits used for a fixnum")

(defconstant fixnum-tag-mask (1- (ash 1 fixnum-tag-bits))
  "Mask to get the fixnum tag")

(defconstant positive-fixnum-bits (- word-bits fixnum-tag-bits 1)
  "Maximum number of bits in a positive fixnum")
