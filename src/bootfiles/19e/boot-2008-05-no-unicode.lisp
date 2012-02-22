;; Bootstrap file to build the code without unicode.  This is needed
;; because some new constants were added.

(in-package "VM")
(defconstant char-bits #-unicode 8 #+unicode 16
  "Number of bits needed to represent a character")

(defconstant char-bytes (truncate char-bits byte-bits)
  "Number of bytes needed to represent a character")


