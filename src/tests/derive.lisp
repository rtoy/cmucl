(in-package 'c)

(defun foo (x y z r)
  (declare (type (unsigned-byte 8) x)
	   (type (unsigned-byte 4) y)
	   (type (signed-byte 8) z)
	   (type unsigned-byte r))
  (values
   (+ x y)
   (+ y z)
   (+ z r)
   (- x y)
   (- y z)
   (- z r)
   (* x y)
   (* y z)
   (* z r)
   (ash z -2)))
