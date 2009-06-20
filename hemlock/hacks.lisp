(in-package "HI")

(defun %sp-byte-blt (src start dest dstart end)
  #-unicode
  (%primitive byte-blt src start dest dstart end)
  #+unicode
  (loop for di of-type fixnum from dstart below end
        for si of-type fixnum from start
        do 
        (setf (aref dest di) (aref src si))))

(defun lisp::sap-to-fixnum (x) (sap-int x))
(defun lisp::fixnum-to-sap (x) (int-sap x))
(defun lisp::%sp-make-fixnum (x) (%primitive make-fixnum x))
(defun lisp::fast-char-upcase (x) (char-upcase x))

;;; prepare-window-for-redisplay  --  Internal
;;;
;;;    Called by make-window to do whatever redisplay wants to set up
;;; a new window.
;;;
(defun prepare-window-for-redisplay (window)
  (setf (window-old-lines window) 0))

(defparameter hunk-width-limit 256)

(defun reverse-video-hook-fun (&rest foo)
  (declare (ignore foo)))
