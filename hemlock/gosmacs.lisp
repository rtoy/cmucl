;;; -*- Package: Hemlock; Log: Hemlock.Log -*-
;;;
;;;    Stuff in this file provides some degree of upward compatibility
;;; for incurable Gosling Emacs users.
;;;
(in-package 'hemlock)

(defcommand "Gosmacs Permute Characters" (p)
  "Transpose the two characters before the point."
  "Transpose the two characters before the point."
  (declare (ignore p))
  (with-mark ((m (current-point) :left-inserting))
    (unless (and (mark-before m) (previous-character m))
      (editor-error "NIB     You have addressed a character not in the buffer?"))
    (rotatef (previous-character m) (next-character m))))

(bind-key "Gosmacs Permute Characters" #\control-t)
(bind-key "Kill Previous Word" #\meta-h)
(bind-key "Replace String" #\meta-r)
(bind-key "Query Replace" #\meta-q)
(bind-key "Fill Paragraph" #\meta-j)
(bind-key "Visit File" '#(#\control-x #\control-r))
(bind-key "Find File" '#(#\control-x #\control-v))
(bind-key "Insert File" '#(#\control-x #\control-i))
