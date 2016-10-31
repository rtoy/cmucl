;; Test case from issue #24.
(defun foo ()
  (print "Hello world"))

(compile 'foo)
