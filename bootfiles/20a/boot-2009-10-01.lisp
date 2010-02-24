;; Bootstrap for non-unicode build.  Just updating the +ef-foo+ values.

(in-package "STREAM")

(handler-bind ((error #'(lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'kernel::continue))))
(vm::defenum (:prefix "+EF-" :suffix "+" :start 1)
  str					; string length
  cin					; input a character
  cout					; output a character
  sin					; input string
  sout					; output string
  os					; octets to string
  so					; string to octets
  en					; encode
  de					; decode
  flush					; flush state
  copy-state				; copy state
  max)
)

