;; Load lisp-unit
(require "asdf")

(load "modules:lisp-unit/lisp-unit.asd")

(asdf:oos 'asdf:load-op :lisp-unit)
