;;;;
;;;; Boot file for making C::SEQUENCE-COUNT = KERNEL:SEQUENCE-COUNT,
;;;; which it should be because KERNEL:SEQUENCE-COUNT names a type
;;;; used in the compiler.
;;;;
;;;; Note that this doesn't work with Pierre Mai's build scripts
;;;; because these load code:exports.lisp before they load a
;;;; target:bootstrap.lisp.  Just choose the
;;;; UNINTERN-CONFLICTING-SYMBOLS restart when asked in this case.
;;;;

(unintern 'c::sequence-count)
(export '(kernel::sequence-count) :kernel)

;;;; end of file
