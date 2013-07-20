;; Add :alien-callback to *features* to build callback support for
;; platforms that support alien callbacks.

#+(or x86 sparc ppc)
(pushnew :alien-callback *features*)

