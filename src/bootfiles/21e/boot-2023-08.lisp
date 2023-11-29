;; Build with -B boot-2023-08 from the 2023-08 snapshot.  We're moving
;; *SOFTWARE-VERSION* from the LISP package to the SYSTEM package.
(ext:without-package-locks
    (unintern 'lisp::*software-version* "LISP"))
