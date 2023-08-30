;; Replacing lisp::*software-version* with system::*software-version*.
;;
;; Build with -B boot-2023-08 from the 21e release.
(ext:without-package-locks
  (unintern 'lisp::*software-version* "LISP"))
