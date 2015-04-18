;; Load extra functionality in the UNIX package.

(ext:without-package-locks
  (load "modules:unix/unix"))

(provide 'unix)
