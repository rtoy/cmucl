;; Load extra functionality in the UNIX package.

(ext:without-package-locks
  (load (compile-file-pathname #-linux "modules:unix/unix"
			       #+linux "modules:unix/unix-glibc2")))

(provide 'unix)
