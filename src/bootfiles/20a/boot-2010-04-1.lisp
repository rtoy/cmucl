;; Bootstrap changes to intl, because the build scripts access
;; functions that don't exist in the previous version of intl.  Just
;; load the old bootstrap for simplicity instead of just defining the
;; necessary functions.
(load "target:bootfiles/20a/boot-2010-02-1")