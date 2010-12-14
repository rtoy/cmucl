;; Setup backend-foreign-linkage-space-start/entry-size for each
;; architecture.

#+x86
(setf (c::backend-foreign-linkage-space-start c:*target-backend*)
      #+linux #x58000000
      #-linux #xB0000000
      (c::backend-foreign-linkage-entry-size c:*target-backend*)
      8)

#+sparc
(setf (c::backend-foreign-linkage-space-start c:*target-backend*)
      ;; This better match the value in sparc-validate.h!
      #x0f800000
      (c::backend-foreign-linkage-entry-size c:*target-backend*)
      ;; This better agree with what sparc-arch.c thinks it is!  Right now,
      ;; it's 4 instructions, so 16 bytes.
      16)
#+ppc
(setf (c::backend-foreign-linkage-space-start c:*target-backend*)
      #x17000000
      (c::backend-foreign-linkage-entry-size c:*target-backend*)
      32)