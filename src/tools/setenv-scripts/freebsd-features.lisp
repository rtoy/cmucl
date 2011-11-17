;; e.g. for FreeBSD on x86 you probably want:
;;(pushnew :freebsd *features*)
;;(pushnew :elf *features*)

(setf *features* (remove :freebsd4 *features*))
