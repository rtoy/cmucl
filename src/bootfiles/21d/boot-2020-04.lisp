;;; Use SSE3 and SSSE3 instructions for x86
#+x86
(pushnew :sse3 *features*)
#+x86
(pushnew :ssse3 *features*)