;; Remove the UNIX package from the package use list of the VM
;; package.  The UNIX package is only needed to provide the SIGCONTEXT
;; symbol, so we just prefix it everywhere.

(unuse-package "UNIX" "VM")
