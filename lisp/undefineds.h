/* Routines that must be linked into the core for lisp to work. */
/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/undefineds.h,v 1.3 1994/03/27 15:24:12 hallgren Exp $ */

/* Pick up all the syscalls. */
accept,
access,
acct,
#ifndef hpux
adjtime,
#endif
bind,
brk,
chdir,
chmod,
chown,
chroot,
close,
connect,
creat,
dup,
dup2,
execve,
exit,
fchmod,
fchown,
fcntl,
#ifndef hpux
flock,
#endif
fork,
fstat,
fsync,
ftruncate,
#ifndef hpux
getdtablesize,
#endif
getegid,
geteuid,
getgid,
getgroups,
gethostid,
gethostname,
getitimer,
#ifndef hpux
getpagesize,
#endif
getpeername,
getpgrp,
getpid,
getppid,
getpriority,
getrlimit,
#ifndef hpux
getrusage,
#endif
getsockname,
getsockopt,
gettimeofday,
getuid,
ioctl,
kill,
killpg,
link,
listen,
lseek,
lstat,
mkdir,
mknod,
mount,
open,
pipe,
profil,
ptrace,
#if !defined(SUNOS) && !defined(parisc) && !defined(osf1)
quota,
#endif
read,
readlink,
readv,
reboot,
recv,
recvfrom,
recvmsg,
rename,
rmdir,
sbrk,
select,
send,
sendmsg,
sendto,
setgroups,
#ifndef SUNOS
sethostid,
#endif
sethostname,
setitimer,
setpgrp,
setpriority,
#if !defined(SUNOS) && !defined(parisc) && !defined(osf1)
setquota,
#endif
#ifndef hpux
setregid,
setreuid,
#endif
setrlimit,
setsockopt,
settimeofday,
shutdown,
sigblock,
sigpause,
#if !defined(ibmrt) && !defined(hpux)
sigreturn,
#endif
sigsetmask,
sigstack,
sigvec,
socket,
socketpair,
stat,
swapon,
symlink,
sync,
syscall,
#ifdef hpux
closedir,
opendir,
readdir,
tcgetattr,
tcsetattr,
#endif
truncate,
umask,
#if !defined(SUNOS) && !defined(parisc)
umount,
#endif
unlink,
#ifndef hpux
utimes,
#endif
vfork,
#ifndef osf1
vhangup,
#endif
wait,
wait3,
write,
writev,

/* Math routines. */
cos,
sin,
tan,
acos,
asin,
atan,
atan2,
sinh,
cosh,
tanh,
#ifndef hpux
asinh,
acosh,
atanh,
#endif
exp,
#ifndef hpux
expm1,
#endif
log,
log10,
#ifndef hpux
log1p,
#endif
pow,
#ifndef hpux
cbrt,
#endif
sqrt,
hypot,

/* Network support. */
gethostbyname,
gethostbyaddr,

/* Other random things. */
getwd,
ttyname

