/* Routines that must be linked into the core for lisp to work. */
/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/undefineds.h,v 1.4 1994/07/05 16:11:04 hallgren Exp $ */

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
#if !defined(SUNOS) && !defined(parisc) && !defined(osf1) && !defined(irix)
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
#if !defined(SUNOS) && !defined(parisc) && !defined(osf1) && !defined(irix)
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
#if !defined(ibmrt) && !defined(hpux) && !defined(irix)
sigreturn,
#endif
sigsetmask,
sigstack,
sigvec,
socket,
socketpair,
stat,
#ifndef irix
swapon,
#endif
symlink,
sync,
syscall,
#ifdef hpux
closedir,
opendir,
readdir,
#endif
#if defined(hpux) || defined(irix)
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
#ifndef irix
vfork,
#endif
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

#ifdef irix
,_getpty
#endif
