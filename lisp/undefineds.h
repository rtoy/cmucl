/* Routines that must be linked into the core for lisp to work. */
/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/undefineds.h,v 1.5 1994/10/25 00:26:53 ram Exp $ */

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
#if !defined(hpux) && !defined(SVR4)
flock,
#endif
fork,
fstat,
fsync,
ftruncate,
#if !defined(hpux) && !defined(SVR4)
getdtablesize,
#endif
getegid,
geteuid,
getgid,
getgroups,
#ifndef SVR4
gethostid,
#endif
gethostname,
getitimer,
#if !defined(hpux) && !defined(SVR4)
getpagesize,
#endif
getpeername,
getpgrp,
getpid,
getppid,
#ifndef SVR4
getpriority,
#endif
getrlimit,
#if !defined(hpux) && !defined(SVR4)
getrusage,
#endif
getsockname,
getsockopt,
gettimeofday,
getuid,
ioctl,
kill,
#ifndef SVR4
killpg,
#endif
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
#ifdef mach
quota,
#endif
read,
readlink,
readv,
#ifndef SVR4
reboot,
#endif
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
#if !defined(SUNOS) && !defined(SOLARIS)
sethostid,
#endif
#ifndef SVR4
sethostname,
#endif
setitimer,
setpgrp,
#ifndef SVR4
setpriority,
#endif
#ifdef mach
setquota,
#endif
#if !defined(hpux) && !defined(SVR4)
setregid,
setreuid,
#endif
setrlimit,
setsockopt,
settimeofday,
shutdown,
#ifndef SVR4
sigblock,
#endif
sigpause,
#if !defined(ibmrt) && !defined(hpux) && !defined(SVR4)
sigreturn,
#endif
#ifndef SVR4
sigsetmask,
sigstack,
sigvec,
#endif
socket,
socketpair,
stat,
#ifndef SVR4
swapon,
#endif
symlink,
sync,
syscall,
#if defined(hpux) || defined(SVR4)
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
#if !defined(SUNOS) && !defined(parisc) && !defined(SOLARIS)
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
#ifndef SVR4
wait3,
#endif
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
#ifdef SVR4
setpgid,
getpgid,
timezone,
altzone,
daylight,
tzname,
dlopen,
dlsym,
dlclose,
dlerror,
#endif
#ifndef SVR4
getwd,
#endif
ttyname

#ifdef irix
,_getpty
#endif
