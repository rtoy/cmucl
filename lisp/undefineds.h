/* Routines that must be linked into the core for lisp to work. */
/* $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/undefineds.h,v 1.17 1997/05/05 23:13:59 dtc Exp $ */

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
#if !defined(hpux) && !defined(SVR4) || defined(SOLARIS25)
getdtablesize,
#endif
getegid,
geteuid,
getgid,
getgroups,
#if !defined (SOLARIS) || defined(SOLARIS25)
gethostid,
#endif
gethostname,
getitimer,
#if !defined(hpux) && !defined(SVR4) || defined(SOLARIS25)
getpagesize,
#endif
getpeername,
getpgrp,
getpid,
getppid,
#if !defined(SVR4)  ||  defined(SOLARIS25)
getpriority,
#endif
getrlimit,
#if !defined(SOLARIS) ||  defined(SOLARIS25)
getrusage,
#endif
getsockname,
getsockopt,
gettimeofday,
getuid,
ioctl,
kill,
#if !defined(SOLARIS) || defined(SOLARIS25)
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
#ifndef __linux__
profil,
#endif
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
#if !defined(SUNOS) && !(defined(SOLARIS) ||  defined(SOLARIS25))
sethostid,
#endif
#if !defined(SVR4) ||  defined(SOLARIS25)
sethostname,
#endif
setitimer,
setpgrp,
#if !defined(SVR4) ||  defined(SOLARIS25)
setpriority,
#endif
#if !defined(mach) && !defined(SOLARIS) && !defined(__FreeBSD__) && !defined(__linux__) && !defined(SUNOS) && !defined(osf1) && !defined(irix)
setquota,
#endif
#if !defined(hpux) && !defined(SVR4) ||  defined(SOLARIS25)
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
#if !defined(ibmrt) && !defined(hpux) && !defined(SVR4) && !defined(i386)
sigreturn,
#endif
#if !defined(SVR4) && !defined(__FreeBSD__)
sigsetmask,
#ifndef __linux__
sigstack,
sigvec,
#endif
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
#if defined(__linux__) || defined(hpux) || defined(SVR4)
closedir,
opendir,
readdir,
tcgetattr,
tcsetattr,
#endif
truncate,
umask,
#if !defined(SUNOS) && !defined(parisc) && !defined(SOLARIS) \
  && !defined(__FreeBSD__)
umount,
#endif
unlink,
#ifndef hpux
utimes,
#endif
#ifndef irix
vfork,
#endif
#if !defined(osf1) && !defined(__FreeBSD__)
vhangup,
#endif
wait,
#if !defined(SOLARIS) ||  defined(SOLARIS25)
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
asinh,
acosh,
atanh,
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
#ifndef i386
sqrt,
#endif
hypot,

/* Network support. */
gethostbyname,
gethostbyaddr,

/* Other random things. */
#if defined(SVR4) || defined(__linux__)
setpgid,
getpgid,
timezone,
altzone,
daylight,
tzname,
#if !defined(__linux__) && !defined (irix)
dlopen,
dlsym,
dlclose,
dlerror,
#endif
#endif
#if !defined (SOLARIS) ||  defined(SOLARIS25)
getwd,
#endif
ttyname

#ifdef irix
,_getpty
#endif
