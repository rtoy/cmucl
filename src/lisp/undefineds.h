/* Routines that must be linked into the core for lisp to work. */

/*
 * Do not wrap this inside an #ifndef/#endif!
 *
 * This file is intentionally included multiple times in undefineds.c
 * and is expected to do something useful each time!
 */

/* Pick up all the syscalls. */
F(accept)
    F(access)
    F(acct)
#ifndef hpux
    F(adjtime)
#endif
    F(bind)
    F(brk)
    F(calloc)
#if defined(hpux) || defined(SVR4) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
    F(cfgetospeed)
    F(cfsetospeed)
    F(cfgetispeed)
    F(cfsetispeed)
#endif
    F(chdir)
    F(chmod)
    F(chown)
    F(chroot)
    F(close)
    F(connect)
    F(creat)
    F(dup)
    F(dup2)
    F(execve)
    F(exit)
    F(fchmod)
    F(fchown)
    F(fcntl)
#if !defined(hpux) && !defined(SVR4)
    F(flock)
#endif
    F(fork)
    F(free)
    F(fstat)
    F(fsync)
    F(ftruncate)
#if !defined(hpux) && !defined(SVR4) || defined(SOLARIS) || defined(irix)
    F(getdtablesize)
#endif
    F(getegid)
    F(getenv)
    F(geteuid)
    F(getgid)
    F(getgroups)
    F(gethostid)
    F(gethostname)
    F(getitimer)
#if !defined(hpux) && !defined(SVR4) || defined(SOLARIS)
    F(getpagesize)
#endif
    F(getpeername)
    F(getpgrp)
    F(getpid)
    F(getppid)
#if !defined(SVR4)  ||  defined(SOLARIS)
    F(getpriority)
#endif
    F(getrlimit)
    F(getrusage)
    F(getsockname)
    F(getsockopt)
    F(gettimeofday)
    F(getuid)
    F(ioctl)
    F(isatty)
    F(kill)
    F(killpg)
    F(link)
    F(listen)
    F(lseek)
    F(lstat)
    F(malloc)
    F(mkdir)
    F(mknod)
    F(mmap)
    F(mprotect)
    F(mount)
    F(msync)
    F(munmap)
    F(open)
    F(opendir)
    F(openpty)
    F(pipe)
    F(profil)
    F(ptrace)
    F(putenv)
#ifdef mach
    F(quota)
#endif
    F(read)
    F(readlink)
    F(readv)
#ifndef SVR4
    F(reboot)
#endif
    F(recv)
    F(recvfrom)
    F(recvmsg)
    F(rename)
    F(rmdir)
    F(sbrk)
    F(select)
    F(send)
    F(sendmsg)
    F(sendto)
    F(setenv)
    F(setgroups)
#if !defined(SUNOS) && !defined(SOLARIS)
    F(sethostid)
#endif
#if !defined(SVR4) ||  defined(SOLARIS)
    F(sethostname)
#endif
    F(setitimer)
    F(setpgrp)
#if !defined(SVR4) ||  defined(SOLARIS)
    F(setpriority)
#endif
#if !defined(mach) && !defined(SOLARIS) && !defined(__FreeBSD__) && !defined(__OpenBSD__) && !defined(__NetBSD__) && !defined(SUNOS) && !defined(osf1) && !defined(irix) && !defined(hpux) && !(defined(linux) && defined(alpha))
/*    F(setquota)*/
#endif
#if !defined(hpux) && !defined(SVR4) ||  defined(SOLARIS)
    F(setregid)
    F(setreuid)
#endif
    F(setrlimit)
    F(setsockopt)
    F(settimeofday)
    F(setgid)
    F(setuid)
    F(shutdown)
#ifndef SVR4
    F(sigblock)
#endif
    F(sigpause)
#if !defined(ibmrt) && !defined(hpux) && !defined(SVR4) && !defined(i386) && !defined(__arm__)
    F(sigreturn)
#endif
#if !defined(SVR4) && !defined(__FreeBSD__) && !defined(__OpenBSD__) && !defined(__NetBSD__) && !defined(DARWIN)
    F(sigsetmask)
    F(sigaltstack)
#endif
#if !defined(SVR4) && !defined(__FreeBSD__) && !defined(__OpenBSD__) && !defined(__NetBSD__) && !defined(DARWIN) && !defined(__linux__)
    F(sigvec)
#endif
    F(socket)
    F(socketpair)
    F(stat)
#ifndef SVR4
    F(swapon)
#endif
    F(symlink)
    F(sync)
    F(syscall)
#if defined(hpux) || defined(SVR4)
    F(closedir)
    F(opendir)
    F(readdir)
#endif
#if defined(hpux) || defined(SVR4) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__linux__)
    F(tcgetattr)
    F(tcsetattr)
    F(tcsendbreak)
    F(tcdrain)
    F(tcflush)
    F(tcflow)
#endif
#if defined(SOLARIS)
    F(times)
#endif
    F(truncate)
    F(umask)
#if !defined(SUNOS) && !defined(parisc) && !defined(SOLARIS) \
  && !defined(__FreeBSD__) && !defined(__OpenBSD__) && !defined(__NetBSD__) \
  && !defined(DARWIN)
    F(umount)
#endif
    F(uname)
    F(unlink)
    F(unsetenv)
#ifndef hpux
    F(utimes)
#endif
#ifndef irix
    F(vfork)
#endif
#if !defined(osf1) && !defined(__FreeBSD__) && !defined(__OpenBSD__) && !defined(__NetBSD__) && !defined(DARWIN)
    F(vhangup)
#endif
    F(wait)
    F(wait3)
    F(write)
    F(writev)

/* Math routines. */
    F(cos)
    F(sin)
    F(tan)
    F(acos)
    F(asin)
    F(atan)
    F(atan2)
    F(sinh)
    F(cosh)
    F(tanh)
    F(asinh)
    F(acosh)
    F(atanh)
    F(exp)
#ifndef hpux
    F(expm1)
#endif
    F(log)
    F(log10)
#ifndef hpux
    F(log1p)
#endif
    F(pow)
#ifndef hpux
    F(cbrt)
#endif
#ifndef i386
    F(sqrt)
#endif
    F(hypot)

/* Network support. */
    F(gethostbyname)
    F(gethostbyaddr)

/* Other random things. */
#if defined(SVR4)
    F(setpgid)
    F(getpgid)
    D(timezone)
    D(altzone)
    D(daylight)
    D(tzname)
#endif
#if defined(SVR4) || defined(__OpenBSD__)
    F(dlopen)
    F(dlsym)
    F(dlclose)
    F(dlerror)
#endif
#if !defined (SOLARIS) ||  defined(SOLARIS25)
    F(getwd)
    F(getcwd)
#endif
    F(ttyname)
#ifdef irix
    F(_getpty)
#endif
#if ( defined(alpha) && defined(linux) )
    F(dlopen)
    F(dlsym)
    F(dlclose)
    F(dlerror)
    F(cfgetospeed)
    F(cfsetospeed)
    F(cfgetispeed)
    F(cfsetispeed)
    F(opendir)
    F(closedir)
    F(readdir)
    F(sched_yield)
    F(setpgid)
    D(tzname)
    D(errno)
    F(open64)
    F(creat64)
    F(lseek64)
    F(truncate64)
    F(ftruncate64)
    F(stat64)
    F(fstat64)
    F(lstat64)
    F(readdir64)
    F(statfs64)
    F(lockf64)
#endif
#if defined(sparc)
    F(dladdr)
    F(open64)
    F(creat64)
    F(lseek64)
    F(truncate64)
    F(ftruncate64)
    F(stat64)
    F(fstat64)
    F(lstat64)
    F(readdir64)
    F(sysinfo)
    F(uname)
    F(getpwent_r)
#endif  
#if defined(sparc) || defined(linux)
    F(getpwnam_r)
    F(getpwuid_r)
    F(getgrnam_r)
    F(getgrgid_r)
#endif
#if defined(__NetBSD__) || defined(DARWIN)
    F(getpwnam)
    F(getpwuid)
    F(getgrnam)
    F(getgrgid)
#endif
    F(setpwent)
    F(getpwent)
    F(endpwent)
#if defined(linux)
    F(cfgetispeed)
    F(cfgetospeed)
    F(cfsetispeed)
    F(cfsetospeed)
    F(closedir)
    F(creat64)
    F(fstat64)
    F(ftruncate64)
    F(lockf64)
    F(lseek64)
    F(lstat64)
    F(open64)
    F(readdir64)
    F(sched_yield)
    F(setpgid)
    F(stat64)
    F(truncate64)
    F(tzname)
#endif
