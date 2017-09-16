/*
 * Support for run-program.
 *
 */

#include <stdio.h>

#include <sys/ioctl.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>
#include <sys/wait.h>

pid_t
spawn(char *program, char *argv[], char *envp[], char *pty_name,
      int stdin, int stdout, int stderr)
{
    pid_t pid;
    sigset_t set;
    int fd;

    pid = fork();
    if (pid != 0)
	return pid;

    /* Put us in our own process group. */
    setsid();

    /* Unblock all signals. */
    sigemptyset(&set);
    sigprocmask(SIG_SETMASK, &set, NULL);

    /* If we are supposed to be part of some other pty, go for it. */
    if (pty_name) {
#ifdef TIOCNOTTY
	fd = open("/dev/tty", O_RDWR, 0);
	if (fd >= 0) {
	    ioctl(fd, TIOCNOTTY, 0);
	    close(fd);
	}
#endif

	fd = open(pty_name, O_RDWR, 0);
	dup2(fd, 0);
	dup2(fd, 1);
	dup2(fd, 2);
	close(fd);
    }

    /* Set up stdin, stdout, and stderr. */
    if (stdin >= 0)
	dup2(stdin, 0);
    if (stdout >= 0)
	dup2(stdout, 1);
    if (stderr >= 0)
	dup2(stderr, 2);

    /* Close all other fds. */
    for (fd = sysconf(_SC_OPEN_MAX) - 1; fd >= 3; fd--)
	close(fd);

    /* Exec the program. */
    execve(program, argv, envp);

    /* It didn't work, so try /bin/sh. */
    argv[0] = program;
    argv[-1] = "sh";
    execve("/bin/sh", argv - 1, envp);

    /* The exec didn't work, flame out. */
    exit(1);
}

/*
 * Call waitpid and return appropriate information about what happened.
 *
 * what  - int taking the values: 
 *              0 - ok
 *              1 - signaled
 *              2 - stopped
 *              3 - continued
 *              4 - exited
 * code   - the terminating signal
 * core   - true (non-zero) if a core was produced
 */

/*
 * Status codes.  Must be in the same order as in ext::prog-status in
 * run-program.lisp
 */
enum status_code {
    SIGNALED,
    STOPPED,
    CONTINUED,
    EXITED
};
    
void
prog_status(pid_t* pid, int* what, int* code, int* corep)
{
    pid_t w;
    int status;

    w = waitpid(-1, &status, WNOHANG | WUNTRACED | WCONTINUED);
    *pid = w;

    if (w <= 0) {
        return;
    }

    if (WIFEXITED(status)) {
        *what = EXITED;
        *code = WEXITSTATUS(status);
        *corep = 0;
    } else if (WIFSIGNALED(status)) {
        *what = SIGNALED;
        *code = WTERMSIG(status);
        *corep = WCOREDUMP(status);
    } else if (WIFSTOPPED(status)) {
        *what = STOPPED;
        *code = WSTOPSIG(status);
        *corep = 0;
    } else if (WIFCONTINUED(status)) {
        *what = CONTINUED;
        *code = 0;
        *corep = 0;
    } else {
        fprintf(stderr, "Unhandled waidpid status: pid = %d, status = 0x%x\n", *pid, status);
    }

    return;
}
