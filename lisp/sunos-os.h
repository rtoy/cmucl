#ifdef SOLARIS
#undef boolean
#endif /* SOLARIS */

#include <sys/types.h>

#ifdef SOLARIS
#define boolean int
#include <signal.h>
#include <unistd.h>
#include <sys/fcntl.h>
#endif /* SOLARIS */

#include <sys/mman.h>

#ifdef SOLARIS
#include <ucontext.h>
#define sigcontext ucontext

#define POSIX_SIGS

#endif /* SOLARIS */

typedef unsigned long os_vm_address_t;
typedef long os_vm_size_t;
typedef off_t os_vm_offset_t;
typedef int os_vm_prot_t;

#define OS_VM_PROT_READ PROT_READ
#define OS_VM_PROT_WRITE PROT_WRITE
#define OS_VM_PROT_EXECUTE PROT_EXEC

#define OS_VM_DEFAULT_PAGESIZE	8192

#ifdef SOLARIS
#include <ucontext.h>
#define HANDLER_ARGS int signal, siginfo_t *code, struct ucontext *context
#define CODE(code)  ((code) ? code->si_code : 0)
#define SAVE_CONTEXT() save_context()

#ifdef NULL
#undef NULL
#define NULL 0
#endif

#define bcopy(a,b,n) memmove(b,a,n)
#define bzero(a,n) memset(a,0,n)
#endif /* SOLARIS */
