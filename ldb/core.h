
#define CORE_PAGESIZE (4*1024)
#define CORE_MAGIC (('C' << 24) | ('O' << 16) | ('R' << 8) | 'E')
#define CORE_END 3840
#define CORE_NDIRECTORY 3861
#define CORE_VALIDATE 3845
#define CORE_VERSION 3860
#define CORE_MACHINE_STATE 3862

#define DYNAMIC_SPACE_ID (1)
#define STATIC_SPACE_ID (2)
#define READ_ONLY_SPACE_ID (3)

struct ndir_entry {
	long identifier;
	long nwords;
	long data_page;
	long address;
	long page_count;
};

struct machine_state {
    lispobj *csp;
    lispobj *fp;
    lispobj *bsp;
    long flags;
    char *number_stack_start;

    long sigcontext_page;
    long control_stack_page;
    long binding_stack_page;
    long number_stack_page;
};
