/*
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/sunos-os.c,v 1.2 1994/10/24 19:17:10 ram Exp $
 *
 * OS-dependent routines.  This file (along with os.h) exports an
 * OS-independent interface to the operating system VM facilities.
 * Suprisingly, this interface looks a lot like the Mach interface
 * (but simpler in some places).  For some operating systems, a subset
 * of these functions will have to be emulated.
 *
 * This is the SunOS version.
 * March 1991, Miles Bader <miles@cogsci.ed.ack.uk> & ted <ted@edu.NMSU>
 *
 */

/* #define DEBUG */

#include <stdio.h>

#include <signal.h>
#include <sys/file.h>

#include "os.h"

/* block size must be larger than the system page size */
#define SPARSE_BLOCK_SIZE (1<<15)
#define SPARSE_SIZE_MASK (SPARSE_BLOCK_SIZE-1)

#define PROT_DEFAULT OS_VM_PROT_ALL

#define OFFSET_NONE ((os_vm_offset_t)(~0))

#define EMPTYFILE "/tmp/empty"
#define ZEROFILE "/dev/zero"

#define INITIAL_MAX_SEGS 32
#define GROW_MAX_SEGS 16

extern char *getenv();

/* ---------------------------------------------------------------- */

#define ADJ_OFFSET(off,adj) (((off)==OFFSET_NONE) ? OFFSET_NONE : ((off)+(adj)))

long os_vm_page_size=(-1);

static struct segment {
    os_vm_address_t start;	/* note: start & length are expected to be on page */
    os_vm_size_t length;        /*       boundaries */
    long file_offset;
    short mapped_fd;
    short protection;
} *segments;

static int n_segments=0, max_segments=0;

static int zero_fd=(-1), empty_fd=(-1);

static os_vm_address_t last_fault=0;
static os_vm_size_t real_page_size_difference=0;

static void os_init_bailout(arg)
char *arg;
{
    char buf[500];
    sprintf(buf,"os_init: %s",arg);
    perror(buf);
    exit(1);
}

void os_init()
{
    char *empty_file=getenv("CMUCL_EMPTYFILE");

    if(empty_file==NULL)
	empty_file=EMPTYFILE;

    empty_fd=open(empty_file,O_RDONLY|O_CREAT);
    if(empty_fd<0)
	os_init_bailout(empty_file);
    unlink(empty_file);

    zero_fd=open(ZEROFILE,O_RDONLY);
    if(zero_fd<0)
	os_init_bailout(ZEROFILE);

    os_vm_page_size=getpagesize();

    max_segments=INITIAL_MAX_SEGS;
    segments=(struct segment *)malloc(sizeof(struct segment)*max_segments);
    if(segments==NULL){
	fprintf(stderr,"os_init: Couldn't allocate %d segment descriptors\n",
		max_segments);
	exit(1);
    }

    if(os_vm_page_size>OS_VM_DEFAULT_PAGESIZE){
	fprintf(stderr,"os_init: Pagesize too large (%d > %d)\n",
		os_vm_page_size,OS_VM_DEFAULT_PAGESIZE);
	exit(1);
    }else{
	/*
	 * we do this because there are apparently dependencies on
	 * the pagesize being OS_VM_DEFAULT_PAGESIZE somewhere...
	 * but since the OS doesn't know we're using this restriction,
	 * we have to grovel around a bit to enforce it, thus anything
	 * that uses real_page_size_difference.
	 */
	real_page_size_difference=OS_VM_DEFAULT_PAGESIZE-os_vm_page_size;
	os_vm_page_size=OS_VM_DEFAULT_PAGESIZE;
    }
}

/* ---------------------------------------------------------------- */

void seg_force_resident(struct segment *seg,
			os_vm_address_t addr,
			os_vm_size_t len)
{
    int prot=seg->protection;

    if(prot!=0){
	os_vm_address_t end=addr+len, touch=addr;
		
	while(touch<end){
	    int contents=(*(char *)touch);
	    if(prot&OS_VM_PROT_WRITE)
		(*(char *)touch)=contents;
	    touch=(os_vm_address_t)(((long)touch+SPARSE_BLOCK_SIZE)&~SPARSE_SIZE_MASK);
	}
    }
}

static struct segment *seg_create_nomerge(addr,len,protection,mapped_fd,file_offset)
os_vm_address_t addr;
os_vm_size_t len;
int protection;
int mapped_fd;
{
    int n;
    struct segment *seg;

    if(len==0)
	return NULL;

    if(n_segments==max_segments){
	struct segment *new_segs;

	max_segments+=GROW_MAX_SEGS;

	new_segs=(struct segment *)
	    realloc(segments,max_segments*sizeof(struct segment));

	if(new_segs==NULL){
	    fprintf(stderr,
		    "seg_create_nomerge: Couldn't grow segment descriptor table to %s segments\n",
		    max_segments);
	    max_segments-=GROW_MAX_SEGS;
	    return NULL;
	}
	    
	segments=new_segs;
    }

    for(n=n_segments, seg=segments; n>0; n--, seg++)
	if(addr<seg->start){
	    seg=(&segments[n_segments]);
	    while(n-->0){
		seg[0]=seg[-1];
		seg--;
	    }
	    break;
	}

    n_segments++;

    seg->start=addr;
    seg->length=len;
    seg->protection=protection;
    seg->mapped_fd=mapped_fd;
    seg->file_offset=file_offset;

    return seg;
}

#if 1
/* returns the first segment containing addr */
static struct segment *seg_find(addr)
os_vm_address_t addr;
{
    int n;
    struct segment *seg;

    for(n=n_segments, seg=segments; n>0; n--, seg++)
	if(seg->start<=addr && seg->start+seg->length>addr)
	    return seg;

    return NULL;
}
#else
/* returns the first segment containing addr */
static struct segment *seg_find(addr)
os_vm_address_t addr;
{
    /* does a binary search */
    struct segment *lo=segments, *hi=segments+n_segments;

    while(hi>lo){
	struct segment *mid=lo+((hi-lo)>>1);
	os_vm_address_t start=mid->start;

	if(addr>=start && addr<start+mid->length)
	    return mid;
	else if(addr<start)
	    hi=mid;
	else
	    lo=mid+1;
    }

    return NULL;
}
#endif

/* returns TRUE if the range from addr to addr+len intersects with any segment */
static boolean collides_with_seg_p(addr,len)
os_vm_address_t addr;
os_vm_size_t len;
{
    int n;
    struct segment *seg;
    os_vm_address_t end=addr+len;

    for(n=n_segments, seg=segments; n>0; n--, seg++)
	if(seg->start>=end)
	    return FALSE;
	else if(seg->start+seg->length>addr)
	    return TRUE;

    return FALSE;
}

#define seg_last_p(seg) (((seg)-segments)>=n_segments-1)

static void seg_destroy(seg)
struct segment *seg;
{
    if(seg!=NULL){
	int n;

	for(n=seg-segments+1; n<n_segments; n++){
	    seg[0]=seg[1];
	    seg++;
	}

	n_segments--;
    }
}

static void seg_try_merge_next(seg)
struct segment *seg;
{
    struct segment *nseg=seg+1;

    if(!seg_last_p(seg)
       && seg->start+seg->length==nseg->start
       && seg->protection==nseg->protection
       && seg->mapped_fd==nseg->mapped_fd
       && ADJ_OFFSET(seg->file_offset,seg->length)==nseg->file_offset)
    {
	/* can merge with the next segment */
#ifdef DEBUG
	fprintf(stderr,
		";;; seg_try_merge: Merged 0x%08x[0x%08x] with 0x%08x[0x%08x]\n",
		seg->start,seg->length,nseg->start,nseg->length);
#endif

	if(((long)nseg->start&SPARSE_SIZE_MASK)!=0){
	    /*
	     * if not on a block boundary, we have to ensure both parts
	     * of a common block are in a known state
	     */
	    seg_force_resident(seg,nseg->start-1,1);
	    seg_force_resident(nseg,nseg->start,1);
	}

	seg->length+=nseg->length;
	seg_destroy(nseg);
    }
}


/*
 * Try to merge seg with adjacent segments.
 */
static void seg_try_merge_adjacent(seg)
struct segment *seg;
{
    if(!seg_last_p(seg))
	seg_try_merge_next(seg);
    if(seg>segments)
	seg_try_merge_next(seg-1);
}

static struct segment *seg_create(addr,len,protection,mapped_fd,file_offset)
os_vm_address_t addr;
os_vm_size_t len;
int protection;
int mapped_fd;
{
    struct segment *seg=seg_create_nomerge(addr,len,protection,mapped_fd,file_offset);
    if(seg!=NULL)
	seg_try_merge_adjacent(seg);
    return seg;
}

/* 
 * Change the attributes of the given range of an existing segment, and return
 * a segment corresponding to the new bit.
 */
static struct segment *seg_change_range(seg,addr,len,protection,mapped_fd,file_offset)
struct segment *seg;
os_vm_address_t addr;
os_vm_size_t len;
int protection;
int mapped_fd;
{
    os_vm_address_t end=addr+len;

    if(len==0)
	return NULL;

    if(protection!=seg->protection
       || mapped_fd!=seg->mapped_fd
       || file_offset!=ADJ_OFFSET(seg->file_offset,addr-seg->start))
    {
	os_vm_size_t old_len=seg->length, seg_offset=(addr-seg->start);
	
	if(old_len<len+seg_offset){
	    struct segment *next=seg+1;
	    
#ifdef DEBUG
	    fprintf(stderr,
		    ";;; seg_change_range: region 0x%08x[0x%08x] overflows 0x%08x[0x%08x]\n",
		    addr,len,
		    seg->start,old_len);
#endif
	    
	    while(!seg_last_p(seg) && next->start+next->length<=end){
#ifdef DEBUG
		fprintf(stderr,
			";;; seg_change_range: merging extra segment 0x%08x[0x%08x]\n",
			next->start,
			next->length);
#endif
		seg_destroy(next);
	    }
	    
	    if(!seg_last_p(seg) && next->start<end){
		next->length-=end-next->start;
		next->start=end;
		old_len=next->start-seg->start;
	    }else
		old_len=len+seg_offset;
	    
#ifdef DEBUG
	    fprintf(stderr,
		    ";;; seg_change_range: extended first seg to 0x%08x[0x%08x]\n",
		    seg->start,
		    old_len);
#endif
	}

	if(seg_offset+len<old_len){
	    /* add second part of old segment */
	    seg_create_nomerge(end,
			       old_len-(seg_offset+len),
			       seg->protection,
			       seg->mapped_fd,
			       ADJ_OFFSET(seg->file_offset,seg_offset+len));

#ifdef DEBUG
	    fprintf(stderr,
		    ";;; seg_change_range: Split off end of 0x%08x[0x%08x]: 0x%08x[0x%08x]\n",
		    seg->start,old_len,
		    end,old_len-(seg_offset+len));
#endif
	}

	if(seg_offset==0){
	    seg->length=len;
	    seg->protection=protection;
	    seg->mapped_fd=mapped_fd;
	    seg->file_offset=file_offset;
	}else{
	    /* adjust first part of remaining old segment */
	    seg->length=seg_offset;

#ifdef DEBUG
	    fprintf(stderr,
		    ";;; seg_change_range: Split off beginning of 0x%08x[0x%08x]: 0x%08x[0x%08x]\n",
		    seg->start,old_len,
		    seg->start,seg_offset);
#endif

	    /* add new middle segment for new protected region */
	    seg=seg_create_nomerge(addr,len,protection,mapped_fd,file_offset);
	}

	seg_try_merge_adjacent(seg);

	last_fault=0;
    }

    return seg;
}

/* ---------------------------------------------------------------- */

static os_vm_address_t mapin(addr,len,protection,map_fd,offset,is_readable)
os_vm_address_t addr;
os_vm_size_t len;
int protection;
int map_fd;
long offset;
int is_readable;
{
    os_vm_address_t real;
    boolean sparse=(len>=SPARSE_BLOCK_SIZE);

    if(offset!=OFFSET_NONE
       && (offset<os_vm_page_size || (offset&(os_vm_page_size-1))!=0))
    {
	fprintf(stderr,
		"mapin: file offset (%d) not multiple of pagesize (%d)\n",
		offset,
		os_vm_page_size);
    }

    if(addr==NULL)
	len+=real_page_size_difference;	/* futz around to get an aligned region */

    last_fault=0;
    real=(os_vm_address_t)
	mmap((caddr_t)addr,
	     (long)len,
	     sparse ? (is_readable ? PROT_READ|PROT_EXEC : 0) : protection,
	     (addr==NULL? 0 : MAP_FIXED)|MAP_PRIVATE,
	     (is_readable || !sparse) ? map_fd : empty_fd,
	     (off_t)(offset==OFFSET_NONE ? 0 : offset));

    if((long)real==-1){
	perror("mapin: mmap");
	return NULL;
    }

    if(addr==NULL){
	/*
	 * now play around with what the os gave us to make it align by
	 * our standards (which is why we overallocated)
	 */
	os_vm_size_t overflow;

	addr=os_round_up_to_page(real);
	if(addr!=real)
	    munmap(real,addr-real);
	
	overflow=real_page_size_difference-(addr-real);
	if(overflow!=0)
	    munmap(addr+len-real_page_size_difference,overflow);

	real=addr;
    }


    return real;
}

static os_vm_address_t map_and_remember(addr,len,protection,map_fd,offset,is_readable)
os_vm_address_t addr;
os_vm_size_t len;
int protection;
int map_fd;
long offset;
int is_readable;
{
    os_vm_address_t real=mapin(addr,len,protection,map_fd,offset,is_readable);

    if(real!=NULL){
	struct segment *seg=seg_find(real);

	if(seg!=NULL)
	    seg=seg_change_range(seg,real,len,protection,map_fd,offset);
	else
	    seg=seg_create(real,len,protection,map_fd,offset);

	if(seg==NULL){
	    munmap(real,len);
	    return NULL;
	}
    }

#ifdef DEBUG
    fprintf(stderr,";;; map_and_remember: 0x%08x[0x%08x] offset: %d, mapped to: %d\n",
	    real,len,offset,map_fd);
#endif

    return real;
}

/* ---------------------------------------------------------------- */

os_vm_address_t os_validate(addr, len)
os_vm_address_t addr;
os_vm_size_t len;
{
    addr=os_trunc_to_page(addr);
    len=os_round_up_size_to_page(len);

#ifdef DEBUG
    fprintf(stderr, ";;; os_validate: 0x%08x[0x%08x]\n",addr,len);
#endif

    if(addr!=NULL && collides_with_seg_p(addr,len))
	return NULL;

    return map_and_remember(addr,len,PROT_DEFAULT,zero_fd,OFFSET_NONE,FALSE);
}

void os_invalidate(addr, len)
os_vm_address_t addr;
os_vm_size_t len;
{
    struct segment *seg=seg_find(addr);

    addr=os_trunc_to_page(addr);
    len=os_round_up_size_to_page(len);

#ifdef DEBUG
    fprintf(stderr, ";;; os_invalidate: 0x%08x[0x%08x]\n",addr,len);
#endif

    if(seg==NULL)
	fprintf(stderr, "os_invalidate: Unknown segment: 0x%08x[0x%08x]\n",addr,len);
    else{
	seg=seg_change_range(seg,addr,len,0,0,OFFSET_NONE);
	if(seg!=NULL)
	    seg_destroy(seg);

	last_fault=0;
	if(munmap(addr,len)!=0)
	    perror("os_invalidate: munmap");
    }
}

os_vm_address_t os_map(fd, offset, addr, len)
int fd;
int offset;
os_vm_address_t addr;
long len;
{
    addr=os_trunc_to_page(addr);
    len=os_round_up_size_to_page(len);

#ifdef DEBUG
    fprintf(stderr, ";;; os_map: 0x%08x[0x%08x]\n",addr,len);
#endif

    return map_and_remember(addr,len,PROT_DEFAULT,fd,offset,TRUE);
}

void os_flush_icache(address, length)
os_vm_address_t address;
os_vm_size_t length;
{
#if defined(MACH) && defined(mips)
	vm_machine_attribute_val_t flush;
	kern_return_t kr;

	flush = MATTR_VAL_ICACHE_FLUSH;

	kr = vm_machine_attribute(task_self(), address, length,
				  MATTR_CACHE, &flush);
	if (kr != KERN_SUCCESS)
		mach_error("Could not flush the instruction cache", kr);
#endif
}

void os_protect(addr, len, prot)
os_vm_address_t addr;
os_vm_size_t len;
int prot;
{
    struct segment *seg=seg_find(addr);

    addr=os_trunc_to_page(addr);
    len=os_round_up_size_to_page(len);

#ifdef DEBUG
    fprintf(stderr,";;; os_protect: 0x%08x[0x%08x]\n",addr,len);
#endif

    if(seg!=NULL){
	int old_prot=seg->protection;

	if(prot!=old_prot){
	    /*
	     * oooooh, sick: we have to make sure all the pages being protected have
	     * faulted in, so they're in a known state...
	     */
	    seg_force_resident(seg,addr,len);

	    seg_change_range(seg,addr,len,prot,seg->mapped_fd,seg->file_offset);

	    if(mprotect((caddr_t)addr,(long)len,prot)!=0)
		perror("os_unprotect: mprotect");
	}
    }else
	fprintf(stderr,"os_protect: Unknown segment: 0x%08x[0x%08x]\n",addr,len);
}

boolean valid_addr(test)
os_vm_address_t test;
{
    return seg_find(test)!=NULL;
}

/* ---------------------------------------------------------------- */

static boolean maybe_gc(sig, code, context)
int sig, code;
struct sigcontext *context;
{
    /*
     * It's necessary to enable recursive SEGVs, since the handle is
     * used for multiple things (e.g., both gc-trigger & faulting in pages).
     * We check against recursive gc's though...
     */

    boolean did_gc;
    static already_trying=0;

    if(already_trying)
	return FALSE;

    sigsetmask(context->sc_mask);

    already_trying=TRUE;
    did_gc=interrupt_maybe_gc(sig, code, context);
    already_trying=FALSE;

    return did_gc;
}

/*
 * The primary point of catching segmentation violations is to allow 
 * read only memory to be re-mapped with more permissions when a write
 * is attempted.  this greatly decreases the residency of the program
 * in swap space since read only areas don't take up room
 *
 * Running into the gc trigger page will also end up here...
 */
void segv_handler(sig, code, context, addr)
int sig, code;
struct sigcontext *context;
caddr_t addr;
{
    if (code == SEGV_PROT) {	/* allow writes to this chunk */
	struct segment *seg=seg_find(addr);

	if(last_fault==addr){
	    if(seg!=NULL && maybe_gc(sig, code, context))
		/* we just garbage collected */
		return;
	    else{
		/* a *real* protection fault */
		fprintf(stderr,
			"segv_handler: Real protection violation: 0x%08x\n",
			addr);
		interrupt_handle_now(sig,code,context);
	    }
	}else
	    last_fault=addr;

	if(seg!=NULL){
	    int err;
	    /* round down to a page */
	    os_vm_address_t block=(os_vm_address_t)((long)addr&~SPARSE_SIZE_MASK);
	    os_vm_size_t length=SPARSE_BLOCK_SIZE;

	    if(block < seg->start){
		length-=(seg->start - block);
		block=seg->start;
	    }
	    if(block+length > seg->start+seg->length)
		length=seg->start+seg->length-block;

#if 0
	    /* unmap it.  probably redundant. */
	    if(munmap(block,length) == -1)
		perror("segv_handler: munmap");
#endif

	    /* and remap it with more permissions */
	    err=(int)
		mmap(block,
		     length,
		     seg->protection,
		     MAP_PRIVATE|MAP_FIXED,
		     seg->mapped_fd,
		     seg->file_offset==OFFSET_NONE
		       ? 0
		       : seg->file_offset+(block-seg->start));

	    if (err == -1) {
		perror("segv_handler: mmap");
		interrupt_handle_now(sig,code,context);
	    }
	}
	else{
	    fprintf(stderr, "segv_handler: 0x%08x not in any segment\n",addr);
	    interrupt_handle_now(sig,code,context);
	}
    }
    /*
     * note that we check for a gc-trigger hit even if it's not a PROT error
     */
    else if(!maybe_gc(sig, code, context)){
	static int nomap_count=0;

	if(code==SEGV_NOMAP){
	    if(nomap_count==0){
		fprintf(stderr,
			"segv_handler: No mapping fault: 0x%08x\n",addr);
		nomap_count++;
	    }else{
		/*
		 * There should be higher-level protection against stack
		 * overflow somewhere, but at least this prevents infinite
		 * puking of error messages...
		 */
		fprintf(stderr,
			"segv_handler: Recursive no mapping fault (stack overflow?)\n");
		exit(-1);
	    }
	}else if(SEGV_CODE(code)==SEGV_OBJERR){
	    extern int errno;
	    errno=SEGV_ERRNO(code);
	    perror("segv_handler: Object error");
	}

	interrupt_handle_now(sig,code,context);

	if(code==SEGV_NOMAP)
	    nomap_count--;
    }
}

void os_install_interrupt_handlers()
{
    interrupt_install_low_level_handler(SIGSEGV,segv_handler);
}
