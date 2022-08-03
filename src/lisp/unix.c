/*
 * C interfaces to unix syscalls
 */

#include <stdio.h>
#include <sys/stat.h>

int unix_stat(const char* path, dev_t *dev, ino_t *ino, mode_t *mode, nlink_t *nlink,
              uid_t *uid, gid_t *gid, dev_t *rdev, off_t *size,
              struct timespec *atime, struct timespec *mtime, struct timespec *ctime,
              long *blksize, long *blocks)
{
    int rc;
    struct stat buf;

    rc = stat(path, &buf);

    fprintf(stderr, "size dev %d\n", sizeof(buf.st_dev));
    fprintf(stderr, "size ino %d\n", sizeof(buf.st_ino));
    fprintf(stderr, "size mode %d\n", sizeof(buf.st_mode));
    fprintf(stderr, "size nlink %d\n", sizeof(buf.st_nlink));
    fprintf(stderr, "size uid %d\n", sizeof(buf.st_uid));
    fprintf(stderr, "size gid %d\n", sizeof(buf.st_gid));
    fprintf(stderr, "size rdev %d\n", sizeof(buf.st_rdev));
    fprintf(stderr, "size size %d\n", sizeof(buf.st_size));
    fprintf(stderr, "size atime %d\n", sizeof(buf.st_atime));
    fprintf(stderr, "size mtime %d\n", sizeof(buf.st_mtime));
    fprintf(stderr, "size ctime %d\n", sizeof(buf.st_ctime));
    fprintf(stderr, "size blksize %d\n", sizeof(buf.st_blksize));
    fprintf(stderr, "size blocks %d\n", sizeof(buf.st_blocks));
    
    
    *dev = buf.st_dev;
    *ino = buf.st_ino;
    *mode = buf.st_mode;
    *nlink = buf.st_nlink;
    *uid = buf.st_uid;
    *gid = buf.st_gid;
    *rdev = buf.st_rdev;
    *size = buf.st_size;
    *atime = buf.st_atim;
    *mtime = buf.st_mtim;
    *ctime = buf.st_ctim;
    *blksize = buf.st_blksize;
    *blocks = buf.st_blocks;

    return rc;
}

int unix_fstat(int fd, dev_t *dev, ino_t *ino, mode_t *mode, nlink_t *nlink,
               uid_t *uid, gid_t *gid, dev_t *rdev, off_t *size,
               struct timespec *atime, struct timespec *mtime, struct timespec *ctime,
               long *blksize, long *blocks)
{
    int rc;
    struct stat buf;

    rc = fstat(fd, &buf);

    *dev = buf.st_dev;
    *ino = buf.st_ino;
    *mode = buf.st_mode;
    *nlink = buf.st_nlink;
    *uid = buf.st_uid;
    *gid = buf.st_gid;
    *rdev = buf.st_rdev;
    *size = buf.st_size;
    *atime = buf.st_atim;
    *mtime = buf.st_mtim;
    *ctime = buf.st_ctim;
    *blksize = buf.st_blksize;
    *blocks = buf.st_blocks;

    return rc;
}

int unix_lstat(const char* path, dev_t *dev, ino_t *ino, mode_t *mode, nlink_t *nlink,
               uid_t *uid, gid_t *gid, dev_t *rdev, off_t *size,
               struct timespec *atime, struct timespec *mtime, struct timespec *ctime,
               long *blksize, long *blocks)
{
    int rc;
    struct stat buf;

    rc = lstat(path, &buf);

    *dev = buf.st_dev;
    *ino = buf.st_ino;
    *mode = buf.st_mode;
    *nlink = buf.st_nlink;
    *uid = buf.st_uid;
    *gid = buf.st_gid;
    *rdev = buf.st_rdev;
    *size = buf.st_size;
    *atime = buf.st_atim;
    *mtime = buf.st_mtim;
    *ctime = buf.st_ctim;
    *blksize = buf.st_blksize;
    *blocks = buf.st_blocks;

    return rc;
}
    
    
  
