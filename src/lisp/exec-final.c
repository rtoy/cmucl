/*
 * Variables for executable support.
 *
 * $Header: /Volumes/share2/src/cmucl/cvs2git/cvsroot/src/lisp/exec-final.c,v 1.1 2010/07/29 01:51:12 rtoy Rel $
 *
 */

/*
 * See lisp.c for documentation, but basically this is linked in for
 * Solaris executables to indicate that the image is builtin.  We must
 * not define initial_function_addr here because the linker script
 * will set the value appropriately.
 */
int builtin_image_flag = 1;

