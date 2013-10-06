/*
 * Variables for executable support.
 *
 */

/*
 * See lisp.c for documentation, but basically this is linked in for
 * executables to indicate that the image is builtin.  We must not
 * define initial_function_addr here because the linker script will
 * set the value appropriately.
 */
int builtin_image_flag = 1;

