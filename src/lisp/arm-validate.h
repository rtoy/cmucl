/*
 * This code was written as part of the CMUCL project and has been
 * placed in the public domain.
 */

#ifndef ARM_VALIDATE_H
#define ARM_VALIDATE_H

/*
 * Address map:
 *
 *      0x0f000000->0xf8000000    8M for linkage table area
 *      0x10000000->0x14000000   64M Read-Only Space.
 *      0x20000000->0x22000000   32M Binding stack growing up.
 *      0x28000000->0x2a000000   32M Control stack growing down.
 *      0x30000000->0x34000000   64M Static Space.
 *      0x40000000->0x60000000  512M Dynamic space 1
 *      0x60000000->0x80000000  512M Dynamic space 2
 *
 */

/* TODO: put this in a common location like validate.h */
#define MB      (1024*1024)
;
#ifdef LINKAGE_TABLE
/*
 * This space start better match the value of
 * target-foreign-linkage-space-start defined in arm/parms.lisp!
 */

#define FOREIGN_LINKAGE_SPACE_START (LinkageSpaceStart)

/*
 * This allows for about 510K symbols (assuming each entry is 16 bytes
 * long).  Hope that's enough!  Make sure this doesn't overlap the
 * READ_ONLY_SPACE_START!
 */
#define FOREIGN_LINKAGE_SPACE_SIZE  (8*MB)
#endif

#define READ_ONLY_SPACE_START   (SpaceStart_TargetReadOnly)
#define READ_ONLY_SPACE_SIZE    (64*MB)

#define BINDING_STACK_START     (0x20000000)
#define BINDING_STACK_SIZE      (32*MB)

#define STATIC_SPACE_START      (SpaceStart_TargetStatic)
#define STATIC_SPACE_SIZE       (64*MB)

#define CONTROL_STACK_START     (0x28000000)
#define CONTROL_STACK_SIZE      (32*MB)
#define CONTROL_STACK_END       (CONTROL_STACK_START + control_stack_size)

#define SIGNAL_STACK_SIZE	SIGSTKSZ

/* The default dynamic space to allocate */
#define DEFAULT_DYNAMIC_SPACE_SIZE  (128*MB)

/* The maximum dynamic space that we can allocate */
#define DYNAMIC_SPACE_SIZE      (512*MB)

#ifdef GENCGC
#error gencgc not supported
#else
#define DYNAMIC_0_SPACE_START   (SpaceStart_TargetDynamic)
#define DYNAMIC_1_SPACE_START   (DYNAMIC_0_SPACE_START + DYNAMIC_SPACE_SIZE)
#endif

#endif /* ARM_VALIDATE_H */
