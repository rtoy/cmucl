#include <stdio.h>
#include <signal.h>

#include "ldb.h"
#include "lisp.h"
#include "os.h"
#include "lispregs.h"
#include "globals.h"

#ifdef ibmrt
typedef unsigned short inst;
#else
typedef unsigned long inst;
#endif


static inst swap_insts(code_obj, pc_offset, new_inst)
     lispobj code_obj;
     int pc_offset;
     inst new_inst;
{
    struct code *code;
    inst *addr, old_inst;

    code = (struct code *)PTR(code_obj);
    addr = (inst *)((char *)code + HeaderValue(code->header)*sizeof(lispobj)
		    + pc_offset);
    old_inst = *addr;
    *addr = new_inst;
    os_flush_icache((os_vm_address_t)addr, sizeof(inst));
    return old_inst;
}

inst breakpoint_install(code_obj, pc_offset)
     lispobj code_obj;
     int pc_offset;
{
    return swap_insts(code_obj, pc_offset, (trap_Breakpoint << 16) | 0xd);
}

void breakpoint_remove(code_obj, pc_offset, orig_inst)
     lispobj code_obj;
     int pc_offset;
     inst orig_inst;
{
    swap_insts(code_obj, pc_offset, orig_inst);
}

static int calc_offset(code, pc)
     struct code *code;
     unsigned long pc;
{
    unsigned long code_start;
    int offset;

    code_start = (unsigned long)code
	+ HeaderValue(code->header)*sizeof(lispobj);
    if (pc < code_start)
	return 0;
    offset = pc - code_start;
    if (offset >= code->code_size)
	return 0;
    return offset;
}

int breakpoint_after_offset(scp)
     struct sigcontext *scp;
{
    struct code *code = (struct code *)PTR(scp->sc_regs[CODE]);

#ifdef sparc
    return calc_offset(code, scp->sc_npc);
#endif

#ifdef mips
    inst cur_inst = *(inst *)scp->sc_pc;
    int opcode = cur_inst >> 26;
    struct sigcontext tmp;

    if (opcode == 1 || ((opcode & 0x3c) == 0x4) || ((cur_inst & 0xf00e0000) == 0x80000000)) {
	tmp = *scp;
	emulate_branch(&tmp, cur_inst);
	return calc_offset(code, tmp.sc_pc);
    }
    else
	return calc_offset(code, scp->sc_pc + 4);
#endif
}


handle_breakpoint(signal, subcode, scp)
     struct sigcontext *scp;
{
    struct code *code = (struct code *)PTR(scp->sc_regs[CODE]);
    lispobj *args;

    args = current_control_stack_pointer;
    current_control_stack_pointer += 3;
    args[0] = fixnum(calc_offset(code, scp->sc_pc));
    args[1] = scp->sc_regs[CODE];
    args[2] = alloc_sap(scp);
    call_into_lisp(HANDLE_BREAKPOINT, SymbolFunction(HANDLE_BREAKPOINT),
		   args, 3);
    scp->sc_mask = sigblock(0);
}
