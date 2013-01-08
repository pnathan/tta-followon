/*
debug_handler.c
Routine for the debug handler

Paul Nathan 2010
Master's Thesis code.

This file is compiled into an assembly file.  prepare_debug_handler.pl
modifies the assembly file into something that will directly run.

This is needed because the default asm generation creates some stack
manipulation instructions that intefere with debug operation.
 */

// 0x10 PS DBG SSR DRW Saved SR for debug interrupts
// 0x11 PS DBG SPC DRW Saved PC for debug interrupts
// 0x12 PS DBG SSP DRW Stores the stack pointer during debug interrupts
// 0x13 PS DBG T NUM DRW The resource ID of the thread whose state is to be read.
// 0x14 PS DBG T REG DRW Register number to be accessed by DGETREG.

#include "cpu.h"
#include "xs1-interface.h"
#include "io.h"
#include "parse_inst.h"
extern void c_debug();
extern struct cpu_state_type cpu_state;

extern int breaking;
extern int firstbreak;
extern int endbreak;

int counter = 0;

void debug_handler()
{
    //////////////IMPORTANT!!!!
    //in generated asm, comment out entsp & other stack manip instructions
    //////////////

    uint16 *pc;
    struct status_type new_state;
    uint32 new_pc;

    uint16 pc0;
    uint16 pc1;

    //Pull regs into the cpu storage
    asm("ldw r0, sp[5]");
    asm("mov %0, r0" : "=r"(cpu_state.r[0]));
    asm("mov %0, r1" : "=r"(cpu_state.r[1]));
    asm("mov %0, r2" : "=r"(cpu_state.r[2]));
    asm("mov %0, r3" : "=r"(cpu_state.r[3]));
    asm("mov %0, r4" : "=r"(cpu_state.r[4]));
    asm("mov %0, r5" : "=r"(cpu_state.r[5]));
    asm("mov %0, r6" : "=r"(cpu_state.r[6]));
    asm("mov %0, r7" : "=r"(cpu_state.r[7]));
    asm("mov %0, r8" : "=r"(cpu_state.r[8]));
    asm("mov %0, r9" : "=r"(cpu_state.r[9]));
    asm("mov %0, r10" : "=r"(cpu_state.r[10]));
    asm("mov %0, r11" : "=r"(cpu_state.r[11]));
    //    asm("mov %0, sp" : "=r"(cpu_state.sp));
    //    asm("mov %0, lr" : "=r"(cpu_state.lr));

    // -- Configure to return to the correct instruction
    //Load program counter address
    asm("ldc r0, %0" : : "i"(PC_RID));
    //Read in from the address
    asm("get r1, ps[r0]");
    //copy the PC address to the pc variable
    asm("mov %0, r1" : "=r"(pc));

    new_state = next_addr(pc, &cpu_state);

    new_pc = new_state.pc;

    if(firstbreak)
    {
	firstbreak = 0;
	//Define a breakpoint setup.
	// write_proc_state(0x40, ((1 << thread) << 16) | 3);    // ibreak
	//setps((0x40 << 8) | 0xB, (1 << thread) << 16 | 3)
	//setps(0x400B, 0x10003);
	pn_setps(IBREAK_ENABLE_REG, 0x10003);
	asm("nop");

	say_s("ibreak enabled\n");
    }


    if(endbreak)
    {
	//define the breakpoint to be at 0x1
	pn_setps(IBREAK_PC_REG, 0x1);
	asm("nop");

	pn_setps(IBREAK_ENABLE_REG, 0x0);
	asm("nop");

	say_s("ibreak disabled\n");
    }

    if(breaking)
    {
	//  write_proc_state(0x30, pc);
	//setps((0x30 << 8) | 0xB, pc)
	//setps(0x300B, pc);

	if(new_state.was_break == true)
	{
	    if(new_state.is_longword == false)
	    {
		pn_setps(IBREAK_PC_REG, new_pc - 2);
	    }
	    else
	    {
		pn_setps(IBREAK_PC_REG, new_pc - 4);
	    }
	}


	asm("nop");

	put_num(counter);
	put_s(" : ");
	say_num(new_pc);
	counter++;
    }

    /*    cpu_state.pc = new_state.pc;
	  cpu_state.lr = new_state.lr;
	  cpu_state.sp = new_state.sp;
    */
    asm("#Finished incrementing the PC");
    //Load r1 with the new PC value
    asm("mov r1, %0" :: "r"(new_pc) : "r1");
    //R0 got clobbered
    asm("ldc r0, %0" : : "i"(PC_RID));
    //write it back
    asm("set ps[r0], r1");
    //delay cycle for the set ps.
    asm("nop");



    //at this point, we should be able to execute any debug code freely.
    //c_debug(&cpu_state);

    //Restore the CPU state
    //Restore LR, SP
    //    asm("mov lr, %0" :: "r"(cpu_state.lr) : "lr");
    //    asm("mov sp, %0" :: "r"(cpu_state.sp) : "sp");
    asm("mov r11, %0" :: "r"(cpu_state.r[11]) : "r11");
    asm("mov r10, %0" :: "r"(cpu_state.r[10]) : "r10");
    asm("mov r9, %0" :: "r"(cpu_state.r[9]) : "r9");
    asm("mov r8, %0" :: "r"(cpu_state.r[8]) : "r8");
    asm("mov r7, %0" :: "r"(cpu_state.r[7]) : "r7");
    asm("mov r6, %0" :: "r"(cpu_state.r[6]) : "r6");
    asm("mov r5, %0" :: "r"(cpu_state.r[5]) : "r5");
    asm("mov r4, %0" :: "r"(cpu_state.r[4]) : "r4");
    asm("mov r3, %0" :: "r"(cpu_state.r[3]) : "r3");
    asm("mov r2, %0" :: "r"(cpu_state.r[2]) : "r2");
    asm("mov r1, %0" :: "r"(cpu_state.r[1]) : "r1");
    //Internal saving on the dcall instruction.
    asm("ldw r0, sp[5]");
    asm("drestsp");
    asm("dret");

    //in generated asm, comment out retsp
}
