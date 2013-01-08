/*
Paul Nathan 2010
Master's Thesis code.


This file tests building a debug handler for the XC1A XCORE

Written after consultation with Henk Muller of XMOS.

Serial settings: 115.2K, 8, N, 1

 */

#include "cpu.h"
#include "io.h"
#include "itoa.h"
#include "xs1-interface.h"
#include "debug_lib.h"
//System libs: put after non-system includes.
#include <print.h> //puts data out through the jtag - use --io on xrun
#include <xs1.h>


//Address to the JTAG debugger.
#define JTAG_DEBUGGER 0xffffd81a

//found in debug_handler.c
extern void debug_handler();

//Found in cpu.c
struct cpu_state_type cpu_state;

//Are we single-stepping?
int breaking;
int firstbreak;
int endbreak;

void c_debug();

void mock()
{
    //declaration block----------------
    int i;
   
    //set up some dummy registers to examine the values from
    //    start_singlestep();
    asm("ldc r0, 2");
    //foo();
    asm("ldc r1, 3");
    for( i = 0; i < 3; i++)
    {
	asm("ldc r2, 4");
	asm("ldc r3, 5");
	asm("ldc r4, 6");
	asm("ldc r5, 7");
	asm("ldc r6, 8");
	asm("ldc r7, 9");
    }
    asm("ldc r8, 10");
    asm("ldc r9, 11");
    asm("ldc r10, 12");
    asm("ldc r11, 13");
    //say_s("end");
    //stop_singlestep();
}


void turn_ss_on()
{
  breaking = 128;
  firstbreak = 128;
}

void turn_ss_off()
{
  breaking = 0;
  endbreak = 128;
}


//Any C logic that should be used is done here.
void c_debug()
{
  //  put_s("At: ");
  //  say_num(cpu_state.pc);
}


//Initialize, deinitialize the debugger
void initialize_debugger()
{  

    breaking = firstbreak = endbreak = 0;

    //Function pointer
    void (*dfunction)() = 0;
    
    
    //Address of debug_handler function
    dfunction = &debug_handler;
    
    init_cpu_state(&cpu_state);
    
    //Install the debugger
    
    //Core 0, register 0x20 holds the debug handler address.
    write_pswitch_reg(0x0, 0x20, (unsigned int)dfunction);
}

void close_debugger()
{
    breaking = firstbreak = endbreak = 0;

    //Reset it so that life is happy!
    write_pswitch_reg(0x0, 0x20, JTAG_DEBUGGER);
}


