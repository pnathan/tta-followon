/*
cpu.h

Definitions and constants that relate to the CPU and JTAG.

Paul Nathan 2010
Master's Thesis code.

 */
#ifndef CPU
#define CPU


//#define WIN32TESTING


//Based on the ABI
typedef unsigned int uint32;
//the instruction word is 16 bits. Some instructions have 2 words
typedef unsigned short uint16;  
typedef unsigned char uint8;
typedef unsigned short bool;
#define false 0
#define true 1

////////////////////////////////////////
// Constants applying to the XS architecture.
// Drawn from the manuals and from personal communications.
////////////////////////////////////////

//Constants for the JTAG addressing.
#define DBG_SR      0x10 
#define DBG_PC      0x11
#define DBG_SP      0x12
#define DBG_RES_ID  0x13
#define DBG_REG_NUM 0x14

//resource IDs for the above; pursuant to the algorithm given in section 9.1
#define REG_TO_ID(a) ((a << 8) | 0x0B)

#define SR_RID REG_TO_ID(DBG_SR)
#define PC_RID REG_TO_ID(DBG_PC)
#define SP_RID REG_TO_ID(DBG_SP)
#define RES_RID REG_TO_ID(DBG_RES_ID)
#define REG_RID REG_TO_ID(DBG_REG_NUM)

//sourced from pg 105 in xs1_en.pdf: 2009-10-19 release
#define RES_TYPE_FOR_THREAD 0x4
#define RES_FOR_THREAD(a) ((a << 24) | RES_TYPE_FOR_THREAD)

////////////////////////////////////////
//Code/Data definitions.

//Structure containing CPU state.
struct cpu_state_type
{
    uint32 r[12];
    //program counter
    uint32 pc;
    uint32 lr;
    uint32 cp;
    uint32 sp;
};

//A bundle used for instruction status
struct status_type
{
    uint32 pc;
    uint32 lr;
    uint32 sp;
    bool   is_longword;
    bool   was_break;
};


//Print using the serial port.
void print_out_cpu(struct cpu_state_type *c);

//Initialize the CPU state to -1 for every register
void init_cpu_state(struct cpu_state_type *c);


#endif
