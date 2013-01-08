/*
cpu.c 

routines for managing the CPU and the JTAG

Paul Nathan 2010
Master's Thesis code.

 */
#include "cpu.h"
#include "itoa.h"
#include "io.h"
/*

CPU State looks like this:

struct cpu_state_type
{
  int r[12];
};

*/

void print_out_cpu(struct cpu_state_type *c)
{
    int i;
    //8 hex digits in 32-bit words + 1 null makes 9 characters    
    char buf[9];
    say_s("CPU state");
    for( i = 0; i < 12; i++)
    {
	itoa16(c->r[i], buf);
	say_s(buf);
    }
    say_s("---");
}


void init_cpu_state(struct cpu_state_type * c)
{
    int i;
    for( i = 0; i < 12; i++)
    {
	c->r[i] = -1;
    }
}
