/*
io.c
C-level routines for input and output

Paul Nathan 2010
Master's Thesis code.

put_s, get_s put and get a string into a preallocated buffer. 
say_s appends a \n\r to the output


Beware buffer overruns.
 */
#include "io.h"
#include "itoa.h"

#ifndef SIM

void say_s(char s[])
{
    put_s(s);
    put_s("\n\r");
}

void put_num(int n)
{
  char buf[9];
  itoa16(n, buf);
  put_s(buf);
}

void say_num(int n)
{
  char buf[9];
  itoa16(n, buf);
  say_s(buf);
}

void get_s(char s[])
{
    int i = 0;
    char c;
    do
    {
	c = get_c();
	s[i++] = c;
    }while(c != '\r');

    //strip the trailing \r
    i = 0;
    while(*s++)
    {
	if(*s == '\r')
	    *s = 0;
	if(*s == '\n')
	    *s = 0;
    }						

    //Now doubly null-terminated
}

#endif
