/*
io.h

Paul Nathan 2010
Master's Thesis code.

String input and output. Currently implemented against the serial port.

Serial settings: 115.2K, 8, N, 1

put_c puts a character to the serial port
put_s puts a string to the serial port
say_s puts a string to the serial port, appended with \r\n

get_c gets 1 character from the serial port
get_s gets a string from the serial port

Note that the string routines work on statically allocated buffers. 
 */
#ifndef IO_H
#define IO_H

#include "io-base.h"

#ifndef SIM

void say_s(char s[]);
void get_s(char s[]);
void put_num(int n);
void say_num(int n);

#else

#define say_s(a)
#define get_s(a)
#define put_num(a)
#define say_num(a)

#endif

#endif
