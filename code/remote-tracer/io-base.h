/*
io-base.h
Definitions for the XC functions

Paul Nathan 2010
Master's Thesis code.
*/
#ifndef IO_BASE_H
#define IO_BASE_H

//When defined, this turns off serial data IO.
#define SIM

//Only use these functions if we are not in sim mode
#ifndef SIM

void put_s(char s[]);
void put_c(char c);
char get_c();

#else

#define put_s(a)
#define put_c(a)
#define get_c()

#endif

#endif
 
