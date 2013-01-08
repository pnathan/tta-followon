/*
  Paul Nathan 2011: multi-transfer.xc
  Produced as part of Master's Thesis work at the University of Idaho.

  This code is intended to run some exchanges in a fairly tricky set
  of patterns. 
*/
#include <platform.h>
#include <stdlib.h>
#include <stdio.h>

#define STARTTRACE(core) asm("starttrace" core ":");
#define ENDTRACE(core) asm("endtrace" core ":");

int main(void) {
    chan queue;
    par {
        on stdcore[0]: {
                int packet = 0xf;
		STARTTRACE("0");
		queue <: packet;		// out
	        queue :> packet;		// in
		queue <: packet + 1;		// out
		queue <: packet + 2;		// out
	        queue :> packet;		// in
		ENDTRACE("0");
            }
        on stdcore[1]: {
                int packet;
		
		STARTTRACE("1");
		queue :> packet;		// in
		queue <: packet + 1;		// out
		queue :> packet;   		// in
		queue :> packet;   		// in, 2
		queue <: packet + 8;		// out
		ENDTRACE("1");
            }
    }
    return 0;
}
