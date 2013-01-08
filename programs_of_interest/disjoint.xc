/*
  Paul Nathan 2011: disjoint.xc
  Produced as part of Master's Thesis work at the University of Idaho.

  This code demonstrates two different channels in communication
 */
#include <platform.h>
#include <stdlib.h>
#include <stdio.h>

#define STARTTRACE(core) asm("starttrace" core ":");
#define ENDTRACE(core) asm("endtrace" core ":");

int main(void) {
    chan queue1;
    chan queue2;
    par {
        on stdcore[0]: {
                int outpacket = 0xf0;

		STARTTRACE("0");
		queue1 <: outpacket;
	        queue1 :> outpacket;		
		ENDTRACE("0");
            }
        on stdcore[1]: {
                int inpacket;
		
		STARTTRACE("1");
		queue1 :> inpacket;
		queue1 <: inpacket + 1;
		ENDTRACE("1");
            }

        on stdcore[2]: {
                int outpacket = 0x05;

		STARTTRACE("2");
		queue2 <: outpacket;
	        queue2 :> outpacket;		
		ENDTRACE("2");
            }
        on stdcore[3]: {
                int inpacket;
		
		STARTTRACE("3");
		queue2 :> inpacket;
		queue2 <: inpacket + 1;
		ENDTRACE("3");
            }
    }
    return 0;
}
