/*
  Paul Nathan 2011: transaction.xc
  Produced as part of Master's Thesis work at the University of Idaho.

  This code is intended to run a transaction.
*/
#include <platform.h>
#include <stdlib.h>
#include <stdio.h>

#define STARTTRACE(core) asm("starttrace" core ":");
#define ENDTRACE(core) asm("endtrace" core ":");

int main(void) {
    chan queue1;
    par {
        on stdcore[0]: master {
                int outpacket = 0x0f;

		STARTTRACE("0");
		queue1 <: outpacket;
	        queue1 <: outpacket + 1;
	        queue1 <: outpacket + 2;
		ENDTRACE("0");
            }
        on stdcore[1]: slave {
                int inpacket;

		STARTTRACE("1");
		queue1 :> inpacket;
		queue1 :> inpacket;
	        queue1 :> inpacket;
		ENDTRACE("1");
            }
    }
    return 0;
}
