/*
  Paul Nathan 2011.
  one-transfer.xc
  Produced as part of Master's Thesis work at the University of Idaho.

This code is intended to determine if START/STOP trace could be inserted before channels were created in the ASsembly.
 */
#include <platform.h>
#include <stdlib.h>
#include <stdio.h>

#define STARTTRACE(core) asm("starttrace" core ":");
#define ENDTRACE(core) asm("endtrace" core ":");

int main(void)
{

  asm("FOO:")
  //STARTTRACE("X");
  chan queue1;
  //ENDTRACE("X");
  par
    {
        on stdcore[0]:
            {
                int outpacket = 0xf0;
		STARTTRACE("0");	
		queue1 <: outpacket;
	        queue1 :> outpacket;
		
		ENDTRACE("0");
            }
        on stdcore[1]:
            {
                int inpacket;
		
		STARTTRACE("1");
		queue1 :> inpacket;
		queue1 <: inpacket + 1;
		ENDTRACE("1");
            }
    }
    return 0;
}
