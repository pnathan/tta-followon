/*
  Paul Nathan 2011. :simple-p-c.xc
  Produced as part of Master's Thesis work at the University of Idaho.

  This code is intended to provide a simple producer-consumer setup

  One core produces, another consumes.   
 */
#include <platform.h>
#include <stdlib.h>
#include <stdio.h>

//Each producer sends out this many tokens
// 0xf chosen because it's easy to read in the trace
#define TOKENS_FROM_A_PRODUCER 0xf
//And we get this many tokens
#define TOKENS_GOTTEN TOKENS_FROM_A_PRODUCER

#define STARTTRACE(core) asm("starttrace" core ":");
#define ENDTRACE(core) asm("endtrace" core ":");

int main(void) {
    chan queue1;
    par {        
        on stdcore[0]: {         //Core 0 is producer.
                int outpacket = 0xffff;
		STARTTRACE("0");
                for(int i = 0; i < TOKENS_FROM_A_PRODUCER; i++) {
                    queue1 <: outpacket;
                    outpacket++;
                }
		ENDTRACE("0");
            }
        on stdcore[1]: {         //Core 1 is the consumer.
                int inpacket;
		
		STARTTRACE("1");

		for (int i = 0 ; i < TOKENS_GOTTEN; i++) {
		    queue1 :> inpacket;
		}
		ENDTRACE("1");
            }
    }
    return 0;
}
