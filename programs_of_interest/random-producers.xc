/*
  Paul Nathan 2011.
  Random-producers.xc
  Produced as part of Master's Thesis work at the University of Idaho.

  This code is intended to provide a non-deterministic interleaving of
  execution traces.

  Two cores are producers, one core is a consumer.
  
  The two producers produce, randomly sleep for a while, then produce again.

  The consumer cycles through both the channels.

 */
#include <platform.h>
#include <stdlib.h>
#include <stdio.h>
//Each producer sends out this many tokens
// 0xf chosen because it's easy to read in the trace
#define TOKENS_FROM_A_PRODUCER 0xf
//And we get 2 x the tokens
#define TOKENS_GOTTEN 2 * TOKENS_FROM_A_PRODUCER


int main(void)
{
    chan queue1;
    chan queue2;

    par
    {
        //Core 0 is Producer 1
        on stdcore[0]:
            {
                int outpacket = 0;
		srand(0);		
                for(int i = 0; i < TOKENS_FROM_A_PRODUCER; i++)
                {
                    queue1 <: outpacket;
                    outpacket++;
                }
            }
        //Core 1 is Producer2
        on stdcore[1]:
            {
                //starts at an easy-to-read point
                unsigned int outpacket = 0xf0;
		srand(0);
                for(int i = 0; i < TOKENS_FROM_A_PRODUCER; i++)
                {
                    queue2 <: outpacket;
                    outpacket++;
                }
            }
        //Core 2 is the consumer.
        on stdcore[2]:
            {

                int packet;
                select {
                case slave { queue1 :> packet; } :
                break;
                case slave { queue2 :> packet; }:
                break;
                }
                
            }
    }
    return 0;
}
