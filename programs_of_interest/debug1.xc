#include <platform.h>

#define PERIOD 10000000
out port cled0 = PORT_CLOCKLED_0; 
out port cled1 = PORT_CLOCKLED_1; 
out port cled2 = PORT_CLOCKLED_2; 
out port cled3 = PORT_CLOCKLED_3; 
out port cledG = PORT_CLOCKLED_SELG; 
out port cledR = PORT_CLOCKLED_SELR;


void tokenFlash(chanend left, chanend right, out port led, int delay, int isMaster) 
{
  
  timer tmr;
  unsigned t;
  int token; 
  unsigned isGreen;

  isGreen = 0;
  
  if (isMaster)    /* master inserts token into ring */
  {
      right <: 1;
      
  }
  
  while (1)
  {

      left :> token; //block until start

      led <: 0x10;  //turn on
      tmr :> t; 
      tmr when timerafter(t+delay) :> void; 

      led <: 0x20; 
      tmr :> t; 
      tmr when timerafter(t+delay) :> void; 


      led <: 0x40; 
      tmr :> t; 
      tmr when timerafter(t+delay) :> void; 


      led <: 0x0; //turn off
      right <: token; /* output token to right neigbour */
  }
}


int main(void)
{
    chan c0, c1, c2, c3; 
    par 
        {	 
            on stdcore [0]:	
            {
                tokenFlash(c0, c1, cled0, PERIOD, 1);
            }
            on stdcore[0]:
            {
                timer tmr;
                unsigned t;
                unsigned isGreen;
                isGreen = 0;
                
                cledR <: 1;
                while(1)
                    {
                        tmr when timerafter(t+12*PERIOD) :> void;
                    tmr :> t;
                        cledG <: isGreen ? 1 : 0;
                        cledR <: isGreen ? 0 : 1;
                        
                        
                        isGreen = ! isGreen;
                    }
                
            }
            on stdcore[1]: tokenFlash(c1, c2, cled1, PERIOD, 0);
            on stdcore[2]: tokenFlash(c2, c3, cled2, PERIOD, 0);
            on stdcore[3]: tokenFlash(c3, c0, cled3, PERIOD, 0);
        } 
    return 0;
}
