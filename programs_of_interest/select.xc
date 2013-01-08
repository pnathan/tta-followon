/*
Paul Nathan 2011: select.xc
Produced as part of Master's Thesis work at the University of Idaho.

Used to determine how select works
*/
#include <platform.h>
#include <stdlib.h>
#include <stdio.h>

#define STARTTRACE(core) asm("starttrace" core ":");
#define ENDTRACE(core) asm("endtrace" core ":");

/*

A timer has a 32-bit counter that is continually incremented at a rate
of 100MHz and whose value can be input at any time. An input on a
timer can also be delayed until a time in the future.

--Programming XC on XMOS Devices, section 2.4

 */
void sleep(int delay)
{
  int time;
  timer t;			/* Ticks at 10 ns intervals */
 t :> time;			/* gets the time */
  time += delay * 100;     	
  t when timerafter(time) :> void;
}
int main(void)
{
  chan queue1, queue2;
  par
    {
      on stdcore[0]:
      {
	int packet;
	int rxsum = 0;
	STARTTRACE("0");
	for (int i = 0 ; i < 5; i++)
	  {
	    select
	      {
	      
	      case queue1 :> packet:
		{
		  rxsum += packet;
		  break;
		}
		/*case queue2 :> packet:
		{
		  rxsum += packet;
		  break;
		}*/
	      }
	  }


	ENDTRACE("0");
      }
      on stdcore[1]:
      {
	srand(1);
	
	STARTTRACE("1");
	for(int i = 0; i < 5; i++)
	  {
	    sleep(rand() % 20);
	    queue1 <: i;
	  }
	ENDTRACE("1");
      }
      /*on stdcore[2]:
      {

	srand(2);
 
	STARTTRACE("2");
	for(int i = 10; i < 15; i++)
	  {
	    sleep(rand() % 20);
	    queue2 <: i;
	  }
	ENDTRACE("2");
      }
      */
    }
    return 0;
}
