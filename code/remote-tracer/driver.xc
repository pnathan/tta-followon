/*
Driver.xc.

Paul Nathan 2010 Master's Thesis code

Demo application for the debugger.


*/

#include "io.h"
#include "debug_lib.h"

int main()
{
    say_s("Hello, very small world!"); 
    initialize_debugger();
    

    start_singlestep()
    asm("ldc r0, 2");    
    asm("ldc r0, 3");		
    //mock();
    stop_singlestep();
    close_debugger();
    say_s("Goodbye, very small world!"); 
   
//    while(1)
//	;
    return 1;
}
