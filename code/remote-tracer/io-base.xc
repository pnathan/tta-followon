/*
io.xc
XC routines for input output

Paul Nathan 2010
Master's Thesis code.


Serial settings: 115.2K, 8, N, 1

txByte transmits 1 byte across the TX line
rxByte recieves 1 byte across the RX line

put_c and get_c provide an independant IO.

 */
//from the tutorial
#include <platform.h>
//#include "io-base.h"


#define BIT_RATE 115200
#define BIT_TIME XS1_TIMER_HZ / BIT_RATE

out port serial_tx = PORT_UART_TX;
in port serial_rx = PORT_UART_RX;

void txByte (out port TXD , int byte ) 
{
    unsigned time ;
    timer t;
    /* input initial time */
t :> time ;
/* output start bit */
    TXD <: 0;
    time += BIT_TIME ;
    t when timerafter ( time ) :> void ;
/* output data bits */
    for ( int i=0; i <8; i ++) {
	TXD <: >> byte ;
	time += BIT_TIME ;
	t when timerafter ( time ) :> void ;
    }
/* output stop bit */
    TXD <: 1;
    time += BIT_TIME ;
    t when timerafter ( time ) :> void ;
}

//Final byte needs to be null.
void tx_byte_stream (out port TXD , char byte[] ) 
{
    unsigned time ;
    timer t;
    int c;
    int index = 0;
    while(byte[index])
    {
	c = byte[index];

	/* input initial time */
	t :> time ;
	/* output start bit */
    	TXD <: 0;
    	time += BIT_TIME ;
    	t when timerafter ( time ) :> void ;
	/* output data bits */
    	for ( int i=0; i <8; i ++) 
	{
		TXD <: >> c;
		time += BIT_TIME ;
		t when timerafter ( time ) :> void ;
    	}
	/* output stop bit */
    	TXD <: 1;
    	time += BIT_TIME ;
    	t when timerafter ( time ) :> void ;

	index++;
   }   
}



char rxByte (in port RXD) 
{
    unsigned byte , time ;
    timer t;
    char ret_val;
/* wait for start bit */
    RXD when pinseq (0) :> void ;
t :> time ;
    time += BIT_TIME /2;
    
/* input data bits */
    for ( int i=0; i <8; i ++) 
    {
	time += BIT_TIME ;
	t when timerafter ( time ) :> void ;	    
    RXD :> >> byte ;
    }
    
/* input stop bit */
    time += BIT_TIME ;
    t when timerafter ( time ) :> void ;
RXD :> void ;
    
    byte >>= 24;
    ret_val = byte;
    return ret_val;
}

#ifndef SIM

void put_c(char c)
{
    txByte(serial_tx, c);
}

char get_c()
{
    return rxByte(serial_rx);
}

void put_s(char s[])
{
    tx_byte_stream(serial_tx, s);
}

#endif
