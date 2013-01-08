//itoa.c 
//itoa for the xmos
#include "memory.h"
#include "itoa.h"
//cadged off the Wiki article. Attributed to K&R 
/* itoa:  convert n to characters in s */
void itoa(int n, char s[])
{
    int i, sign;

    if ((sign = n) < 0)  /* record sign */
        n = -n;          /* make n positive */
    i = 0;
    do {       /* generate digits in reverse order */
        s[i++] = n % 10 + '0';   /* get next digit */
    } while ((n /= 10) > 0);     /* delete it */
    if (sign < 0)
        s[i++] = '-';
    s[i] = '\0';
    reverse_s(s);
} 


//goal: hex output
void itoa16(int n, char s[])
{
    int i, sign;

    if ((sign = n) < 0)  /* record sign */
        n = -n;          /* make n positive */
    i = 0;
    do {       /* generate digits in reverse order */
	if(n % 16 < 10)
	    s[i++] = n % 16 + '0';   /* get next digit */
	else
	    s[i++] = ((n % 16) - 10) + 'A';   /* get next digit */
    } while ((n /= 16) > 0);     /* delete it */
    if (sign < 0)
        s[i++] = '-';
    s[i] = '\0';
    reverse_s(s);
} 


