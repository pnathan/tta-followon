#include "memory.h"
//reverse in memory.
void reverse(char *s, int len)
{
    int i;
    char c;
    //to avoid falling off the end
    len--;
    for(i = 0; i < len/2; i++)
    {
	c = s[i];
	s[i] = s[len - i];
	s[len - i] = c;
    }
}

void reverse_s(char *s)
{
    reverse(s, strlen(s));
}

int strlen(char *s)
{
    int i = 0;
    while(*s++) i++;
    return i;
}

/* void memset(char* b, char c, int len) */
/* { */
/*     int i; */
/*     for(i = 0; i < len; i++) */
/*     { */
/* 	b[i] = c; */
/*     } */
/* } */
