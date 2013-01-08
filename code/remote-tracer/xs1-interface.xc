#include <xs1.h>
//Exposes needed XS1 functions to C/ASM
void pn_setps(int reg, int val)
{
	setps(reg, val);
}
