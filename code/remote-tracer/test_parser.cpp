#include "tester.h"
#include "parse_inst.h"
#include "cpu.h"

#include <iostream>

using namespace std;

int main()
{
    cpu_state_type cpu;
    cpu.pc = (uint32)memory;
    cpu.lr = 0;
    cpu.sp = 0;
    status_type next;

    cout << "PC:: " << hex << cpu.pc << endl;
    cout << "PC:: " << (void*)memory << endl;
    for(int i = 0; i < 10; i++)
    {
		next = next_addr((uint16*)cpu.pc, &cpu);
		cout << "PC:: " << next.pc << endl;
		cpu.pc = next.pc;

    }
    return 0;
}
