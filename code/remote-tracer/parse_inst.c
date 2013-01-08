
/*
parse_inst.c

Paul Nathan 2010
Master's thesis, U of Idaho

Routines for parsing an instruction

uint32 next_addr(uint32 *pc, uint32 *inst);
Parses a XCore instruction and returns the next address that the program counter should go to.

bool is_longword(uint16 *inst);
Is the instruction a 4-byte word or 2-byte word?

uint32 get_inst(uint32 pc)
Given an instruction address, get the instruction itself.
If the instruction is 2 bytes long, this will hold:
result | 0x0000ffff == result

*/

#include "parse_inst.h"

//takes the first 16 bits
bool is_longword(uint16 inst)
{
	bool result = false;
	if((inst & 0xf000) == 0xf000)      //1111 -    - -
	{
		if(((0x800 & inst) == 0x800) ||   //xxxx 1xxx xxxx xxxx
			//Mask out everything but the bits we care about
			(((0xf00 & inst) >> 10) == 0)) //xxxx 00xx xxxx xxxx
		{
			result = true;
		}
	}
	return result;
}

/*note: 

When immediate doesn't seem to work right, check if the top and bottom
words need to be swapped */

//pulls out a 20-bit immediate
uint32 extract_20(uint16* inst)
{
	uint32 imm;
	//ASSUMPTION: LOWER 10 BITS ARE NOT AS SIGNIFICANT
	imm = (0x3ff & inst[0]) << 10;
	imm |= 0x3ff & inst[1];
	return imm;
}

uint32 extract_16(uint16 *inst)
{
	uint32 imm;
	//16-bit prefix
	//last 10 on top, last 6 bits on bottom
	imm = 0x3ff & inst[0] << 10;
	imm |= 0x3f & inst[1];
	return imm;
}
uint32 extract_register(uint16 inst)
{
	return 0xf & (inst >> 6);
}
//gets the instruction from the location specified in the program counter.
uint32 get_inst(uint32 pc)
{
	uint32 result = 0;
	uint16 *pc_pointer = (uint16*)(pc);
	if(is_longword(pc))
	{
		//get lower 2 bytes
		result = *pc_pointer;
		//get upper 2 bytes & pack them
		result |= (*(pc_pointer + 1) << 16);
	}
	else
	{
		//Cast and dereference
		result = *((uint16*)pc);
	}

	return result;
}

//Copied from XS1 assembly language manual
/*
The branch instructions include conditional and unconditional relative
branches. A branch using the address in a register is provided; a
relative branch which adds a scaled register operand to the program
counter is provided to support jump tables.

BRFT if c then pc <-  pc + u16 * 2 branch relative forward true
BRFF if !c then pc <-  pc + u16 * 2 branch relative forward false
BRBT if c then pc <-  pc - u16 * 2 branch relative backward true
BRBF if !c then pc <-  pc -  u16 * 2 branch relative backward false
BRFU pc <-   pc + u16 * 2 branch relative forward unconditional
BRBU pc <-   pc - u16 * 2 branch relative backward unconditional
BRU pc <-   pc + s * 2 branch relative unconditional (via register)
BAU pc <-   s branch absolute unconditional (via register)

In some cases, the calling instructions described below can be used to
optimise branches; as they overwrite the link register they are not
suitable for use in leaf procedures which do not save the link
register.  The procedure calling instructions include relative calls,
calls via the constant pool, indexed calls via a dedicated register
(r11) and calls via a register. Most calls within a single program
module can be encoded in a single instruction; inter-module calling
requires at most two instructions.

BLRF lr <-  pc; branch and link relative forward
pc <-   pc + u20 * 2
BLRB lr <-  pc; branch and link relative backward
pc <-  pc u20 * 2
BLACP lr <-  pc; branch and link absolute via constant pool
pc <-  mem[cp + u20 * Bpw]
BLAT lr <-  pc; branch and link absolute via table
pc <-   mem[r11 + u16 * Bpw]
BLA lr   pc; branch and link absolute (via register)
pc <- s

Notice that control transfers which do not affect the link (required
for tail calls to procedures) can be performed using one of the LDWCP,
LDWCPL, LDAPF or LDAPB instructions followed by BAU r11.

RETSP if u16 > 0 then    contract stack
{ sp <-  sp + u16 * Bpw;
lr <-  mem[sp]}; and return
pc <-  lr
*/
#ifdef WIN32TESTING
#include <iostream>
using namespace std;
#define TESTLOG(a) { cout << hex << a << endl; }
#else
#define TESTLOG(a)
#endif


//note that this can produce false positives in the general case
#define match(data, mask) ((data & mask) == mask)

/*
* Pointer to instruction
* Current CPU state.
*/
struct status_type next_addr(uint16 *pc_ptr,  struct cpu_state_type *cpu)
{
	//Polishing TODO:
	//Refactor imm decls out
	//hoist next_* into results;

	uint16 inst[2];
	//get some data from code space
	//asm("#stuff");
	inst[0] = *(pc_ptr + 0);
	inst[1] = *(pc_ptr + 1);
	//asm("#stuff2");

	TESTLOG(inst[0]);
	TESTLOG(inst[1]);

	uint32 imm = 0;
	struct status_type results;
	//Direct conversion right over.
	uint32 pc = (uint32)pc_ptr;

	uint32 next_pc = 0;
	uint32 next_lr = 0;
	uint32 next_sp = 0;

	//Ensure that these will be valid if not modified
	results.lr = cpu->lr;
	results.sp = cpu->sp;

	results.was_break = true;

	if(is_longword(inst[0]))
	{
	    results.is_longword = true;
	        if(match(inst[1], blacp_prefix))
		{
			imm = extract_20(inst);
			//LR gets current PC.
			next_lr = pc;
			next_pc = (cpu->cp + imm * bpw);
		}
		else if(match(inst[1], blat_prefix))		    
		{
			imm = extract_16(inst);
			next_lr = pc;
			next_pc = *((uint32*)(cpu->r[11] + imm * bpw));
		}
		else if (match(inst[1], blrb_prefix))
		{
			imm = extract_20(inst);
			next_lr = pc;
			next_pc = pc - imm * 2;
		}
		else if(match(inst[1], blrf_prefix))
		{
			imm = extract_20(inst);
			next_lr = pc;
			next_pc = pc + imm * 2;
		}
		else if(match(inst[1], brbf_prefix))
		{
			//shift the register denotation down to LSB
			//Mask out anything else
			int c = extract_register(inst[1]);
			if(cpu->r[c] == 0)
			{
				int imm = extract_16(inst);
				next_pc = pc - imm * 2;
			}
		}
		else if(match(inst[1], brbt_prefix))
		{
			int c = extract_register(inst[1]);
			if(cpu->r[c] != 0)
			{
				int imm = extract_16(inst);
				next_pc = pc - imm * 2;
			}
		}
		else if(match(inst[1], brbu_prefix))
		{
			int imm = extract_16(inst);
			next_pc = pc - imm * 2;
		}
		else if(match(inst[1], brff_prefix))
		{
			//if c = 0, then pc = pc + imm * 2
			int c = extract_register(inst[1]);
			if(cpu->r[c] == 0) //test for 0
			{
				int imm = extract_16(inst);
				next_pc = pc + imm * 2;
			}
		}
		else if(match(inst[1], brft_prefix))
		{
			//if c != 0, then pc = pc + imm * 2
			int c = extract_register(inst[1]);
			if(cpu->r[c] != 0) //test for 0
			{
				int imm = extract_16(inst);
				next_pc = pc + imm * 2;
			}
		}
		else if(match(inst[1], brfu_prefix))
		{
			imm = extract_16(inst);
			next_pc = pc + imm * 2;
		}
		else if (match(inst[1], retsp_prefix))
		{
			int imm = extract_16(inst);
			if(imm > 0)
			{
				//adjust the SP!?
				next_sp = cpu->sp + imm * bpw;
				next_lr = *((uint32*)next_sp);
			}
			next_pc = next_lr;
		}
		else
		{
			next_pc = cpu->pc + 4;
			results.was_break = false;
		}
	}
	else ///SHORT WORD
	{
	    results.is_longword = false;
	    if(match(inst[0], bau_prefix))
		{
			//Mask out the non-register bits
			next_pc = cpu->r[inst[0] & 0x000f];
		}
	    else if(match(inst[0], bla_prefix))
		{
			next_lr = pc;
			next_pc =  cpu->r[inst[0] & 0x000f];
		}
	    else if(match(inst[0], blacp_prefix))
		{
			//TODO: look over the immediate mask
			next_lr = pc;
			//Mask out the instruction
			imm = ~(0xFC00) & inst[0];
			next_pc =  *((uint32*)(cpu->cp + imm * bpw));
		}
	    else if (match(inst[0], blat_prefix))
		{
			imm = 0x3f & inst[0];
			next_lr = pc;
			next_pc = *((uint32*)(cpu->r[11] + imm * bpw));
		}
	    else if (match(inst[0], blrb_prefix))
		{
			imm = 0x3ff & inst[0];
			next_lr = pc;
			next_pc = pc - imm * 2;
		}
	    else if(match(inst[0], blrf_prefix))
		{
			imm = 0x3ff & inst[0];
			next_lr = pc;
			next_pc = pc + imm * 2;
		}
	    else if(match(inst[0], brbf_prefix))
		{
			int c = extract_register(inst[0]);
			if(cpu->r[c] == 0)
			{
				int imm = 0x3f & inst[0];
				//0x3f is the immediate
				next_pc = pc - imm * 2;
			}
		}
	    else if(match(inst[0], brbt_prefix))
		{
			int c = extract_register(inst[0]);
			if(cpu->r[c] != 0)
			{
				int imm = 0x3f & inst[0];
				//0x3f is the immediate
				next_pc = pc - imm * 2;
			}
		}
	    else if(match(inst[0], brbu_prefix))
		{
			int imm = 0x3f & inst[0];
			next_pc = pc - imm * 2;
		}
	    else if(match(inst[0], brff_prefix))
		{
			int c = extract_register(inst[0]);
			if(cpu->r[c] == 0)
			{
				int imm = 0x3f & inst[0];
				next_pc = pc + imm * 2;
			}
		}
	    else if(match(inst[0], brft_prefix))
		{
			int c = extract_register(inst[0]);
			if(cpu->r[c] != 0)
			{
				int imm = 0x3f & inst[0];
				next_pc = pc + imm * 2;
			}
		}
	    else if(match(inst[0], brfu_prefix))
		{
			int imm = 0x3f & inst[0];
			next_pc = pc + imm * 2;
		}
	    else if(match(inst[0], bru_prefix))
		{
			int c = 0xf & inst[0]; //register is in lowest 4 bits
			int s = cpu->r[c];     //Note that s can be signed.
			next_pc = pc + s * 2;
		}
	    else if(match(inst[0], retsp_prefix))
		{
			int imm = 0x3f & inst[0];
			if(imm > 0)
			{
				//adjust the SP!?
				next_sp = cpu->sp + imm * bpw;
				next_lr = *((uint32*)next_sp);
			}
			next_pc = next_lr;
		}
		else
		{
			//may be removable.
			next_pc = pc + 2;
			results.was_break = false;
		}
	}

	results.pc = (uint32)next_pc;
	results.lr = (uint32)next_lr;
	results.sp = (uint32)next_sp;
	return results;
}
