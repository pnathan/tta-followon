#ifndef PARSE_INST
#define PARSE_INST
#include "cpu.h"

//Bits per word
#define bpw  16
/*
 Indeed, the least
significant byte is stored at the lowest address.
*/
//Instruction prefixes
#define bau_prefix    0x27f0
#define bla_prefix    0x27e0
#define blacp_prefix  0xe000
#define blat_prefix   0x7340
#define blrb_prefix   0xd400
#define blrf_prefix   0xd000 //something's up here... TODO NEXT
#define brbf_prefix   0x7c00
#define brbt_prefix   0x7400
#define brbu_prefix   0x7700
#define brff_prefix   0x7800
#define brft_prefix   0x7000
#define brfu_prefix   0x7300
#define bru_prefix    0x2fe0
#define retsp_prefix  0x77c0

//Is it a long instruction word? 
bool is_longword(uint16 inst);
//Get a 20-bit prefix
uint32 extract_20(uint16* inst);
//get a 16-bit prefix, 10 bits LO word, 6 bits HI word
uint32 extract_16(uint16* inst); 
uint32 extract_register(uint16 inst);
//get the instruction laying at pc
uint32 get_inst(uint32 pc);
//Get the next address
struct status_type next_addr(uint16 *pc,  struct cpu_state_type *cpu);
#endif
