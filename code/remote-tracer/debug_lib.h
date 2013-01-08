#ifndef DEBUG_LIB
#define DEBUG_LIB


//Fake out being functions.
#define start_singlestep() { \
    turn_ss_on();	\
    asm("dcall"); \
    asm("#SS START"); \
  }

#define stop_singlestep() { \
    asm("#SS STOP"); \
    turn_ss_off();   \
  }
void turn_ss_on();
void turn_ss_off();
void initialize_debugger();
void close_debugger();

//Mocks some code. Ha, ha.
void mock();

#endif
