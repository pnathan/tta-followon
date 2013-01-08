#Generates tracer.xe, the tracing executable for the XCore

# Part of the thesis project of Paul Nathan at the University of
#Idaho.

#Generally make everything. This is the default
all: debug tracer.xe 

#Makes the extra info for the debugging
debug: disasm.tracer

tracer.xe: cpu.o io.o memory.o  debug_handler.o itoa.o io-base.o driver.o debug_lib.o xs1-interface.o parse_inst.o
	xcc cpu.o io.o memory.o debug_lib.o debug_handler.o itoa.o io-base.o driver.o xs1-interface.o parse_inst.o \
	XC-1A.xn -o tracer.xe

parse_inst.o: parse_inst.c parse_inst.h cpu.o
	xcc -c parse_inst.c

io-base.o: io-base.xc io-base.h
	xcc -c io-base.xc XC-1A.xn

io.o: io.c io.h
	xcc -c io.c

cpu.o: cpu.c cpu.h
	xcc -c cpu.c

memory.o: memory.c memory.h
	xcc -c memory.c

debug_lib.o: debug_lib.c debug_lib.h
	xcc -c debug_lib.c XC-1A.xn

driver.o: driver.xc
	xcc -c driver.xc XC-1A.xn

#Strips out some code that the compiler generates but forces errors in
#a debug handler.
debug_handler.o: debug_handler.c
	xcc -S debug_handler.c
	perl prepare_debug_handler.pl debug_handler.s debug_handler
	xcc -c	debug_handler.s

itoa.o:	itoa.c itoa.h
	xcc -c itoa.c

xs1-interface.o: xs1-interface.xc xs1-interface.h
	xcc -c xs1-interface.xc

#Removes temp files
clean:
	rm -f *.o
	rm -f disasm.tracer

#Removes the output as well
clobber: clean
	rm tracer.xe

#Disassembly
disasm.tracer: tracer.xe
	xobjdump -d tracer.xe > disasm.tracer
