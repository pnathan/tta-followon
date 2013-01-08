all: simple-p-c.xe random-producers.xe one-transfer.xe disjoint.xe multi-transfer.xe three-transfer.xe disjoint.xe one-exchange.xe select.xe transaction.xe

one-transfer.xe: one-transfer.xc
	xcc one-transfer.xc ../code/XC-1A.xn -o one-transfer.xe
	xobjdump one-transfer.xe -S -D > one-transfer.objdump
	xsim one-transfer.xe --trace-to one-transfer.trace

one-exchange.xe: one-exchange.xc
	xcc one-exchange.xc ../code/XC-1A.xn -o one-exchange.xe
	xobjdump one-exchange.xe -S -D > one-exchange.objdump
	xsim one-exchange.xe --trace-to one-exchange.trace


three-transfer.xe: three-transfer.xc
	xcc three-transfer.xc ../code/XC-1A.xn -o three-transfer.xe
	xobjdump three-transfer.xe -S -D > three-transfer.objdump
	xsim three-transfer.xe --trace-to three-transfer.trace


disjoint.xe: disjoint.xc
	xcc disjoint.xc ../code/XC-1A.xn -o disjoint.xe
	xobjdump disjoint.xe -S -D > disjoint.objdump
	xsim disjoint.xe --trace-to disjoint.trace

multi-transfer.xe: multi-transfer.xc
	xcc multi-transfer.xc ../code/XC-1A.xn -o multi-transfer.xe
	xobjdump multi-transfer.xe -S -D > multi-transfer.objdump
	xsim multi-transfer.xe --trace-to multi-transfer.trace

select.xe: select.xc
	xcc select.xc ../code/XC-1A.xn -o select.xe
	xobjdump select.xe -S -D > select.objdump
	xsim select.xe --trace-to select.trace


transaction.xe: transaction.xc
	xcc transaction.xc ../code/XC-1A.xn -o transaction.xe
	xobjdump transaction.xe -S -D > transaction.objdump
	xsim transaction.xe --trace-to transaction.trace


simple-p-c.xe: simple-p-c.xc
	xcc simple-p-c.xc ../code/XC-1A.xn -o simple-p-c.xe
	xobjdump simple-p-c.xe -S -D > simple-p-c.objdump
	xsim simple-p-c.xe --trace-to simple-p-c.trace

random-producers.xe: random-producers.xc
	xcc random-producers.xc ../code/XC-1A.xn -o random-producers.xe

.PHONY: clean
clean:
	rm -f *.xe