.SUFFIXES: .f .out

F77 = gfortran
FFLAGS =
L = ../libport.a

.f.out:
	$(F77) $(FFLAGS) $*.f $L
	./a.out | sed -f ../ss >$*.out
	rm -f $*.o a.out

M0 = thyp.out mach.out err1.out err2.out err3.out stk1.out stk2.out
M = $(M0) errk.out

machtest: $M	
	for i in $(M0); do cmp $$i sgi/$$i; done
	cmp -s errk.out sgi/errk.li || cmp errk.out sgi/errk.out
	rm $M
	touch machtest

## File sgi/errk.li is for little-endian IEEE arithmetic: the unstructured
## double-precision stack dump in parts of the errk test is different
## for big- and little-endian IEEE-arithmetic systems.  This part of
## the test will fail on systems that do not properly support denormalized
## numbers.  Example of a faulty system: Linux on a DEC Alpha machine.
## On such systems, just "touch machtest" and type "make" again in the
## parent (port3) directory.

## "make all.out" to generate all .out files other than those for machtest.

A = \
	extr.out \
	gama.out \
	gmad.out \
	lgac.out \
	lgad.out \
	lgea.out \
	llza.out \
	lrad.out \
	lrpa.out \
	ltqd.out \
	ltsq.out \
	lyad.out \
	lyma.out \
	lyrc.out \
	lysc.out \
	mfed.out \
	mfte.out \
	mntb.out \
	plyc.out \
	prac.out \
	prad.out \
	prsa.out \
	qbgd.out \
	qblg.out \
	qggd.out \
	qgsg.out \
	qpad.out \
	qpra.out \
	rnrm.out \
	sdba.out \
	sple.out \
	svad.out \
	svbd.out \
	svcd.out \
	svda.out \
	svdb.out \
	svdc.out \
	vdad.out \
	vdbd.out \
	vded.out

all.out: $A
	touch all.out
