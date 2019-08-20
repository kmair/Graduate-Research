.SUFFIXES: .f .o
F77 = gfortran
FFLAGS = -O

.f.o:
	$(F77) -c $(FFLAGS) $*.f

done: \
	acosh.o \
	arcos.o \
	arsin.o \
	asinh.o \
	atanh.o \
	b1slc.o \
	b1slr.o \
	besci.o \
	bescj.o \
	besri.o \
	besrj.o \
	cdexp.o \
	cdlog.o \
	cosh.o \
	dacosh.o \
	darcos.o \
	darsin.o \
	dasinh.o \
	datanh.o \
	db1slc.o \
	db1slr.o \
	dbesci.o \
	dbescj.o \
	dbesri.o \
	dbesrj.o \
	dcosh.o \
	dgamma.o \
	dsinh.o \
	dtan.o \
	dtanh.o \
	gamma.o \
	sinh.o \
	tan.o \
	tanh.o \
	makefile
	ar ru ../libport.a *.o
	rm -f *.o
	touch done
