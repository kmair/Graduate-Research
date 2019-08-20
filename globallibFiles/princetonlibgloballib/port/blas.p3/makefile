.SUFFIXES: .f .o
F77 = gfortran
FFLAGS = -O

.f.o:
	$(F77) -c $(FFLAGS) $*.f

done: \
	cabs1.o \
	camax.o \
	caxpy.o \
	ccopy.o \
	cdotc.o \
	cdotu.o \
	crotg.o \
	cscal.o \
	csrot.o \
	csscal.o \
	cswap.o \
	damax.o \
	dasum.o \
	daxpy.o \
	dcopy.o \
	ddot.o \
	dnrm2.o \
	drot.o \
	drotg.o \
	dscal.o \
	dsum.o \
	dswap.o \
	icamax.o \
	icamin.o \
	idamax.o \
	idamin.o \
	idmax.o \
	idmin.o \
	iimax.o \
	iimin.o \
	isamax.o \
	isamin.o \
	ismax.o \
	ismin.o \
	samax.o \
	sasum.o \
	saxpy.o \
	scasum.o \
	scopy.o \
	sdot.o \
	snrm2.o \
	srot2.o \
	srot.o \
	srotg.o \
	sscal.o \
	ssum.o \
	sswap.o \
	makefile
	ar ru ../libport.a *.o
	rm -f *.o
	touch done
