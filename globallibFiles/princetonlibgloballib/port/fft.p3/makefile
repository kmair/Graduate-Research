.SUFFIXES: .f .o
F77 = gfortran
FFLAGS = -O

.f.o:
	$(F77) -c $(FFLAGS) $*.f

done: \
	df1ft.o \
	dfftc.o \
	dfftci.o \
	dfft.o \
	dfftr.o \
	dfftri.o \
	dm1ft.o \
	dm2ft.o \
	dm33ft.o \
	dm3ft.o \
	dm44ft.o \
	dm5ft.o \
	dm66ft.o \
	dm7ft.o \
	dm88ft.o \
	dmftcc.o \
	dmftci.o \
	dmftcr.o \
	dmftrc.o \
	dmftri.o \
	drltr.o \
	f1ft.o \
	fftc.o \
	fftci.o \
	fft.o \
	fftr.o \
	fftri.o \
	m1ft.o \
	m2ft.o \
	m33ft.o \
	m3ft.o \
	m44ft.o \
	m5ft.o \
	m66ft.o \
	m7ft.o \
	m88ft.o \
	mftcc.o \
	mftci.o \
	mftcr.o \
	mftrc.o \
	mftri.o \
	rltr.o \
	makefile
	ar ru ../libport.a *.o
	rm -f *.o
	touch done
