.SUFFIXES: .f .o
F77 = gfortran
FFLAGS = -O

.f.o:
	$(F77) -c $(FFLAGS) $*.f

done: \
	a9rntc.o \
	a9rntd.o \
	a9rnti.o \
	a9rntl.o \
	a9rntr.o \
	ceil.o \
	d1mach.o \
	dceil.o \
	dflr.o \
	e9rint.o \
	enter.o \
	entsrc.o \
	eprint.o \
	erroff.o \
	fdump.o \
	flr.o \
	frmatd.o \
	frmati.o \
	frmatr.o \
	i0tk00.o \
	i10wid.o \
	i1mach.o \
	i8save.o \
	i8tsel.o \
	iceil.o \
	idceil.o \
	idflr.o \
	iflr.o \
	istkgt.o \
	istkin.o \
	istkmd.o \
	istkqu.o \
	istkrl.o \
	istkst.o \
	leave.o \
	n5err.o \
	nerror.o \
	r1mach.o \
	retsrc.o \
	s1mach.o \
	s2mach.o \
	s3mach.o \
	s88fmt.o \
	sdump.o \
	seterr.o \
	stkdmp.o \
	u9dmp.o \
	makefile
	ar ru ../libport.a *.o
	rm -f *.o
	touch done
