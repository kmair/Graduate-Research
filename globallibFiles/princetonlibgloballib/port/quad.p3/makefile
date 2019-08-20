.SUFFIXES: .f .o
F77 = gfortran
FFLAGS = -O

.f.o:
	$(F77) -c $(FFLAGS) $*.f

done: \
	a6aus0.o \
	a6ausq.o \
	bquad.o \
	cspqu.o \
	d1qchg.o \
	d1qdap.o \
	d1qext.o \
	d1qfun.o \
	d1qint.o \
	d1quad.o \
	d2quad.o \
	d6aus0.o \
	d6ausq.o \
	dbquad.o \
	dcspqu.o \
	dg8ex2.o \
	dg8exa.o \
	dg8log.o \
	dg8xab.o \
	dg8xa.o \
	dgausq.o \
	dgq0in.o \
	dgq1.o \
	dgqex2.o \
	dgqexa.o \
	dgqex.o \
	dgqlog.o \
	dgqm11.o \
	dgqxab.o \
	dgqxa.o \
	dodeq.o \
	dquad.o \
	drquad.o \
	dsplni.o \
	g8ex2.o \
	g8exa.o \
	g8log.o \
	g8xab.o \
	g8xa.o \
	gausq.o \
	gq0in.o \
	gq1.o \
	gqex2.o \
	gqexa.o \
	gqex.o \
	gqlog.o \
	gqm11.o \
	gqxab.o \
	gqxa.o \
	odeq.o \
	quad.o \
	r1qchg.o \
	r1qdap.o \
	r1qext.o \
	r1qfun.o \
	r1qint.o \
	r1quad.o \
	r2quad.o \
	rquad.o \
	splni.o \
	makefile
	ar ru ../libport.a *.o
	rm -f *.o
	touch done
