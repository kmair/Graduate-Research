.SUFFIXES: .f .o
F77 = gfortran
FFLAGS = -O

.f.o:
	$(F77) -c $(FFLAGS) $*.f

done: \
	a4gqr.o \
	a4ppg.o \
	a4pps.o \
	a4sqr.o \
	a6ph2.o \
	a6sim.o \
	b6ard.o \
	c4nst.o \
	c4ons.o \
	d4clm.o \
	d4gqr.o \
	d4rpg.o \
	d4rps.o \
	d4sqr.o \
	da4gqr.o \
	da4ppg.o \
	da4pps.o \
	da4sqr.o \
	da6ph2.o \
	da6sim.o \
	dc4nst.o \
	dc4ons.o \
	dd4clm.o \
	dd4gqr.o \
	dd4rpg.o \
	dd4rps.o \
	dd4sqr.o \
	dd6bar.o \
	dd6sim.o \
	de4ql.o \
	de6chg.o \
	de6lim.o \
	dfeasa.o \
	dfeas.o \
	dfmin.o \
	dg4etc.o \
	dg6th2.o \
	dg6tzd.o \
	dh4hg.o \
	di4ntl.o \
	diqp.o \
	dl4ags.o \
	dl4p2.o \
	dl4ph1.o \
	dlinpa.o \
	dlinpr.o \
	dlpman.o \
	dlprnt.o \
	dm4top.o \
	dm6con.o \
	dp4rjd.o \
	dp6mdc.o \
	dq6add.o \
	dq6drp.o \
	dq6int.o \
	dq6iqp.o \
	dqp1nt.o \
	dqp2nt.o \
	ds4rot.o \
	dt6dia.o \
	du6dat.o \
	e4ql.o \
	e6chg.o \
	e6lim.o \
	feasa.o \
	feas.o \
	fmin.o \
	g4etc.o \
	g6th2.o \
	g6tzd.o \
	h4hg.o \
	i4ntl.o \
	iqp.o \
	l4ags.o \
	l4p2.o \
	l4ph1.o \
	linpa.o \
	linpr.o \
	lpman.o \
	lprnt.o \
	m4top.o \
	m6con.o \
	p4rjd.o \
	p6mdc.o \
	q6add.o \
	q6drp.o \
	q6int.o \
	q6iqp.o \
	qp1nt.o \
	qp2nt.o \
	s6imd.o \
	t6dia.o \
	u6dat.o \
	makefile
	ar ru ../libport.a *.o
	rm -f *.o
	touch done
