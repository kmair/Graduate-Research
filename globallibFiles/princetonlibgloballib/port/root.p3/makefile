.SUFFIXES: .f .o
F77 = gfortran
FFLAGS = -O

.f.o:
	$(F77) -c $(FFLAGS) $*.f

done: \
	cpoly.o \
	d1poly.o \
	d1rply.o \
	d2poly.o \
	d2rply.o \
	d3poly.o \
	d3rply.o \
	d4poly.o \
	d4rply.o \
	d5poly.o \
	d5rply.o \
	d6poly.o \
	d6rply.o \
	d7poly.o \
	d7rply.o \
	d8poly.o \
	d8rply.o \
	d9poly.o \
	d9rply.o \
	dcpoly.o \
	drpoly.o \
	dz1con.o \
	dz1del.o \
	dz1dot.o \
	dz1fac.o \
	dz1jac.o \
	dz1mv.o \
	dz1new.o \
	dz1one.o \
	dz1pg.o \
	dz1sol.o \
	dz1upd.o \
	dz1vm.o \
	dz1xpg.o \
	dzero.o \
	dzone.o \
	dzonej.o \
	mullr.o \
	r1poly.o \
	r1rply.o \
	r2poly.o \
	r2rply.o \
	r3poly.o \
	r3rply.o \
	r4poly.o \
	r4rply.o \
	r5poly.o \
	r5rply.o \
	r6poly.o \
	r6rply.o \
	r7poly.o \
	r7rply.o \
	r8poly.o \
	r8rply.o \
	r9poly.o \
	r9rply.o \
	rpoly.o \
	z1con.o \
	z1del.o \
	z1dot.o \
	z1fac.o \
	z1jac.o \
	z1mv.o \
	z1new.o \
	z1one.o \
	z1pg.o \
	z1sol.o \
	z1upd.o \
	z1vm.o \
	z1xpg.o \
	zero.o \
	zone.o \
	zonej.o \
	makefile
	ar ru ../libport.a *.o
	rm -f *.o
	touch done
