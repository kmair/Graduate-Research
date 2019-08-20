.SUFFIXES: .f .o
F77 = gfortran
FFLAGS = -O

.f.o:
	$(F77) -c $(FFLAGS) $*.f

done: \
	d7mlp.o \
	d7tpr.o \
	dmnfb.o \
	dmnf.o \
	dmngb.o \
	dmng.o \
	dmnhb.o \
	dmnh.o \
	dn2fb.o \
	dn2f.o \
	dn2gb.o \
	dn2g.o \
	dn2pb.o \
	dn2p.o \
	dn2rdp.o \
	dnsfb.o \
	dnsf.o \
	dnsgb.o \
	dnsg.o \
	drmnfb.o \
	drmnf.o \
	drmngb.o \
	drmng.o \
	drmnhb.o \
	drmnh.o \
	drn2g.o \
	drnsgb.o \
	drnsg.o \
	ds3grd.o \
	ds7grd.o \
	dw7zbf.o \
	f7dhb.o \
	f7hes.o \
	h2rfa.o \
	h2rfg.o \
	ivset.o \
	l7itv.o \
	l7ivm.o \
	l7mst.o \
	l7nvr.o \
	l7sqr.o \
	l7srt.o \
	l7svn.o \
	l7svx.o \
	l7tsq.o \
	l7tvm.o \
	l7upd.o \
	l7vml.o \
	n2cvp.o \
	n2lrd.o \
	o7prd.o \
	parck.o \
	q7apl.o \
	q7rad.o \
	q7rfh.o \
	q7rsh.o \
	r7mdc.o \
	r7tvm.o \
	rldst.o \
	s7dmp.o \
	s7lup.o \
	s7lvm.o \
	v2axy.o \
	v2nrm.o \
	v7cpy.o \
	v7dfl.o \
	v7scl.o \
	v7scp.o \
	v7shf.o \
	v7swp.o \
	makefile
	ar ru ../libport.a *.o
	rm -f *.o
	touch done
