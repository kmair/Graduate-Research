.SUFFIXES: .f .o
F77 = gfortran
FFLAGS = -O

.f.o:
	$(F77) -c $(FFLAGS) $*.f

done: \
	d7egr.o \
	da7sst.o \
	dc7vfn.o \
	dd7dgb.o \
	dd7dog.o \
	dd7dup.o \
	dd7mlp.o \
	dd7tpr.o \
	dd7upd.o \
	df7dhb.o \
	df7hes.o \
	dg7itb.o \
	dg7lit.o \
	dg7qsb.o \
	dg7qts.o \
	dh2rfa.o \
	dh2rfg.o \
	ditsum.o \
	divset.o \
	dl7itv.o \
	dl7ivm.o \
	dl7msb.o \
	dl7mst.o \
	dl7nvr.o \
	dl7sqr.o \
	dl7srt.o \
	dl7svn.o \
	dl7svx.o \
	dl7tsq.o \
	dl7tvm.o \
	dl7upd.o \
	dl7vml.o \
	dn2cvp.o \
	dn2lrd.o \
	do7prd.o \
	dparck.o \
	dq7apl.o \
	dq7rad.o \
	dq7rfh.o \
	dq7rsh.o \
	dr7mdc.o \
	dr7tvm.o \
	drldst.o \
	drn2gb.o \
	ds7bqn.o \
	ds7cpr.o \
	ds7dmp.o \
	ds7ipr.o \
	ds7lup.o \
	ds7lvm.o \
	dsm.o \
	dv2axy.o \
	dv2nrm.o \
	dv7cpy.o \
	dv7dfl.o \
	dv7ipr.o \
	dv7prm.o \
	dv7scl.o \
	dv7scp.o \
	dv7shf.o \
	dv7swp.o \
	dv7vmp.o \
	i7copy.o \
	i7do.o \
	i7mdcn.o \
	i7pnvr.o \
	i7shft.o \
	m7seq.o \
	m7slo.o \
	n7msrt.o \
	s7etr.o \
	s7rtdt.o \
	stopx.o \
	makefile
	ar ru ../libport.a *.o
	rm -f *.o
	touch done
