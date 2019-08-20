.SUFFIXES: .f .out

F77 = f77
FFLAGS =
L = ../libport.a

.f.out:
	$(F77) $(FFLAGS) $*.f $L
	./a.out | sed -f ../ss >$*.out
	rm -f $*.o a.out

## "make all.out" to generate .out files for all self-contained examples.

A = \
	apnr.out \
	bura.out \
	burb.out \
	cdex.out \
	cdlg.out \
	cpla.out \
	cspa.out \
	cspe.out \
	cspg.out \
	cspq.out \
	ddea.out \
	desa.out \
	ebea.out \
	errk.out \
	evaa.out \
	extr.out \
	ffta.out \
	fftc.out \
	fmtr.out \
	lbaa.out \
	lbab.out \
	lbaf.out \
	lbak.out \
	lban.out \
	lbap.out \
	lgef.out \
	lgeh.out \
	lgel.out \
	lgem.out \
	lnab.out \
	lpsa.out \
	lpsb.out \
	lpsf.out \
	lpsg.out \
	lpsk.out \
	lpsm.out \
	lrpa.out \
	lrpb.out \
	lrpe.out \
	lrpf.out \
	lrpg.out \
	lsfa.out \
	lyma.out \
	lymk.out \
	lymp.out \
	mfte.out \
	mftf.out \
	mftg.out \
	mllr.out \
	mnna.out \
	nlsa.out \
	nlsb.out \
	nlsj.out \
	nlsk.out \
	nlsp.out \
	nlsr.out \
	nmsk.out \
	np2a.out \
	np2b.out \
	np2e.out \
	np2f.out \
	nsfa.out \
	nsnm.out \
	ntle.out \
	ntlf.out \
	ntlh.out \
	ntlk.out \
	ntlm.out \
	ntlp.out \
	ntlr.out \
	ntlt.out \
	ntlu.out \
	pdea.out \
	pdew.out \
	prs1.out \
	prsz.out \
	qbla.out \
	qblc.out \
	qblg.out \
	qgsg.out \
	qgsh.out \
	qgsj.out \
	qgsm.out \
	qgsp.out \
	qgsr.out \
	qgst.out \
	qodd.out \
	qpra.out \
	ranc.out \
	rnrm.out \
	rpad.out \
	sdba.out \
	splf.out \
	vdsa.out \
	vdsb.out \
	vdse.out \
	xkhd.out \
	xkhi.out \
	xkth.out \
	zera.out \
	zona.out \
	zonb.out

all.out: $A
	touch all.out
