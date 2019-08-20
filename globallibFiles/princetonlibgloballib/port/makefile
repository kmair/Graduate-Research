P3 = app1.p3 app2.p3 blas.p3 de.p3 fft.p3 frame.p3 lin.p3 linopt.p3\
	nl2opt.p3 nl3opt.p3 nl4opt.p3 post.p3 quad.p3 root.p3\
	sparse.p3 spfns.p3 util.p3

libport.a: mfiles frame.p3/done p3tests/machtest\
		app1.p3/done app2.p3/done blas.p3/done de.p3/done\
		fft.p3/done lin.p3/done linopt.p3/done nl2opt.p3/done\
		nl3opt.p3/done nl4opt.p3/done post.p3/done quad.p3/done\
		root.p3/done sparse.p3/done spfns.p3/done util.p3/done
	-ranlib libport.a 2>&1 | sed '/not found/d'

# Some systems need ranlib, some do not.  The above ranlib invocation
# should work on both kinds of systems.  If your system lacks ranlib,
# you do not need it.

app1.p3/done:
	cd app1.p3; make

app2.p3/done:
	cd app2.p3; make

blas.p3/done:
	cd blas.p3; make

de.p3/done:
	cd de.p3; make

fft.p3/done:
	cd fft.p3; make

frame.p3/done:
	cd frame.p3; make
	-ranlib libport.a 2>&1 | sed '/not found/d'

lin.p3/done:
	cd lin.p3; make

linopt.p3/done:
	cd linopt.p3; make

nl2opt.p3/done:
	cd nl2opt.p3; make

nl3opt.p3/done:
	cd nl3opt.p3; make

nl4opt.p3/done:
	cd nl4opt.p3; make

p3tests/machtest:
	cd p3tests; make; rm -f *.out

post.p3/done:
	cd post.p3; make

quad.p3/done:
	cd quad.p3; make

root.p3/done:
	cd root.p3; make

sparse.p3/done:
	cd sparse.p3; make

spfns.p3/done:
	cd spfns.p3; make

util.p3/done:
	cd util.p3; make

mfiles:
	for i in $(P3); do \
		cat makefile.top >$$i/makefile;\
		cd $$i; ls *.f | sed 's/\.f$$/.o/;s/.*/	& \\/' >>makefile; cd ..;\
		cat makefile.bot >>$$i/makefile;\
		done
	touch mfiles

clean:
	rm -f\
		libport.a\
		mfiles\
		*.p3/makefile\
		*/done\
		*/*.o\
		*/*.out\
		p3tests/machtest\
		nohup.out
