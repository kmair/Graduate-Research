# /****************************************************************
# Modified version of makefile for modtoblack solver
# modtoblack is an AMPL solver that reads a problem in .mod format and
#            returns the problem coded in C
# ****************************************************************/

.SUFFIXES: .c .cpp .f .o
CC = cc
S = ..
# "S = .." assumes this directory is solvers/examples .

CFLAGS = -g
# Add -DKR_headers to CFLAGS if your C compiler does not
# understand ANSI C function headers, e.g.
#   CFLAGS = -O -DKR_headers
# If things don't run right, you may need to change -O to -g
# so you can poke around with a debugger.

# See $S/makefile for other comments about CFLAGS.

.c.o:
	$(CC) -c $(CFLAGS) -I$S $*.c

.cpp.o:
	$(CC) -c $(CFLAGS) -I$S $*.cpp

## On HP systems, add -ldld to the end of the L = line below.
## On linux and Solaris systems, add -ldl to the end of the L = line below.
## On pythagoras use Lf2c = -lg2c

L = -lm -ldl
Lf2c = 
cport = -L../.. -lport
#cport = cport.a
# $(cport) = PORT subroutine library.
# Ask netlib@netlib.bell-labs.com to
#   send dmngb dmnhb dn2g dn2gb dq7rgs from port
# to get source for the relevant (small) part of PORT.
# -lf2c is for f2c's version of Fortran I/O: we assume
# $(cport) was compiled by f2c and cc.
Lp = $(cport) $(Lf2c)

FC = fc
FFLAGS =
# Fortran compiler (fc script provided with f2c)

.f.o:
	$(FC) -c $(FFLAGS) $*.f

A = $S/amplsolver.a
Af = $S/funcadd0.o $A

modtoblack = MODtoBLACK.o $A
modtoblack: $(modtoblack)
	$(CC) -o modtoblack $(modtoblack) $(Lp) $L

modtocode = MODtoCODE.o $A
modtocode: $(modtocode)
	$(CC) -o modtocode $(modtocode) $(Lp) $L

$S/amplsolver.a:
	cd $S; make amplsolver.a

clean:
	rm -f $(all1) $(all) *.o dietd.duw *.mex*

## For use with MATLAB 5.x, "make amplfunc.mex" or "make spamfunc.mex".
## Make sure your PATH includes MATLAB's "bin" directory.
# added to af funcadd1 by luis m rios

af = amplfunc.c $S/funcaddr.o $A

amplfunc.mex: $(af)
	mex -I$S $(af) $L
	touch amplfunc.mex

saf = spamfunc.c $S/funcaddr.o $A

spamfunc.mex: $(saf)
	mex -I$S $(saf)
	touch spamfunc.mex

$S/funcaddr.o: $S/funcadd1.c
	cd $S; make funcaddr.o

## Variants for MATLAB 4.x: "make amplfun4.mex" or "make spamfun4.mex".

af4 = amplfun4.c $A

amplfun4.mex: $(af4)
	cmex -I$S $(af4)
	touch amplfun4.mex

saf4 = spamfun4.c $A

spamfun4.mex: $(saf4)
	cmex -I$S $(saf4)
	touch spamfun4.mex
