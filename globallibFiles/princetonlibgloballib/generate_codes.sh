#!/bin/sh -x
################################################################
# This file generate the codes for the test problems from the 
# princetonlib and globallib collections from AMPL .mod files
#
# The AMPL solver modtocode is used to convert AMPL .mod files to
# C source code. modtocode uses the nlc program by D. Gay distributed
# with AMPL.
# The ampl program is assumed to be present in the modfiles directory.
# Make sure that ampl is appropriate for the machine's architecture.
#
# Luis M Rios lmrios@gmail.com
################################################################

curdir=`pwd`
port=port
modfiles=mod_models
nlc=solvers/nlc

#compile port
cd $port
make clean
make
mv libport.a ../
cd ..

#compile nlc program from AMPL/solvers/nlc/
cd $nlc
make clean
make
mv nlc ../../$modfiles
cd ../..

#compile AMPL solver
cd solvers/examples
make modtocode
mv modtocode ../../$modfiles
cd ../../$modfiles

#call script to convert .mod files
./convert2source.sh

