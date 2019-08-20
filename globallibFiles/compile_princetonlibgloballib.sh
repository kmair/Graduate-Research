#!/bin/sh -x
#############################################################
# This file compiles the test problems from the princetonlib
# and globallib collections
#
# Luis M Rios lmrios@gmail.com
##############################################################

curdir=`pwd`
princetondir=princetonlibgloballib

#compile amplsolver.a
cd $princetondir/solvers
make clean
make

#compile to object files other necessary files
cd nlc
gcc -c -I.. nlcmisc.c
cd ..

cd examples
gcc -c -I.. keywds.c
gcc -c -I.. rvmsg.c
cd ..

cd ..

#compile test problems
for testproblem in `ls *.c | sed -e 's/\.[a-zA-Z]*$//'`; do
gcc -o $testproblem.exe $testproblem.c -Isolvers solvers/nlc/nlcmisc.o solvers/examples/rvmsg.o solvers/examples/keywds.o solvers/funcadd0.o solvers/amplsolver.a -lm -ldl
done
