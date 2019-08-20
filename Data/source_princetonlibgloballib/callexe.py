import os
import sys
import re
import argparse
import subprocess
import numpy as np

if __name__ == '__main__':
    compileFile = sys.argv[1]
    Str = compileFile.split(sep = '.')
    if len(Str)>1:
        raise Exception("Just mention the Compiler file's name without type\n For example: To compile 'file.c', the first argument must be 'file' only")
    inputFile = sys.argv[2]
    if inputFile != "input.in":
        raise Exception("Input file name must only be 'input.in' and cannot be anything else")
    
# read .c file and match the line of for loop to get
# the number of variables
# output: (integer) number of variables
##################################### Num of var #####################################
# def get_number(file):
#     numOfVar = -1
#     infile = open(file+'.c','r')
#     for line in infile.readlines():
#         matchObj = re.match(r'for',line,re.M|re.I)
#         if matchObj:
#             line_group1 = line.split(';')
#             line_group2 = line_group1[1].split('<')
#             numOfVar = line_group2[1].strip()
#     return int(numOfVar)
###############################################################################################################
# generate input.in file according to requested number of input values
# output: input.in file
# def create_input(file,number):
#     infile = open(file, 'w')
#     for i in range(number):
#         infile.write(str(2*(i+1))+"\n")
#     infile.close()
import tensorflow

def main():
    # get the number of variables
    #numOfVar = get_number(compileFile)
    
    # compile .c file
    os.system('gcc '+compileFile+'.c -o '+compileFile)
    # create input.in file
    #create_input(inputFile, numOfVar)
    # run executable file
    os.system('.\\'+compileFile)
    



main()