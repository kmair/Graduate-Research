/*
------------------------------------------------------------------
 .mod to black box program converter
 
 modtoblack is an AMPL solver
 The purpose is to read a model file (.mod) and produce a black-box
 standalone executable and a file containing relevant information of the problem.
 The standalone programs and files generated are expected to be use with
 the script mydfo.
 {\tt modtoblack} consists of a main program, which provides input, output 
 and execution for the {\tt nlc} function by D. Gay \cite{g97} that reads 
 the .mod file.
 
 The executable is called 'truth.exe' and is delivered in the same directory
 where AMPL resides. 
 The input file is "input.in". It is composed of the variable values, each
 value in a single line. The output file is "output.out" and only contains
 the value of the objective evaluated.
 
 The file 'problem.data' contains relevant information about the problem to
 be solved.
 line 1: number of variables
 line 2: lower bounds*
 line 3: upper bounds*
 line 4: starting values
 
 *bounds are limited to be within [-LARGE_NUM, LARGE_NUM] 

developed by Luis Miguel Rios, lmrios@gmail.com
------------------------------------------------------------------
*/

#include "asl.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LARGE_NUM 10000000
#define MAX_OBJ_VALUE 1E80

int
main(int argc, char **argv)
{
    FILE *nl;
    int i;
    char *fmt, *s, *stub;
    cgrad *cg;
    ograd *og;
    real *b, *c;
    ASL *asl;
	char msg[64];

    if (argc < 2) {
        printf("Usage: %s stub\n", argv[0]);
        return 1;
        }

//--------------------------------------------------
//The ASL structure can be changed to adapt better to
//the problem to solve. Some structures are reduced to
//avoid otherwise unused routines.
//
// reader   ASLtype      nonlinear information
// f_read   ASL_read_f   no derivatives: linear objectives and constraints only
// fg_read  ASL_read_fg  first derivatives
//--------------------------------------------------
    asl = ASL_alloc(ASL_read_fg);
    stub = argv[1];
//--------------------------------------------------
//Jac0dim reads: (among others)
//n_var    number of variables (total)
//nbv      number of binary variables 
//n_con    number of constraints (total)
//n_obj    number of objectives (total)
//--------------------------------------------------
    nl = jac0dim(stub, (fint)strlen(stub));

#define Double(n) (real *)Malloc((n)*sizeof(real));
    X0 = Double(n_var);

//Set want_derivs = 0 if you want to compute nonlinear
//functions but will never compute derivatives, reduce
//overhead (before calling fg_read)
    want_derivs = 0;
    fg_read(nl,0);
        
//--------------------------------
//Write file black_box_code.c
//Produce C code out of AMPL model and data
//Use 'nlc' program from AMPL/solvers/nlc/
//The main function of the black_box_code is
//presented below
//--------------------------------
    char nlc_usage[80];    
    strcpy (nlc_usage, "./nlc -1 ");    
    strcat(nlc_usage, stub);
    strcat(nlc_usage, ".nl > black_box.c");
    system(nlc_usage);                     
        
//-----------------------------------------------
//Write main function of black_box.c
//Reads file    input.in
//Outputs file output.out
//Evaluates the objective function (objective 0)
//Evaluates the constraints
//Print the constraints value to a file
//-----------------------------------------------        
     
     FILE *black_main;     
     black_main = fopen("black_main.c", "w");
     fprintf (black_main, "\n");
     fprintf (black_main, "#include <stdio.h>\n");
     fprintf (black_main, "#include <stdlib.h>\n");     
     fprintf (black_main, "\n");
     fprintf (black_main, "main(int argc, char **argv)\n");
     fprintf (black_main, "{\n");
     fprintf (black_main, "\n");
//Read input
     fprintf (black_main, "FILE *file_input;\n");
     fprintf (black_main, "real *x_input, f_val, *c_val;\n");
     fprintf (black_main, "double *input_values;\n");
     fprintf (black_main, "fint objective_number;\n");
     fprintf (black_main, "int i;\n");
     fprintf (black_main, "\n");
     fprintf (black_main, "x_input = malloc (%d * sizeof(real));\n",n_var);
     fprintf (black_main, "input_values = malloc (%d * sizeof(double));\n",n_var);
     fprintf (black_main, "c_val = malloc (\%d * sizeof(real));\n",n_con);
     fprintf (black_main, "\n");     
     fprintf (black_main, "file_input = fopen(\"input.in\",\"r\");\n");
	 fprintf (black_main, "if (file_input != NULL)\n");
     fprintf (black_main, "{\n");
     fprintf (black_main, "for (i=0; i < %d; i++)\n", n_var); 
     fprintf (black_main, "    fscanf(file_input, \"%%lf\" ,&input_values[i]);\n");
     fprintf (black_main, "\n");
     fprintf (black_main, "fclose(file_input);\n");
     fprintf (black_main, "for (i=0; i < %d; i++)\n", n_var); 
     fprintf (black_main, " {\n");
     fprintf (black_main, "    x_input[i] = input_values[i];\n");
     fprintf (black_main, " }\n");
     fprintf (black_main, "\n");
//Evaluate function     
	 fprintf (black_main, "f_val = feval0_(&objective_number, x_input);\n");
     fprintf (black_main, "}\n");
     fprintf (black_main, "else\n");
     fprintf (black_main, "{\n");
     fprintf (black_main, "f_val = 1e80;\n");
     fprintf (black_main, "}\n");
     fprintf (black_main, "\n");

//Produce output
     fprintf (black_main, "FILE *output_out;\n");
     fprintf (black_main, "output_out = fopen (\"output.out\",\"w\");\n");
     fprintf (black_main, "fprintf(output_out,\"%%30.15f\\n\",f_val);\n");
	 fprintf (black_main, "fclose(output_out);\n");
     fprintf (black_main, "\n");     
     fprintf (black_main, "}\n");
     fclose(black_main);   

//-------------------------------- 
//Compile black_box, delete intermediate files
//-------------------------------- 
system("cat black_box.c black_main.c > black_box_code.c");

system("rm -f black_box.c");

system("rm -f black_main.c");
system("cp black_box_code.c solvers/nlc/");
system("gcc -o truth.exe solvers/nlc/black_box_code.c solvers/nlc/nlcmisc.o solvers/examples/rvmsg.o solvers/examples/keywds.o solvers/funcadd0.o solvers/amplsolver.a -lm -ldl");    

//Change bounds to within [-LARGE_NUM,LARGE_NUM]
     for(i = 0; i < n_var; i++)
        {
            if (LUv[2*i] < -LARGE_NUM)
            {
				LUv[2*i] = -LARGE_NUM;
            }
            if (LUv[2*i+1] > LARGE_NUM)
            {
				LUv[2*i+1] = LARGE_NUM;
            }        
        }
//--------------------------------------
//Produce file problem.data containing:
//number of variables
//lower bounds
//upper bounds
//starting point
//Produce input file input.in
//---------------------------------------
	 FILE *pro_data;
	 FILE *input_in;
	 pro_data = fopen("problem.data","w");
	 input_in = fopen("input.in","w");
	 float start_value;

	 fprintf(pro_data,"%d\n",n_var);
     for(i = 0; i < n_var; i++)
        {
	 fprintf(pro_data,"%f  ",LUv[2*i]);
	    }
	 fprintf(pro_data,"\n");
     for(i = 0; i < n_var; i++)
        {
	 fprintf(pro_data,"%f  ",LUv[2*i+1]);
	    }
	 fprintf(pro_data,"\n");
     for(i = 0; i < n_var; i++)
        {
	 start_value = 0.5*LUv[2*i] + 0.5*LUv[2*i+1];
	 fprintf(pro_data,"%f  ",start_value);
	 fprintf(input_in,"%f  \n",start_value);
	    }
	 fprintf(pro_data,"\n");

	 fclose(pro_data);
	 fclose(input_in);

	 solve_result_num = 0;
	 sprintf(msg,"Black-box successfully created");
	 write_sol(msg, X0, 0, 0);
	 return 0;
    }
