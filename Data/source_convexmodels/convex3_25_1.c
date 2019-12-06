#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

main(int argc, char **argv)
{
 double f_val;

 int i;
 int j;
 int n;
 n = 25;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 f_val = 0;
 double f_temp;
 f_temp = 0;
 for (i=0; i< (n-1); i++)
 {
 f_temp = f_temp + (x[i]*x[i] + x[n-1]*x[n-1])*(x[i]*x[i] + x[n-1]*x[n-1]) - 4*x[i] + 3;
 }
 f_val = f_temp;
 FILE *file_output;
 file_output = fopen("output.out","w");
 fprintf(file_output, "%30.15f\n" ,f_val);
    fclose(file_output);


 return 0;
 }
