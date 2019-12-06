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
 n = 5;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double a[n][2*n];
a[0][0] =   62.9447;
a[0][5] =  -62.9447;
a[0][1] =  -80.4919;
a[0][6] =   80.4919;
a[0][2] =  -68.4774;
a[0][7] =   68.4774;
a[0][3] =  -71.6227;
a[0][8] =   71.6227;
a[0][4] =   31.1481;
a[0][9] =  -31.1481;
a[1][0] =   81.1584;
a[1][5] =  -81.1584;
a[1][1] =  -44.3004;
a[1][6] =   44.3004;
a[1][2] =   94.1186;
a[1][7] =  -94.1186;
a[1][3] =  -15.6477;
a[1][8] =   15.6477;
a[1][4] =  -92.8577;
a[1][9] =   92.8577;
a[2][0] =  -74.6026;
a[2][5] =   74.6026;
a[2][1] =    9.3763;
a[2][6] =   -9.3763;
a[2][2] =   91.4334;
a[2][7] =  -91.4334;
a[2][3] =   83.1471;
a[2][8] =  -83.1471;
a[2][4] =   69.8259;
a[2][9] =  -69.8259;
a[3][0] =   82.6752;
a[3][5] =  -82.6752;
a[3][1] =   91.5014;
a[3][6] =  -91.5014;
a[3][2] =   -2.9249;
a[3][7] =    2.9249;
a[3][3] =   58.4415;
a[3][8] =  -58.4415;
a[3][4] =   86.7986;
a[3][9] =  -86.7986;
a[4][0] =   26.4718;
a[4][5] =  -26.4718;
a[4][1] =   92.9777;
a[4][6] =  -92.9777;
a[4][2] =   60.0561;
a[4][7] =  -60.0561;
a[4][3] =   91.8985;
a[4][8] =  -91.8985;
a[4][4] =   35.7470;
a[4][9] =  -35.7470;
 f_val = 0;
 double f_temp;
 for (i=0; i< 2*n; i++)
 {
 f_temp = 0;
 for (j=0; j< n; j++)
 {
 f_temp = f_temp + a[j][i]*x[j];
 }
 if (fabs(f_temp) > f_val)
 f_val = f_temp;
 }
 FILE *file_output;
 file_output = fopen("output.out","w");
 fprintf(file_output, "%30.15f\n" ,f_val);
    fclose(file_output);


 return 0;
 }