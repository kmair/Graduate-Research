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
 int m;
 n = 15;
 m = 12;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double A[m][n];
A[0][0] =   -0.0701;
A[0][1] =   -0.2592;
A[0][2] =    0.4220;
A[0][3] =    0.6874;
A[0][4] =   -1.2011;
A[0][5] =    0.0589;
A[0][6] =    2.4411;
A[0][7] =   -3.6287;
A[0][8] =    6.4235;
A[0][9] =    6.2556;
A[0][10] =    6.8924;
A[0][11] =   -0.0632;
A[0][12] =   -0.8315;
A[0][13] =    0.9051;
A[0][14] =   -0.4044;
A[1][0] =    0.5116;
A[1][1] =    0.5206;
A[1][2] =    0.9833;
A[1][3] =    0.0229;
A[1][4] =    1.4792;
A[1][5] =    1.0779;
A[1][6] =   -0.2566;
A[1][7] =    3.9359;
A[1][8] =    5.3735;
A[1][9] =   -2.5621;
A[1][10] =   10.2682;
A[1][11] =    0.0068;
A[1][12] =    0.1265;
A[1][13] =   -0.4036;
A[1][14] =   -0.7500;
A[2][0] =   -0.1605;
A[2][1] =    0.0520;
A[2][2] =   -0.7362;
A[2][3] =   -0.4026;
A[2][4] =   -0.6488;
A[2][5] =    2.0886;
A[2][6] =   -1.5822;
A[2][7] =   -3.3705;
A[2][8] =    0.1100;
A[2][9] =    4.5038;
A[2][10] =    3.1041;
A[2][11] =    0.8211;
A[2][12] =    0.0786;
A[2][13] =   -0.6832;
A[2][14] =   -0.2233;
A[3][0] =    0.3492;
A[3][1] =    0.0347;
A[3][2] =   -0.4936;
A[3][3] =   -1.4750;
A[3][4] =   -0.6108;
A[3][5] =    1.1304;
A[3][6] =    1.5581;
A[3][7] =    0.3946;
A[3][8] =    4.1782;
A[3][9] =    0.5666;
A[3][10] =    4.2276;
A[3][11] =   -0.5871;
A[3][12] =    0.5361;
A[3][13] =   -0.2774;
A[3][14] =    0.6354;
A[4][0] =    0.6980;
A[4][1] =    0.4503;
A[4][2] =   -0.0085;
A[4][3] =   -0.4795;
A[4][4] =   -1.0614;
A[4][5] =    1.2168;
A[4][6] =    4.0441;
A[4][7] =   -0.8151;
A[4][8] =   -0.3901;
A[4][9] =   -0.5437;
A[4][10] =    8.9608;
A[4][11] =   -0.3228;
A[4][12] =   -0.5338;
A[4][13] =    0.4833;
A[4][14] =    0.9624;
A[5][0] =    0.0341;
A[5][1] =   -0.6234;
A[5][2] =    0.9541;
A[5][3] =   -0.9010;
A[5][4] =    0.9589;
A[5][5] =   -0.4514;
A[5][6] =    2.2422;
A[5][7] =   -3.3010;
A[5][8] =    4.7565;
A[5][9] =    4.7509;
A[5][10] =    3.8897;
A[5][11] =    0.1483;
A[5][12] =    0.1747;
A[5][13] =    0.4118;
A[5][14] =    0.7240;
A[6][0] =   -0.1027;
A[6][1] =    0.2059;
A[6][2] =    0.0035;
A[6][3] =   -1.0052;
A[6][4] =   -1.5432;
A[6][5] =   -1.2688;
A[6][6] =    2.7377;
A[6][7] =    1.8830;
A[6][8] =   -2.5680;
A[6][9] =    6.6936;
A[6][10] =    1.4168;
A[6][11] =   -0.0261;
A[6][12] =   -0.0821;
A[6][13] =    0.4018;
A[6][14] =   -0.8324;
A[7][0] =   -0.4290;
A[7][1] =   -0.2517;
A[7][2] =   -0.5457;
A[7][3] =    1.0638;
A[7][4] =    1.6149;
A[7][5] =    0.1015;
A[7][6] =    1.7119;
A[7][7] =    0.9550;
A[7][8] =    6.8881;
A[7][9] =   -6.2762;
A[7][10] =   -2.4698;
A[7][11] =   -0.4756;
A[7][12] =    0.7220;
A[7][13] =   -0.9875;
A[7][14] =   -0.3246;
A[8][0] =   -0.2587;
A[8][1] =   -0.2715;
A[8][2] =    0.0831;
A[8][3] =   -0.3613;
A[8][4] =   -1.2265;
A[8][5] =   -1.3682;
A[8][6] =    0.7919;
A[8][7] =    2.3770;
A[8][8] =   -3.2085;
A[8][9] =    3.0908;
A[8][10] =    2.7896;
A[8][11] =    0.1592;
A[8][12] =    0.3217;
A[8][13] =   -0.2513;
A[8][14] =   -0.5277;
A[9][0] =   -0.5701;
A[9][1] =    0.1225;
A[9][2] =    0.1817;
A[9][3] =   -1.5135;
A[9][4] =   -0.7663;
A[9][5] =    1.6675;
A[9][6] =    2.1005;
A[9][7] =    0.5596;
A[9][8] =   -6.1796;
A[9][9] =   -3.9262;
A[9][10] =   -2.8350;
A[9][11] =    0.7567;
A[9][12] =   -0.2922;
A[9][13] =    0.8030;
A[9][14] =   -0.3644;
A[10][0] =   -0.1828;
A[10][1] =    0.5974;
A[10][2] =   -0.2134;
A[10][3] =   -0.4236;
A[10][4] =    0.6715;
A[10][5] =    0.7935;
A[10][6] =   -0.0272;
A[10][7] =    2.6152;
A[10][8] =    3.6343;
A[10][9] =   -1.4975;
A[10][10] =   -1.9149;
A[10][11] =   -0.8781;
A[10][12] =   -0.3056;
A[10][13] =   -0.3633;
A[10][14] =    0.9689;
A[11][0] =   -0.2492;
A[11][1] =   -0.4948;
A[11][2] =   -1.1874;
A[11][3] =    0.9323;
A[11][4] =   -1.2877;
A[11][5] =    1.5402;
A[11][6] =    3.0330;
A[11][7] =    0.6449;
A[11][8] =    4.7911;
A[11][9] =   -3.4345;
A[11][10] =   -2.7216;
A[11][11] =   -0.1182;
A[11][12] =   -0.4926;
A[11][13] =    0.1942;
A[11][14] =    0.0965;
 double b[m];
b[0] =    8.0572;
b[1] =    7.3938;
b[2] =    1.0935;
b[3] =    4.8602;
b[4] =    3.5359;
b[5] =    8.7284;
b[6] =    5.0301;
b[7] =    0.0408;
b[8] =    0.9050;
b[9] =   -4.8234;
b[10] =    2.2811;
b[11] =   -1.6703;
 f_val = 0;
 double f_temp;
 double f_temp2;
 f_temp2 = 0;
 for (i=0; i< m; i++)
 {
 f_temp = 0;
 for (j=0; j< n; j++)
 {
 f_temp = f_temp + A[i][j]*x[j];
 }
 f_temp = f_temp - b[i];
 f_temp2 = f_temp2 + f_temp*f_temp;
 }
 f_temp2 = 0.5*f_temp2;
 for (i=0; i< n; i++)
 {
 f_temp2 = f_temp2 + fabs(x[i]);
 }
 f_val = f_temp2;
 FILE *file_output;
 file_output = fopen("output.out","w");
 fprintf(file_output, "%30.15f\n" ,f_val);
    fclose(file_output);


 return 0;
 }