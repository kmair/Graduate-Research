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
A[0][0] =    0.2032;
A[0][1] =   -0.6227;
A[0][2] =   -0.3393;
A[0][3] =    0.6119;
A[0][4] =    0.0788;
A[0][5] =   -0.5374;
A[0][6] =   -0.5131;
A[0][7] =    1.5600;
A[0][8] =    0.0090;
A[0][9] =    1.8385;
A[0][10] =   -0.8486;
A[0][11] =   -0.0830;
A[0][12] =    1.0913;
A[0][13] =    0.0804;
A[0][14] =   -0.4485;
A[1][0] =    0.7437;
A[1][1] =   -0.6424;
A[1][2] =    0.5173;
A[1][3] =    0.1124;
A[1][4] =   -1.2374;
A[1][5] =   -1.2466;
A[1][6] =    0.4110;
A[1][7] =   -1.0236;
A[1][8] =    2.0507;
A[1][9] =   -2.0632;
A[1][10] =    0.3823;
A[1][11] =   -0.3458;
A[1][12] =   -0.4692;
A[1][13] =   -1.5256;
A[1][14] =   -0.0382;
A[2][0] =    0.5766;
A[2][1] =    0.1912;
A[2][2] =   -0.5102;
A[2][3] =   -0.4674;
A[2][4] =    0.7086;
A[2][5] =    0.7637;
A[2][6] =    0.9927;
A[2][7] =   -1.4730;
A[2][8] =   -0.5938;
A[2][9] =    0.4606;
A[2][10] =    1.4922;
A[2][11] =    0.3407;
A[2][12] =   -0.2992;
A[2][13] =    1.5064;
A[2][14] =   -0.3709;
A[3][0] =    0.6805;
A[3][1] =   -0.4429;
A[3][2] =    0.4612;
A[3][3] =    0.3561;
A[3][4] =    0.2813;
A[3][5] =    0.7667;
A[3][6] =    0.8229;
A[3][7] =   -1.0934;
A[3][8] =   -1.8601;
A[3][9] =    1.2571;
A[3][10] =   -0.9477;
A[3][11] =    1.0297;
A[3][12] =    1.9311;
A[3][13] =    1.4309;
A[3][14] =    0.6825;
A[4][0] =    0.1420;
A[4][1] =   -0.3172;
A[4][2] =    0.3700;
A[4][3] =   -0.5252;
A[4][4] =    0.9853;
A[4][5] =    0.1336;
A[4][6] =   -1.2532;
A[4][7] =   -0.5965;
A[4][8] =   -1.1760;
A[4][9] =   -1.1149;
A[4][10] =   -0.4407;
A[4][11] =   -1.3532;
A[4][12] =    0.3088;
A[4][13] =   -0.2271;
A[4][14] =    1.1147;
A[5][0] =    0.1424;
A[5][1] =   -0.1772;
A[5][2] =    0.6219;
A[5][3] =   -0.0859;
A[5][4] =    1.3470;
A[5][5] =   -0.4994;
A[5][6] =    1.0790;
A[5][7] =   -0.8467;
A[5][8] =   -0.4196;
A[5][9] =    1.8056;
A[5][10] =    1.6376;
A[5][11] =    1.1014;
A[5][12] =    1.4799;
A[5][13] =    1.0487;
A[5][14] =   -0.7796;
A[6][0] =    0.6108;
A[6][1] =   -0.1689;
A[6][2] =    0.6294;
A[6][3] =   -0.2276;
A[6][4] =    1.1848;
A[6][5] =    1.0253;
A[6][6] =   -1.5892;
A[6][7] =    1.7000;
A[6][8] =   -0.6831;
A[6][9] =    1.0992;
A[6][10] =    0.5190;
A[6][11] =    1.1687;
A[6][12] =   -0.9464;
A[6][13] =   -1.3086;
A[6][14] =   -0.6016;
A[7][0] =   -0.8004;
A[7][1] =   -0.2053;
A[7][2] =   -0.3965;
A[7][3] =    0.0761;
A[7][4] =   -0.2496;
A[7][5] =    0.1622;
A[7][6] =   -0.2864;
A[7][7] =   -1.7843;
A[7][8] =   -1.1105;
A[7][9] =    1.3630;
A[7][10] =    2.2217;
A[7][11] =   -0.4985;
A[7][12] =    0.5176;
A[7][13] =    0.4460;
A[7][14] =    0.4624;
A[8][0] =    0.6633;
A[8][1] =    0.1981;
A[8][2] =   -0.3658;
A[8][3] =    0.9697;
A[8][4] =   -1.3784;
A[8][5] =    1.4116;
A[8][6] =    0.7745;
A[8][7] =    0.3685;
A[8][8] =    1.7911;
A[8][9] =    0.3067;
A[8][10] =   -1.3402;
A[8][11] =   -0.2396;
A[8][12] =    0.3739;
A[8][13] =   -0.8911;
A[8][14] =   -0.2847;
A[9][0] =   -0.1588;
A[9][1] =   -0.6010;
A[9][2] =    0.0442;
A[9][3] =    0.5033;
A[9][4] =    0.1128;
A[9][5] =    1.2120;
A[9][6] =    0.9417;
A[9][7] =   -1.3406;
A[9][8] =    0.7523;
A[9][9] =    1.2216;
A[9][10] =    1.4800;
A[9][11] =    0.2475;
A[9][12] =    1.9613;
A[9][13] =   -0.2023;
A[9][14] =    1.0832;
A[10][0] =   -0.7978;
A[10][1] =   -0.5627;
A[10][2] =   -0.3352;
A[10][3] =    0.9475;
A[10][4] =   -0.8063;
A[10][5] =   -0.4427;
A[10][6] =   -0.4442;
A[10][7] =    1.3458;
A[10][8] =    1.8979;
A[10][9] =   -0.7138;
A[10][10] =    0.7954;
A[10][11] =    0.6881;
A[10][12] =   -1.7523;
A[10][13] =    1.2806;
A[10][14] =    1.0800;
A[11][0] =    0.4236;
A[11][1] =   -0.7313;
A[11][2] =    0.6368;
A[11][3] =   -0.5230;
A[11][4] =   -0.7744;
A[11][5] =    0.1432;
A[11][6] =    0.8195;
A[11][7] =   -1.3220;
A[11][8] =   -0.2547;
A[11][9] =   -1.1546;
A[11][10] =   -1.1355;
A[11][11] =    0.8308;
A[11][12] =    0.0021;
A[11][13] =   -1.1309;
A[11][14] =    0.3286;
 double b[m];
b[0] =    0.9798;
b[1] =   -2.4259;
b[2] =    3.4334;
b[3] =    3.7550;
b[4] =    0.8995;
b[5] =    4.9602;
b[6] =    4.3479;
b[7] =    2.7878;
b[8] =   -1.1203;
b[9] =    3.6479;
b[10] =   -1.8492;
b[11] =   -1.3007;
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
