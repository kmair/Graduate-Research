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
 n = 20;
 m = 16;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double A[m][n];
A[0][0] =    0.7268;
A[0][1] =   -0.9846;
A[0][2] =   -1.5498;
A[0][3] =   -0.0018;
A[0][4] =   -1.3374;
A[0][5] =    0.4976;
A[0][6] =    0.9001;
A[0][7] =    1.9099;
A[0][8] =   -0.6669;
A[0][9] =    1.9301;
A[0][10] =   -1.6856;
A[0][11] =   -2.4367;
A[0][12] =   -2.5819;
A[0][13] =   -3.4736;
A[0][14] =   -0.5994;
A[0][15] =    0.0016;
A[0][16] =   -0.4837;
A[0][17] =   -0.1776;
A[0][18] =    0.9938;
A[0][19] =    0.4554;
A[1][0] =    0.5720;
A[1][1] =    0.8244;
A[1][2] =   -0.3151;
A[1][3] =    1.5422;
A[1][4] =    0.3926;
A[1][5] =    0.6740;
A[1][6] =   -1.9854;
A[1][7] =   -0.4112;
A[1][8] =    2.1959;
A[1][9] =    2.2743;
A[1][10] =    0.9889;
A[1][11] =    1.3681;
A[1][12] =   -0.3497;
A[1][13] =   -4.8997;
A[1][14] =    1.7398;
A[1][15] =    0.0002;
A[1][16] =    0.4657;
A[1][17] =   -0.5270;
A[1][18] =   -0.2821;
A[1][19] =    0.3020;
A[2][0] =    1.0134;
A[2][1] =    1.1866;
A[2][2] =   -0.8344;
A[2][3] =   -0.7314;
A[2][4] =    1.7092;
A[2][5] =   -1.7794;
A[2][6] =    1.8148;
A[2][7] =    0.7397;
A[2][8] =   -1.3993;
A[2][9] =    2.6399;
A[2][10] =    0.3941;
A[2][11] =   -2.5814;
A[2][12] =    0.5108;
A[2][13] =    4.7284;
A[2][14] =   -3.8018;
A[2][15] =    0.0010;
A[2][16] =   -0.7665;
A[2][17] =   -0.6099;
A[2][18] =    0.2505;
A[2][19] =    0.3292;
A[3][0] =   -0.8228;
A[3][1] =   -1.3660;
A[3][2] =   -0.0637;
A[3][3] =   -0.4294;
A[3][4] =   -1.5326;
A[3][5] =   -0.0182;
A[3][6] =    2.1575;
A[3][7] =    1.8741;
A[3][8] =    2.3364;
A[3][9] =   -0.6473;
A[3][10] =   -1.3033;
A[3][11] =    3.1030;
A[3][12] =    2.0640;
A[3][13] =    1.8721;
A[3][14] =    0.3618;
A[3][15] =    0.0016;
A[3][16] =    0.4921;
A[3][17] =    0.4108;
A[3][18] =   -0.2133;
A[3][19] =    0.8776;
A[4][0] =   -0.2200;
A[4][1] =    0.4138;
A[4][2] =    1.2693;
A[4][3] =   -1.3437;
A[4][4] =    1.5628;
A[4][5] =   -0.9053;
A[4][6] =   -1.9650;
A[4][7] =   -1.2915;
A[4][8] =    2.2537;
A[4][9] =    1.7860;
A[4][10] =    0.7657;
A[4][11] =    2.6888;
A[4][12] =   -0.5673;
A[4][13] =    3.7439;
A[4][14] =    1.5391;
A[4][15] =    0.0014;
A[4][16] =    0.6196;
A[4][17] =   -0.6389;
A[4][18] =   -0.9847;
A[4][19] =    0.0702;
A[5][0] =   -0.0168;
A[5][1] =    0.4924;
A[5][2] =   -0.7310;
A[5][3] =    1.6172;
A[5][4] =    0.1972;
A[5][5] =    0.3753;
A[5][6] =    1.6848;
A[5][7] =   -0.5575;
A[5][8] =   -0.5797;
A[5][9] =   -2.0324;
A[5][10] =    1.3517;
A[5][11] =    0.1044;
A[5][12] =    0.2300;
A[5][13] =    4.5594;
A[5][14] =   -5.5894;
A[5][15] =   -0.0015;
A[5][16] =    0.4905;
A[5][17] =    0.0447;
A[5][18] =    0.0906;
A[5][19] =   -0.2031;
A[6][0] =   -0.5122;
A[6][1] =   -0.6795;
A[6][2] =    0.3304;
A[6][3] =   -0.2348;
A[6][4] =   -0.2838;
A[6][5] =    2.1189;
A[6][6] =    1.9941;
A[6][7] =    0.3049;
A[6][8] =    0.7141;
A[6][9] =    0.7419;
A[6][10] =   -1.3728;
A[6][11] =    1.2484;
A[6][12] =    3.0088;
A[6][13] =    1.8711;
A[6][14] =   -0.3408;
A[6][15] =   -0.0013;
A[6][16] =   -0.3257;
A[6][17] =   -0.4077;
A[6][18] =    0.0182;
A[6][19] =    0.3409;
A[7][0] =   -0.9803;
A[7][1] =    0.9465;
A[7][2] =   -1.5791;
A[7][3] =   -1.4453;
A[7][4] =   -1.3582;
A[7][5] =   -0.3490;
A[7][6] =   -2.3802;
A[7][7] =   -0.3155;
A[7][8] =    0.3861;
A[7][9] =    1.1795;
A[7][10] =   -0.5492;
A[7][11] =    3.3177;
A[7][12] =    2.8207;
A[7][13] =    4.3343;
A[7][14] =    4.4440;
A[7][15] =   -0.0014;
A[7][16] =    0.1686;
A[7][17] =   -0.0744;
A[7][18] =   -0.5064;
A[7][19] =   -0.1189;
A[8][0] =    1.0044;
A[8][1] =   -0.5683;
A[8][2] =   -1.1492;
A[8][3] =    0.7538;
A[8][4] =   -0.9864;
A[8][5] =   -2.2300;
A[8][6] =    0.1154;
A[8][7] =   -2.3020;
A[8][8] =    2.2534;
A[8][9] =   -2.4547;
A[8][10] =   -0.8627;
A[8][11] =   -2.6062;
A[8][12] =    0.3169;
A[8][13] =   -2.5264;
A[8][14] =   -4.4399;
A[8][15] =   -0.0019;
A[8][16] =   -0.0621;
A[8][17] =    0.8505;
A[8][18] =   -0.9092;
A[8][19] =   -0.7343;
A[9][0] =    0.4794;
A[9][1] =   -1.3049;
A[9][2] =    1.1129;
A[9][3] =    0.0236;
A[9][4] =   -0.2056;
A[9][5] =    2.0512;
A[9][6] =    0.7323;
A[9][7] =    1.3734;
A[9][8] =   -1.6326;
A[9][9] =    0.1848;
A[9][10] =    1.7617;
A[9][11] =    1.7552;
A[9][12] =    2.8351;
A[9][13] =    4.0041;
A[9][14] =   -0.6610;
A[9][15] =   -0.0020;
A[9][16] =   -0.8255;
A[9][17] =   -0.5682;
A[9][18] =    0.6835;
A[9][19] =   -0.1216;
A[10][0] =   -0.7453;
A[10][1] =   -1.1217;
A[10][2] =   -1.0169;
A[10][3] =   -0.5981;
A[10][4] =    0.1281;
A[10][5] =    2.0299;
A[10][6] =   -0.5595;
A[10][7] =   -2.6267;
A[10][8] =   -2.8339;
A[10][9] =    2.3032;
A[10][10] =   -2.2821;
A[10][11] =    2.2757;
A[10][12] =   -3.1676;
A[10][13] =    4.3615;
A[10][14] =    1.8353;
A[10][15] =    0.0004;
A[10][16] =    0.6574;
A[10][17] =   -0.9980;
A[10][18] =   -0.9035;
A[10][19] =    0.0953;
A[11][0] =   -0.7455;
A[11][1] =    0.8216;
A[11][2] =    1.0992;
A[11][3] =    0.8820;
A[11][4] =   -0.5679;
A[11][5] =   -0.6768;
A[11][6] =    0.7272;
A[11][7] =   -0.3948;
A[11][8] =    2.1738;
A[11][9] =   -1.4008;
A[11][10] =    2.5172;
A[11][11] =    1.9583;
A[11][12] =    2.1812;
A[11][13] =   -2.0761;
A[11][14] =   -2.3608;
A[11][15] =    0.0004;
A[11][16] =    0.3719;
A[11][17] =    0.8132;
A[11][18] =   -0.3674;
A[11][19] =   -0.2097;
A[12][0] =    0.4337;
A[12][1] =    0.5831;
A[12][2] =   -0.5397;
A[12][3] =    1.1689;
A[12][4] =    1.0671;
A[12][5] =   -1.1464;
A[12][6] =    1.2803;
A[12][7] =   -2.6006;
A[12][8] =   -1.5129;
A[12][9] =   -1.5720;
A[12][10] =   -1.3170;
A[12][11] =   -2.1509;
A[12][12] =   -1.1669;
A[12][13] =   -4.4297;
A[12][14] =    5.1807;
A[12][15] =    0.0017;
A[12][16] =   -0.4653;
A[12][17] =    0.3601;
A[12][18] =    0.5668;
A[12][19] =   -0.2035;
A[13][0] =   -0.2520;
A[13][1] =    0.7816;
A[13][2] =    0.5703;
A[13][3] =   -0.8568;
A[13][4] =    1.4913;
A[13][5] =    1.3702;
A[13][6] =    0.3687;
A[13][7] =    2.6739;
A[13][8] =   -1.4573;
A[13][9] =    2.0133;
A[13][10] =   -0.0028;
A[13][11] =   -0.4965;
A[13][12] =   -1.9176;
A[13][13] =    2.7765;
A[13][14] =    2.2349;
A[13][15] =    0.0010;
A[13][16] =    0.9390;
A[13][17] =    0.0299;
A[13][18] =    0.9448;
A[13][19] =    0.5027;
A[14][0] =   -0.8968;
A[14][1] =    0.3418;
A[14][2] =   -1.4920;
A[14][3] =    0.1198;
A[14][4] =    0.9146;
A[14][5] =    0.0553;
A[14][6] =    0.6426;
A[14][7] =    0.1255;
A[14][8] =    0.8049;
A[14][9] =   -0.0264;
A[14][10] =    1.7755;
A[14][11] =   -3.3786;
A[14][12] =    2.2787;
A[14][13] =   -4.3097;
A[14][14] =   -3.3727;
A[14][15] =   -0.0008;
A[14][16] =   -0.6325;
A[14][17] =    0.0441;
A[14][18] =    0.1729;
A[14][19] =    0.0447;
A[15][0] =   -0.1888;
A[15][1] =    0.8975;
A[15][2] =    0.7810;
A[15][3] =   -0.2256;
A[15][4] =   -0.3656;
A[15][5] =    0.2987;
A[15][6] =   -1.0804;
A[15][7] =    2.3021;
A[15][8] =   -1.1165;
A[15][9] =   -2.0606;
A[15][10] =    1.1085;
A[15][11] =   -1.2157;
A[15][12] =   -1.0727;
A[15][13] =    2.3754;
A[15][14] =    0.6299;
A[15][15] =   -0.0000;
A[15][16] =   -0.4001;
A[15][17] =   -0.7942;
A[15][18] =    0.5561;
A[15][19] =   -0.0191;
 double b[m];
b[0] =   -3.4365;
b[1] =   -3.5429;
b[2] =    6.6414;
b[3] =    4.0930;
b[4] =    2.9528;
b[5] =    8.4434;
b[6] =    8.0281;
b[7] =    3.1901;
b[8] =    1.4274;
b[9] =    6.7007;
b[10] =    5.9019;
b[11] =    0.8928;
b[12] =   -9.6068;
b[13] =   -1.5020;
b[14] =    0.1648;
b[15] =   -3.4122;
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
