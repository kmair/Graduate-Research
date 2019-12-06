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
 n = 25;
 m = 20;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double A[m][n];
A[0][0] =   -0.1898;
A[0][1] =    0.1209;
A[0][2] =    0.6786;
A[0][3] =   -0.6670;
A[0][4] =    0.7167;
A[0][5] =    0.7609;
A[0][6] =   -0.2091;
A[0][7] =   -1.1565;
A[0][8] =   -2.3862;
A[0][9] =    0.5698;
A[0][10] =    1.7833;
A[0][11] =    1.8188;
A[0][12] =    2.9421;
A[0][13] =    1.1244;
A[0][14] =    1.8521;
A[0][15] =   -0.0582;
A[0][16] =   -3.3317;
A[0][17] =   -2.2052;
A[0][18] =   -1.4124;
A[0][19] =    0.4328;
A[0][20] =    0.0489;
A[0][21] =   -2.3858;
A[0][22] =   -0.2667;
A[0][23] =    0.4113;
A[0][24] =   -0.3826;
A[1][0] =    0.8076;
A[1][1] =   -0.3407;
A[1][2] =   -0.0106;
A[1][3] =   -0.4465;
A[1][4] =   -0.2314;
A[1][5] =   -0.5796;
A[1][6] =   -1.3851;
A[1][7] =    1.6139;
A[1][8] =    0.8757;
A[1][9] =   -1.4084;
A[1][10] =    2.3607;
A[1][11] =    1.7260;
A[1][12] =   -0.3377;
A[1][13] =    1.1875;
A[1][14] =   -3.9158;
A[1][15] =    0.4961;
A[1][16] =   -1.4157;
A[1][17] =    4.4509;
A[1][18] =   -0.8363;
A[1][19] =   -1.3711;
A[1][20] =   -0.3940;
A[1][21] =   -2.4254;
A[1][22] =   -0.3784;
A[1][23] =   -0.1325;
A[1][24] =   -0.8298;
A[2][0] =   -0.3513;
A[2][1] =    0.4413;
A[2][2] =    0.8679;
A[2][3] =   -0.9386;
A[2][4] =    0.6854;
A[2][5] =   -1.5091;
A[2][6] =    0.2715;
A[2][7] =   -1.0791;
A[2][8] =   -0.2317;
A[2][9] =   -1.8917;
A[2][10] =    0.7608;
A[2][11] =   -1.3880;
A[2][12] =    0.9432;
A[2][13] =    1.4227;
A[2][14] =    0.6824;
A[2][15] =    2.7351;
A[2][16] =    1.3904;
A[2][17] =   -0.8717;
A[2][18] =   -0.5700;
A[2][19] =    1.4318;
A[2][20] =    0.9637;
A[2][21] =    2.2707;
A[2][22] =    0.4839;
A[2][23] =    0.3329;
A[2][24] =    0.7537;
A[3][0] =    0.1558;
A[3][1] =   -0.2916;
A[3][2] =    0.5767;
A[3][3] =   -0.9687;
A[3][4] =    0.5382;
A[3][5] =    0.3544;
A[3][6] =    1.2402;
A[3][7] =   -1.0143;
A[3][8] =    1.5966;
A[3][9] =    0.1594;
A[3][10] =   -2.6606;
A[3][11] =    1.6940;
A[3][12] =   -0.2789;
A[3][13] =   -3.5186;
A[3][14] =    3.4294;
A[3][15] =   -2.4756;
A[3][16] =    4.1390;
A[3][17] =   -2.7781;
A[3][18] =   -0.4336;
A[3][19] =   -1.0682;
A[3][20] =   -0.6433;
A[3][21] =    2.6618;
A[3][22] =   -0.4244;
A[3][23] =   -0.2187;
A[3][24] =    0.1814;
A[4][0] =    0.1204;
A[4][1] =    0.6845;
A[4][2] =   -0.7106;
A[4][3] =   -0.4684;
A[4][4] =    1.3153;
A[4][5] =   -0.4366;
A[4][6] =   -0.9780;
A[4][7] =   -1.7657;
A[4][8] =   -0.7459;
A[4][9] =    2.2738;
A[4][10] =   -1.7458;
A[4][11] =    0.7312;
A[4][12] =    0.3690;
A[4][13] =   -2.2017;
A[4][14] =   -3.3271;
A[4][15] =    0.2312;
A[4][16] =   -3.8970;
A[4][17] =    1.2659;
A[4][18] =   -1.2282;
A[4][19] =    2.1346;
A[4][20] =    0.9214;
A[4][21] =   -0.6165;
A[4][22] =   -0.4347;
A[4][23] =   -0.2333;
A[4][24] =   -0.3938;
A[5][0] =    0.1955;
A[5][1] =   -0.2970;
A[5][2] =   -0.5078;
A[5][3] =   -0.7001;
A[5][4] =   -0.5544;
A[5][5] =    0.4562;
A[5][6] =   -1.3527;
A[5][7] =   -0.7163;
A[5][8] =   -2.5012;
A[5][9] =    0.6177;
A[5][10] =   -2.6175;
A[5][11] =   -1.5136;
A[5][12] =   -3.1484;
A[5][13] =   -2.6481;
A[5][14] =    0.4446;
A[5][15] =    2.5646;
A[5][16] =    2.2881;
A[5][17] =   -4.7559;
A[5][18] =   -0.6362;
A[5][19] =   -1.0137;
A[5][20] =    0.2220;
A[5][21] =   -1.0573;
A[5][22] =    0.1597;
A[5][23] =    0.4091;
A[5][24] =   -0.6639;
A[6][0] =    0.1028;
A[6][1] =    0.5285;
A[6][2] =   -0.7209;
A[6][3] =    0.3763;
A[6][4] =   -0.4552;
A[6][5] =    1.0606;
A[6][6] =   -1.0412;
A[6][7] =   -0.8102;
A[6][8] =   -1.4829;
A[6][9] =    1.0034;
A[6][10] =    2.4059;
A[6][11] =   -0.6704;
A[6][12] =    2.0739;
A[6][13] =    3.4178;
A[6][14] =    3.5103;
A[6][15] =   -1.2072;
A[6][16] =   -4.2617;
A[6][17] =   -4.4772;
A[6][18] =   -0.7596;
A[6][19] =    0.1826;
A[6][20] =    0.5882;
A[6][21] =    0.3024;
A[6][22] =   -0.1541;
A[6][23] =    0.3758;
A[6][24] =   -0.3202;
A[7][0] =   -0.3715;
A[7][1] =    0.5442;
A[7][2] =    0.0661;
A[7][3] =    0.8550;
A[7][4] =   -0.3261;
A[7][5] =   -1.2220;
A[7][6] =    0.5600;
A[7][7] =    0.3106;
A[7][8] =    1.1114;
A[7][9] =   -1.4034;
A[7][10] =   -0.0859;
A[7][11] =   -0.2638;
A[7][12] =    1.1770;
A[7][13] =    0.1215;
A[7][14] =   -2.4489;
A[7][15] =    0.3979;
A[7][16] =    2.4642;
A[7][17] =   -3.1345;
A[7][18] =    1.1115;
A[7][19] =   -0.8367;
A[7][20] =   -0.4195;
A[7][21] =    1.6564;
A[7][22] =   -0.3671;
A[7][23] =    0.1151;
A[7][24] =   -0.8651;
A[8][0] =   -0.5875;
A[8][1] =   -0.8031;
A[8][2] =   -0.8585;
A[8][3] =   -0.3145;
A[8][4] =   -1.3676;
A[8][5] =    0.2707;
A[8][6] =    0.7755;
A[8][7] =   -0.3716;
A[8][8] =    0.2578;
A[8][9] =   -1.5491;
A[8][10] =    1.6304;
A[8][11] =    0.0032;
A[8][12] =   -2.0566;
A[8][13] =    1.3827;
A[8][14] =    0.2063;
A[8][15] =    1.8116;
A[8][16] =   -0.3499;
A[8][17] =   -2.9365;
A[8][18] =   -0.5483;
A[8][19] =   -1.3214;
A[8][20] =   -1.3155;
A[8][21] =    1.6920;
A[8][22] =    0.1355;
A[8][23] =    0.0034;
A[8][24] =    0.3070;
A[9][0] =    0.3932;
A[9][1] =   -0.1741;
A[9][2] =   -0.0609;
A[9][3] =   -0.8495;
A[9][4] =    0.3854;
A[9][5] =    0.2525;
A[9][6] =    0.4371;
A[9][7] =   -1.1266;
A[9][8] =    1.9480;
A[9][9] =   -2.4447;
A[9][10] =   -0.2387;
A[9][11] =    0.2287;
A[9][12] =   -3.3313;
A[9][13] =    1.3383;
A[9][14] =   -3.0414;
A[9][15] =   -2.4786;
A[9][16] =   -4.3600;
A[9][17] =    2.1505;
A[9][18] =    1.2916;
A[9][19] =    0.2276;
A[9][20] =   -0.8245;
A[9][21] =    2.0603;
A[9][22] =    0.2436;
A[9][23] =   -0.1182;
A[9][24] =   -0.5114;
A[10][0] =   -0.7767;
A[10][1] =    0.8306;
A[10][2] =    0.9348;
A[10][3] =    0.6225;
A[10][4] =    0.1883;
A[10][5] =    0.0668;
A[10][6] =   -0.1061;
A[10][7] =   -0.3851;
A[10][8] =    1.2585;
A[10][9] =    0.8633;
A[10][10] =   -0.0320;
A[10][11] =   -0.8729;
A[10][12] =    1.8060;
A[10][13] =   -3.3381;
A[10][14] =    2.0267;
A[10][15] =    1.1977;
A[10][16] =    3.2775;
A[10][17] =   -0.5601;
A[10][18] =   -0.8054;
A[10][19] =   -1.9733;
A[10][20] =    0.0198;
A[10][21] =    1.6900;
A[10][22] =    0.3016;
A[10][23] =    0.2236;
A[10][24] =    0.5158;
A[11][0] =    0.5090;
A[11][1] =   -0.1529;
A[11][2] =    0.3529;
A[11][3] =   -0.0671;
A[11][4] =   -0.3882;
A[11][5] =   -0.1392;
A[11][6] =   -0.6194;
A[11][7] =   -1.2409;
A[11][8] =   -2.0907;
A[11][9] =    0.0383;
A[11][10] =   -0.4109;
A[11][11] =   -1.1757;
A[11][12] =    0.4633;
A[11][13] =   -2.1927;
A[11][14] =   -1.5101;
A[11][15] =   -4.1651;
A[11][16] =   -3.2129;
A[11][17] =    3.3870;
A[11][18] =    0.9563;
A[11][19] =   -1.8251;
A[11][20] =   -1.2754;
A[11][21] =   -2.7997;
A[11][22] =   -0.2282;
A[11][23] =    0.2369;
A[11][24] =   -0.4020;
A[12][0] =   -0.1579;
A[12][1] =   -0.3026;
A[12][2] =    0.7183;
A[12][3] =    0.3980;
A[12][4] =   -0.2267;
A[12][5] =    1.4306;
A[12][6] =    0.4720;
A[12][7] =    1.1938;
A[12][8] =    2.4841;
A[12][9] =   -0.0750;
A[12][10] =    2.1668;
A[12][11] =    1.1473;
A[12][12] =    1.0583;
A[12][13] =   -1.7212;
A[12][14] =   -2.5490;
A[12][15] =    0.1098;
A[12][16] =   -3.6886;
A[12][17] =   -1.0804;
A[12][18] =    0.3792;
A[12][19] =   -2.0400;
A[12][20] =    0.1341;
A[12][21] =    2.3816;
A[12][22] =   -0.0652;
A[12][23] =   -0.3925;
A[12][24] =   -0.1624;
A[13][0] =    0.5569;
A[13][1] =    0.2435;
A[13][2] =   -0.0073;
A[13][3] =    0.3890;
A[13][4] =   -0.1091;
A[13][5] =   -0.9739;
A[13][6] =    0.2070;
A[13][7] =   -1.2583;
A[13][8] =    1.2579;
A[13][9] =    1.9113;
A[13][10] =    2.0701;
A[13][11] =    2.4470;
A[13][12] =    0.9247;
A[13][13] =    3.3000;
A[13][14] =    3.9144;
A[13][15] =    0.5818;
A[13][16] =   -2.1890;
A[13][17] =    3.3092;
A[13][18] =   -1.6737;
A[13][19] =   -0.9512;
A[13][20] =   -0.6000;
A[13][21] =   -2.7340;
A[13][22] =   -0.0970;
A[13][23] =   -0.3729;
A[13][24] =   -0.8883;
A[14][0] =    0.7948;
A[14][1] =    0.6393;
A[14][2] =   -0.4112;
A[14][3] =   -0.1669;
A[14][4] =   -0.5356;
A[14][5] =    0.1821;
A[14][6] =    0.5463;
A[14][7] =    0.6450;
A[14][8] =   -2.5616;
A[14][9] =    2.4599;
A[14][10] =    0.7364;
A[14][11] =    0.5309;
A[14][12] =    1.6081;
A[14][13] =   -2.1606;
A[14][14] =    1.7342;
A[14][15] =   -4.1072;
A[14][16] =   -1.7786;
A[14][17] =    2.4354;
A[14][18] =    0.6997;
A[14][19] =    1.5725;
A[14][20] =    0.5261;
A[14][21] =   -2.6691;
A[14][22] =    0.1069;
A[14][23] =   -0.1471;
A[14][24] =   -0.9383;
A[15][0] =   -0.1308;
A[15][1] =    0.5147;
A[15][2] =   -0.6139;
A[15][3] =    0.3385;
A[15][4] =   -0.7918;
A[15][5] =    0.3877;
A[15][6] =   -1.2517;
A[15][7] =    0.8985;
A[15][8] =    0.9439;
A[15][9] =    1.3723;
A[15][10] =   -1.1489;
A[15][11] =    0.5248;
A[15][12] =    2.6581;
A[15][13] =   -2.0049;
A[15][14] =    2.6535;
A[15][15] =    3.2700;
A[15][16] =    1.0166;
A[15][17] =    0.8221;
A[15][18] =   -1.6814;
A[15][19] =    0.0682;
A[15][20] =    1.0223;
A[15][21] =    1.1993;
A[15][22] =    0.2669;
A[15][23] =    0.2497;
A[15][24] =   -0.6268;
A[16][0] =   -0.7527;
A[16][1] =    0.0686;
A[16][2] =    0.8932;
A[16][3] =    0.2835;
A[16][4] =   -1.1085;
A[16][5] =   -1.4403;
A[16][6] =    0.5807;
A[16][7] =   -1.7428;
A[16][8] =   -0.0764;
A[16][9] =   -1.3386;
A[16][10] =   -1.1522;
A[16][11] =    1.3458;
A[16][12] =    1.1386;
A[16][13] =    1.4060;
A[16][14] =    1.4929;
A[16][15] =    2.7761;
A[16][16] =    1.7006;
A[16][17] =   -3.3275;
A[16][18] =    0.6385;
A[16][19] =    1.8434;
A[16][20] =   -1.2742;
A[16][21] =    0.2179;
A[16][22] =    0.4359;
A[16][23] =    0.1085;
A[16][24] =   -0.4582;
A[17][0] =    0.4843;
A[17][1] =    0.4315;
A[17][2] =    0.4963;
A[17][3] =    0.0534;
A[17][4] =   -1.0565;
A[17][5] =    0.4361;
A[17][6] =    0.6016;
A[17][7] =   -1.2853;
A[17][8] =   -2.4517;
A[17][9] =    0.9314;
A[17][10] =   -1.4076;
A[17][11] =   -0.7179;
A[17][12] =   -2.6032;
A[17][13] =    1.1843;
A[17][14] =    1.5433;
A[17][15] =    2.2926;
A[17][16] =    4.6728;
A[17][17] =    0.2827;
A[17][18] =    1.3121;
A[17][19] =    2.4168;
A[17][20] =   -0.4257;
A[17][21] =    0.1395;
A[17][22] =   -0.3641;
A[17][23] =    0.4022;
A[17][24] =   -0.8675;
A[18][0] =    0.2195;
A[18][1] =    0.4280;
A[18][2] =    0.8722;
A[18][3] =   -0.6559;
A[18][4] =    1.2888;
A[18][5] =    1.0366;
A[18][6] =    1.4352;
A[18][7] =   -1.6862;
A[18][8] =   -2.0316;
A[18][9] =    1.3702;
A[18][10] =    1.1302;
A[18][11] =   -0.2634;
A[18][12] =    1.4668;
A[18][13] =    1.6245;
A[18][14] =    0.2516;
A[18][15] =    2.4007;
A[18][16] =    4.7267;
A[18][17] =   -0.3660;
A[18][18] =   -0.2368;
A[18][19] =   -0.8719;
A[18][20] =   -0.2107;
A[18][21] =    0.0127;
A[18][22] =   -0.2414;
A[18][23] =   -0.2847;
A[18][24] =   -0.3330;
A[19][0] =    0.0149;
A[19][1] =   -0.0986;
A[19][2] =    0.3317;
A[19][3] =    0.0253;
A[19][4] =   -0.0218;
A[19][5] =   -0.3215;
A[19][6] =    1.0972;
A[19][7] =    1.1090;
A[19][8] =   -2.2309;
A[19][9] =    0.4554;
A[19][10] =    1.5188;
A[19][11] =    0.0529;
A[19][12] =   -0.3622;
A[19][13] =    2.6886;
A[19][14] =   -2.5353;
A[19][15] =   -1.0529;
A[19][16] =    2.4678;
A[19][17] =   -1.1809;
A[19][18] =    0.4560;
A[19][19] =   -1.4103;
A[19][20] =   -1.1259;
A[19][21] =   -2.4634;
A[19][22] =   -0.1202;
A[19][23] =    0.0972;
A[19][24] =   -0.9002;
 double b[m];
b[0] =    2.5140;
b[1] =  -10.0108;
b[2] =    3.2278;
b[3] =    4.4648;
b[4] =   -2.4799;
b[5] =   11.9784;
b[6] =    5.9374;
b[7] =    2.1763;
b[8] =    1.5065;
b[9] =  -16.2586;
b[10] =   10.0241;
b[11] =   -6.4334;
b[12] =   -3.9587;
b[13] =   -4.3147;
b[14] =   -0.6810;
b[15] =   10.6821;
b[16] =    4.4086;
b[17] =    7.1488;
b[18] =    9.2219;
b[19] =   -0.5742;
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