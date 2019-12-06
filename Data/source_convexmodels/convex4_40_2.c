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
 n = 40;
 m = 32;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double A[m][n];
A[0][0] =   -0.5183;
A[0][1] =   -0.5361;
A[0][2] =    0.0931;
A[0][3] =   -0.5827;
A[0][4] =   -0.8225;
A[0][5] =   -1.4905;
A[0][6] =    0.4604;
A[0][7] =    0.6649;
A[0][8] =   -1.1385;
A[0][9] =   -1.0214;
A[0][10] =    0.3107;
A[0][11] =   -0.3312;
A[0][12] =   -2.0428;
A[0][13] =    2.3630;
A[0][14] =    0.2845;
A[0][15] =   -1.7265;
A[0][16] =   -1.3612;
A[0][17] =    2.9079;
A[0][18] =   -1.6376;
A[0][19] =   -0.3334;
A[0][20] =   -0.0777;
A[0][21] =    3.8627;
A[0][22] =   -0.1248;
A[0][23] =   -0.3914;
A[0][24] =   -1.2190;
A[0][25] =   -4.4521;
A[0][26] =    1.4320;
A[0][27] =    2.8973;
A[0][28] =   -4.4400;
A[0][29] =   -2.8756;
A[0][30] =   -0.8094;
A[0][31] =    0.0775;
A[0][32] =    0.5400;
A[0][33] =    0.1064;
A[0][34] =    0.9551;
A[0][35] =   -0.1013;
A[0][36] =    0.1423;
A[0][37] =   -0.0393;
A[0][38] =    0.0416;
A[0][39] =   -0.1045;
A[1][0] =    0.0066;
A[1][1] =    0.0771;
A[1][2] =    0.5153;
A[1][3] =    0.6391;
A[1][4] =   -0.6178;
A[1][5] =   -1.4053;
A[1][6] =   -0.8015;
A[1][7] =    1.2069;
A[1][8] =   -0.7784;
A[1][9] =    0.3899;
A[1][10] =    1.8771;
A[1][11] =   -1.9844;
A[1][12] =   -0.9080;
A[1][13] =   -0.1182;
A[1][14] =    0.1799;
A[1][15] =   -0.8023;
A[1][16] =   -0.8100;
A[1][17] =   -0.8601;
A[1][18] =   -3.0137;
A[1][19] =    0.1628;
A[1][20] =    0.6319;
A[1][21] =   -1.5569;
A[1][22] =    3.8479;
A[1][23] =   -1.1107;
A[1][24] =   -3.4147;
A[1][25] =   -0.6172;
A[1][26] =   -3.2028;
A[1][27] =   -0.1422;
A[1][28] =   -4.1287;
A[1][29] =   -1.4100;
A[1][30] =   -0.4313;
A[1][31] =    0.4447;
A[1][32] =    0.8904;
A[1][33] =   -0.0516;
A[1][34] =   -0.3623;
A[1][35] =    0.8699;
A[1][36] =   -0.9831;
A[1][37] =    0.5928;
A[1][38] =    0.2610;
A[1][39] =   -0.7387;
A[2][0] =    0.2240;
A[2][1] =    0.0385;
A[2][2] =    0.0842;
A[2][3] =   -0.8197;
A[2][4] =   -0.7877;
A[2][5] =    1.2385;
A[2][6] =   -0.2941;
A[2][7] =   -0.9980;
A[2][8] =    0.6682;
A[2][9] =    1.3827;
A[2][10] =   -1.8147;
A[2][11] =   -1.3042;
A[2][12] =   -1.8515;
A[2][13] =    1.2956;
A[2][14] =   -1.7483;
A[2][15] =   -3.0457;
A[2][16] =   -3.1220;
A[2][17] =   -0.7830;
A[2][18] =    1.8001;
A[2][19] =   -0.0522;
A[2][20] =   -1.1123;
A[2][21] =    1.8863;
A[2][22] =   -3.5322;
A[2][23] =    2.2898;
A[2][24] =   -4.2037;
A[2][25] =    2.9919;
A[2][26] =    2.6434;
A[2][27] =   -0.0429;
A[2][28] =    1.7960;
A[2][29] =   -3.2391;
A[2][30] =    0.5526;
A[2][31] =    0.7359;
A[2][32] =    0.2235;
A[2][33] =   -0.5636;
A[2][34] =    0.6577;
A[2][35] =    0.2097;
A[2][36] =    0.0698;
A[2][37] =    0.1050;
A[2][38] =   -0.6918;
A[2][39] =   -0.9204;
A[3][0] =    0.8050;
A[3][1] =    0.5343;
A[3][2] =   -0.8328;
A[3][3] =   -0.4209;
A[3][4] =   -1.1596;
A[3][5] =    0.9647;
A[3][6] =   -0.9299;
A[3][7] =   -1.5187;
A[3][8] =   -1.4750;
A[3][9] =   -0.2399;
A[3][10] =    1.6970;
A[3][11] =   -1.3461;
A[3][12] =    0.6019;
A[3][13] =    1.0113;
A[3][14] =   -2.1644;
A[3][15] =    1.2728;
A[3][16] =   -2.3095;
A[3][17] =   -2.2016;
A[3][18] =    2.2855;
A[3][19] =   -3.1169;
A[3][20] =   -0.0938;
A[3][21] =   -3.6611;
A[3][22] =    0.8766;
A[3][23] =    0.9148;
A[3][24] =    3.7127;
A[3][25] =    3.0642;
A[3][26] =   -2.4670;
A[3][27] =    3.9608;
A[3][28] =    4.3654;
A[3][29] =   -1.5675;
A[3][30] =   -0.2037;
A[3][31] =    0.6869;
A[3][32] =    0.3592;
A[3][33] =    0.0174;
A[3][34] =   -0.7859;
A[3][35] =   -0.2993;
A[3][36] =   -0.0102;
A[3][37] =    0.0753;
A[3][38] =   -0.9382;
A[3][39] =    0.3627;
A[4][0] =   -0.0279;
A[4][1] =   -0.6481;
A[4][2] =   -0.2742;
A[4][3] =    1.2382;
A[4][4] =    1.1198;
A[4][5] =   -0.9912;
A[4][6] =   -0.2234;
A[4][7] =   -0.4768;
A[4][8] =    0.8683;
A[4][9] =   -0.9483;
A[4][10] =   -1.1434;
A[4][11] =   -1.6216;
A[4][12] =    1.3532;
A[4][13] =   -0.8993;
A[4][14] =   -1.0803;
A[4][15] =    0.2803;
A[4][16] =    0.9128;
A[4][17] =   -3.2666;
A[4][18] =    1.5574;
A[4][19] =    0.4596;
A[4][20] =    1.1095;
A[4][21] =    3.7359;
A[4][22] =    2.8088;
A[4][23] =   -1.4192;
A[4][24] =    2.2882;
A[4][25] =   -2.6829;
A[4][26] =    3.2833;
A[4][27] =    5.7306;
A[4][28] =    3.3384;
A[4][29] =   -0.4429;
A[4][30] =    0.7697;
A[4][31] =    0.8981;
A[4][32] =    0.1579;
A[4][33] =   -0.6299;
A[4][34] =    0.4826;
A[4][35] =   -0.1284;
A[4][36] =   -0.3632;
A[4][37] =   -0.5304;
A[4][38] =    0.7300;
A[4][39] =    0.0293;
A[5][0] =    0.3791;
A[5][1] =    0.1248;
A[5][2] =    0.9174;
A[5][3] =    0.9313;
A[5][4] =   -0.7191;
A[5][5] =   -1.4812;
A[5][6] =    0.2553;
A[5][7] =   -1.1385;
A[5][8] =    1.0450;
A[5][9] =   -0.4149;
A[5][10] =    1.5666;
A[5][11] =    0.6944;
A[5][12] =   -0.6061;
A[5][13] =    2.4730;
A[5][14] =   -1.9366;
A[5][15] =   -1.4778;
A[5][16] =   -2.7713;
A[5][17] =    2.5600;
A[5][18] =   -1.6528;
A[5][19] =    0.0274;
A[5][20] =   -2.8761;
A[5][21] =    2.6039;
A[5][22] =    0.0573;
A[5][23] =    0.7929;
A[5][24] =    1.6544;
A[5][25] =   -0.2345;
A[5][26] =    3.4627;
A[5][27] =    3.6834;
A[5][28] =   -3.2148;
A[5][29] =   -1.4374;
A[5][30] =    0.1151;
A[5][31] =   -0.0316;
A[5][32] =    0.2460;
A[5][33] =   -0.9853;
A[5][34] =   -0.2113;
A[5][35] =    0.0776;
A[5][36] =   -0.9968;
A[5][37] =    0.8526;
A[5][38] =    0.6270;
A[5][39] =   -0.4667;
A[6][0] =   -0.2613;
A[6][1] =    0.7980;
A[6][2] =    0.6446;
A[6][3] =    1.1101;
A[6][4] =   -0.2173;
A[6][5] =   -0.1006;
A[6][6] =   -0.4464;
A[6][7] =    0.8722;
A[6][8] =    1.1782;
A[6][9] =   -1.3472;
A[6][10] =    2.0070;
A[6][11] =    1.6716;
A[6][12] =    0.9454;
A[6][13] =   -2.4668;
A[6][14] =    2.8922;
A[6][15] =   -2.5910;
A[6][16] =    1.9530;
A[6][17] =   -0.9677;
A[6][18] =   -0.6747;
A[6][19] =    0.8515;
A[6][20] =   -2.9248;
A[6][21] =   -3.2266;
A[6][22] =   -0.6634;
A[6][23] =   -1.7815;
A[6][24] =   -2.3791;
A[6][25] =   -1.8189;
A[6][26] =   -3.2926;
A[6][27] =   -5.5603;
A[6][28] =    2.6477;
A[6][29] =    1.3681;
A[6][30] =   -0.6072;
A[6][31] =    0.7784;
A[6][32] =   -0.4033;
A[6][33] =    0.8963;
A[6][34] =    0.9176;
A[6][35] =    0.4933;
A[6][36] =   -0.9962;
A[6][37] =    0.5111;
A[6][38] =    0.3998;
A[6][39] =    0.1997;
A[7][0] =    0.3601;
A[7][1] =   -0.1708;
A[7][2] =    0.5862;
A[7][3] =    0.1295;
A[7][4] =    1.0682;
A[7][5] =    1.0489;
A[7][6] =    0.8134;
A[7][7] =   -0.7758;
A[7][8] =   -0.4738;
A[7][9] =    1.1658;
A[7][10] =   -2.0094;
A[7][11] =    0.1310;
A[7][12] =    1.8893;
A[7][13] =   -0.7452;
A[7][14] =    2.4213;
A[7][15] =    2.8252;
A[7][16] =   -2.0565;
A[7][17] =   -2.4649;
A[7][18] =   -3.0368;
A[7][19] =   -1.3176;
A[7][20] =    2.2941;
A[7][21] =    1.7016;
A[7][22] =   -0.8009;
A[7][23] =   -1.1378;
A[7][24] =   -2.7976;
A[7][25] =   -1.5992;
A[7][26] =   -0.6137;
A[7][27] =   -2.6942;
A[7][28] =    2.3030;
A[7][29] =   -1.5994;
A[7][30] =    0.0897;
A[7][31] =    0.3617;
A[7][32] =   -0.8784;
A[7][33] =   -0.5915;
A[7][34] =   -0.5685;
A[7][35] =    0.8471;
A[7][36] =   -0.5534;
A[7][37] =   -0.6707;
A[7][38] =   -0.6745;
A[7][39] =    0.2596;
A[8][0] =   -0.3830;
A[8][1] =   -0.7576;
A[8][2] =    0.5353;
A[8][3] =   -0.1332;
A[8][4] =    0.3964;
A[8][5] =   -0.8086;
A[8][6] =    1.2713;
A[8][7] =    0.6308;
A[8][8] =    0.5724;
A[8][9] =    1.9951;
A[8][10] =    1.9241;
A[8][11] =    1.2873;
A[8][12] =    1.2938;
A[8][13] =    1.2187;
A[8][14] =    3.0049;
A[8][15] =    1.0996;
A[8][16] =    1.6079;
A[8][17] =    1.4785;
A[8][18] =   -3.4046;
A[8][19] =   -1.7042;
A[8][20] =    2.7468;
A[8][21] =   -2.4942;
A[8][22] =   -0.9351;
A[8][23] =    3.8790;
A[8][24] =   -1.1801;
A[8][25] =    3.3740;
A[8][26] =    2.1408;
A[8][27] =    3.9866;
A[8][28] =   -1.0191;
A[8][29] =    0.5529;
A[8][30] =   -0.7409;
A[8][31] =   -0.6216;
A[8][32] =    0.8832;
A[8][33] =    0.9661;
A[8][34] =   -0.7189;
A[8][35] =    0.1897;
A[8][36] =    0.4021;
A[8][37] =   -0.0758;
A[8][38] =    0.9436;
A[8][39] =    0.5912;
A[9][0] =   -0.0857;
A[9][1] =   -0.9159;
A[9][2] =   -0.0572;
A[9][3] =    0.8750;
A[9][4] =    1.2338;
A[9][5] =    0.9221;
A[9][6] =   -0.5182;
A[9][7] =    0.7939;
A[9][8] =    0.1107;
A[9][9] =    1.4405;
A[9][10] =    1.1031;
A[9][11] =    0.3753;
A[9][12] =    0.2994;
A[9][13] =   -2.2668;
A[9][14] =    0.4872;
A[9][15] =    1.7982;
A[9][16] =   -1.2401;
A[9][17] =   -0.4927;
A[9][18] =   -1.4869;
A[9][19] =   -3.3363;
A[9][20] =    1.0912;
A[9][21] =   -3.6638;
A[9][22] =   -4.0285;
A[9][23] =    2.7394;
A[9][24] =   -1.9531;
A[9][25] =    4.3395;
A[9][26] =   -4.1138;
A[9][27] =   -3.5885;
A[9][28] =    1.3922;
A[9][29] =   -2.8182;
A[9][30] =   -0.6704;
A[9][31] =    0.5546;
A[9][32] =   -0.5383;
A[9][33] =    0.2918;
A[9][34] =    0.2637;
A[9][35] =    0.8515;
A[9][36] =   -0.7055;
A[9][37] =    0.2300;
A[9][38] =    0.7820;
A[9][39] =   -0.4942;
A[10][0] =   -0.5639;
A[10][1] =   -0.9396;
A[10][2] =    0.1579;
A[10][3] =   -0.4153;
A[10][4] =    1.0089;
A[10][5] =   -0.8782;
A[10][6] =    1.3809;
A[10][7] =    0.6902;
A[10][8] =   -0.5260;
A[10][9] =   -1.8316;
A[10][10] =    0.7723;
A[10][11] =    0.5242;
A[10][12] =    1.8735;
A[10][13] =    0.8454;
A[10][14] =   -2.0681;
A[10][15] =    0.9482;
A[10][16] =    0.0169;
A[10][17] =    3.1398;
A[10][18] =   -1.7105;
A[10][19] =    1.8624;
A[10][20] =   -2.1102;
A[10][21] =   -3.5341;
A[10][22] =    0.0710;
A[10][23] =    2.5471;
A[10][24] =    3.9012;
A[10][25] =    4.0946;
A[10][26] =    2.9378;
A[10][27] =    2.8949;
A[10][28] =   -6.1244;
A[10][29] =    2.1624;
A[10][30] =    0.3874;
A[10][31] =   -0.1665;
A[10][32] =    0.3720;
A[10][33] =    0.4209;
A[10][34] =    0.8914;
A[10][35] =    0.7009;
A[10][36] =    0.5958;
A[10][37] =   -0.8344;
A[10][38] =    0.5778;
A[10][39] =   -0.9534;
A[11][0] =    0.0158;
A[11][1] =    1.0377;
A[11][2] =    0.5664;
A[11][3] =    0.6747;
A[11][4] =   -1.2386;
A[11][5] =   -0.4170;
A[11][6] =   -0.0553;
A[11][7] =    0.5607;
A[11][8] =    0.1969;
A[11][9] =    1.8311;
A[11][10] =    0.8953;
A[11][11] =    1.0391;
A[11][12] =    1.5677;
A[11][13] =   -0.4935;
A[11][14] =   -1.2639;
A[11][15] =   -2.0787;
A[11][16] =    2.2662;
A[11][17] =   -1.2193;
A[11][18] =    0.0612;
A[11][19] =   -2.4095;
A[11][20] =    3.1243;
A[11][21] =    0.7251;
A[11][22] =    2.4447;
A[11][23] =    0.3686;
A[11][24] =   -2.7411;
A[11][25] =    0.5594;
A[11][26] =   -3.9436;
A[11][27] =    0.4008;
A[11][28] =    2.3330;
A[11][29] =   -1.0662;
A[11][30] =    0.2111;
A[11][31] =    0.2791;
A[11][32] =    0.8585;
A[11][33] =    0.2062;
A[11][34] =    0.1085;
A[11][35] =    0.7985;
A[11][36] =   -0.0094;
A[11][37] =   -0.5704;
A[11][38] =   -0.2768;
A[11][39] =   -0.1211;
A[12][0] =   -0.2049;
A[12][1] =    0.2965;
A[12][2] =   -1.1137;
A[12][3] =   -1.0946;
A[12][4] =   -1.3232;
A[12][5] =   -0.1321;
A[12][6] =    1.0430;
A[12][7] =   -0.6997;
A[12][8] =   -0.4295;
A[12][9] =   -1.6367;
A[12][10] =   -0.8862;
A[12][11] =    1.7531;
A[12][12] =   -0.1564;
A[12][13] =    0.1784;
A[12][14] =   -1.2233;
A[12][15] =   -2.5919;
A[12][16] =   -0.0578;
A[12][17] =    1.6848;
A[12][18] =    2.6168;
A[12][19] =    3.0446;
A[12][20] =    0.8826;
A[12][21] =    2.4828;
A[12][22] =   -2.0867;
A[12][23] =    1.1905;
A[12][24] =    2.8165;
A[12][25] =    1.1971;
A[12][26] =    0.0193;
A[12][27] =   -4.5521;
A[12][28] =    1.3128;
A[12][29] =   -0.5970;
A[12][30] =   -0.3318;
A[12][31] =    0.5628;
A[12][32] =   -0.8155;
A[12][33] =   -0.2693;
A[12][34] =    0.8400;
A[12][35] =   -0.3815;
A[12][36] =   -0.4481;
A[12][37] =   -0.4920;
A[12][38] =    0.2316;
A[12][39] =    0.8402;
A[13][0] =   -0.1544;
A[13][1] =    0.9556;
A[13][2] =    0.5261;
A[13][3] =    0.6038;
A[13][4] =   -0.1695;
A[13][5] =   -0.7053;
A[13][6] =    0.8958;
A[13][7] =    1.3093;
A[13][8] =   -0.1940;
A[13][9] =    1.4842;
A[13][10] =    1.2756;
A[13][11] =   -1.7469;
A[13][12] =    0.2507;
A[13][13] =   -0.7778;
A[13][14] =   -1.2025;
A[13][15] =   -1.6165;
A[13][16] =    0.2737;
A[13][17] =   -2.0783;
A[13][18] =    0.7674;
A[13][19] =    0.4701;
A[13][20] =   -1.9849;
A[13][21] =   -1.3284;
A[13][22] =    2.7038;
A[13][23] =   -0.4934;
A[13][24] =   -3.6704;
A[13][25] =   -1.6595;
A[13][26] =    0.7563;
A[13][27] =    2.1246;
A[13][28] =    0.9607;
A[13][29] =   -0.9248;
A[13][30] =    0.2847;
A[13][31] =    0.5363;
A[13][32] =   -0.9193;
A[13][33] =   -0.1352;
A[13][34] =   -0.3642;
A[13][35] =   -0.3808;
A[13][36] =    0.6815;
A[13][37] =   -0.4849;
A[13][38] =    0.6758;
A[13][39] =    0.9696;
A[14][0] =    0.4381;
A[14][1] =    0.9840;
A[14][2] =    0.9229;
A[14][3] =    0.9571;
A[14][4] =    1.2480;
A[14][5] =   -0.3383;
A[14][6] =    0.2906;
A[14][7] =   -0.7811;
A[14][8] =    1.8307;
A[14][9] =    0.7204;
A[14][10] =    1.3003;
A[14][11] =    1.6919;
A[14][12] =   -2.2137;
A[14][13] =    1.7819;
A[14][14] =    2.5344;
A[14][15] =    0.1403;
A[14][16] =    1.8750;
A[14][17] =    2.3096;
A[14][18] =   -2.5928;
A[14][19] =   -1.4884;
A[14][20] =    2.0736;
A[14][21] =   -1.3367;
A[14][22] =    1.2643;
A[14][23] =   -2.9898;
A[14][24] =   -1.4777;
A[14][25] =   -3.4981;
A[14][26] =   -0.4207;
A[14][27] =    3.4254;
A[14][28] =   -1.3540;
A[14][29] =   -0.5792;
A[14][30] =    0.0977;
A[14][31] =    0.8804;
A[14][32] =   -0.1345;
A[14][33] =    0.1605;
A[14][34] =   -0.5758;
A[14][35] =    0.1267;
A[14][36] =    0.5985;
A[14][37] =   -0.1751;
A[14][38] =   -0.2946;
A[14][39] =   -0.0542;
A[15][0] =   -0.5295;
A[15][1] =   -0.6428;
A[15][2] =   -1.0184;
A[15][3] =    1.1489;
A[15][4] =   -0.1073;
A[15][5] =   -1.1249;
A[15][6] =    0.9377;
A[15][7] =    1.4083;
A[15][8] =   -0.6435;
A[15][9] =   -1.9657;
A[15][10] =    0.7583;
A[15][11] =   -1.0733;
A[15][12] =   -0.0662;
A[15][13] =    2.4980;
A[15][14] =    1.4959;
A[15][15] =   -2.1283;
A[15][16] =   -1.9338;
A[15][17] =    1.4531;
A[15][18] =   -2.6444;
A[15][19] =    0.7128;
A[15][20] =   -0.2274;
A[15][21] =    0.4426;
A[15][22] =   -1.2340;
A[15][23] =    4.2394;
A[15][24] =   -3.4371;
A[15][25] =    0.1990;
A[15][26] =   -3.4793;
A[15][27] =    0.7860;
A[15][28] =   -5.0069;
A[15][29] =    3.1043;
A[15][30] =    0.1541;
A[15][31] =   -0.3202;
A[15][32] =   -0.3942;
A[15][33] =    0.1292;
A[15][34] =   -0.0792;
A[15][35] =    0.2987;
A[15][36] =   -0.9289;
A[15][37] =   -0.4845;
A[15][38] =   -0.9970;
A[15][39] =    0.5303;
A[16][0] =   -0.3864;
A[16][1] =    0.0832;
A[16][2] =    0.6351;
A[16][3] =    0.4218;
A[16][4] =   -0.3745;
A[16][5] =    0.6121;
A[16][6] =    0.6136;
A[16][7] =   -0.9750;
A[16][8] =   -0.0518;
A[16][9] =    1.2544;
A[16][10] =   -0.3928;
A[16][11] =    0.9915;
A[16][12] =    2.0380;
A[16][13] =    0.6102;
A[16][14] =   -2.8598;
A[16][15] =   -2.2918;
A[16][16] =    1.7030;
A[16][17] =    0.3696;
A[16][18] =   -3.4268;
A[16][19] =   -1.6009;
A[16][20] =    1.1101;
A[16][21] =    1.6572;
A[16][22] =   -2.1416;
A[16][23] =   -1.9590;
A[16][24] =    3.7206;
A[16][25] =   -2.3746;
A[16][26] =   -0.9680;
A[16][27] =   -4.0349;
A[16][28] =    4.0864;
A[16][29] =   -0.4864;
A[16][30] =    0.8167;
A[16][31] =   -0.9557;
A[16][32] =    0.6479;
A[16][33] =    0.4876;
A[16][34] =   -0.7586;
A[16][35] =   -0.9062;
A[16][36] =   -0.5714;
A[16][37] =    0.7588;
A[16][38] =    0.0641;
A[16][39] =   -0.2069;
A[17][0] =   -0.7127;
A[17][1] =    0.2305;
A[17][2] =    0.9802;
A[17][3] =    0.0015;
A[17][4] =   -1.1289;
A[17][5] =    0.0398;
A[17][6] =   -1.0573;
A[17][7] =   -0.5354;
A[17][8] =    0.8331;
A[17][9] =   -0.4183;
A[17][10] =   -1.0119;
A[17][11] =   -0.2223;
A[17][12] =   -0.5638;
A[17][13] =   -1.2298;
A[17][14] =   -2.8237;
A[17][15] =    2.8438;
A[17][16] =    2.7442;
A[17][17] =   -1.5401;
A[17][18] =   -2.4917;
A[17][19] =    0.1920;
A[17][20] =    3.6255;
A[17][21] =    2.9946;
A[17][22] =    3.1780;
A[17][23] =    0.7362;
A[17][24] =   -0.9268;
A[17][25] =    3.8879;
A[17][26] =   -0.7561;
A[17][27] =    2.6589;
A[17][28] =    5.6236;
A[17][29] =   -0.2559;
A[17][30] =    0.7377;
A[17][31] =    0.5661;
A[17][32] =    0.0830;
A[17][33] =   -0.4096;
A[17][34] =    0.3380;
A[17][35] =   -0.7663;
A[17][36] =    0.7451;
A[17][37] =    0.0774;
A[17][38] =   -0.6276;
A[17][39] =   -0.8129;
A[18][0] =   -0.3212;
A[18][1] =    0.0164;
A[18][2] =   -0.3506;
A[18][3] =    0.6919;
A[18][4] =    0.8266;
A[18][5] =    0.2156;
A[18][6] =   -0.6247;
A[18][7] =   -0.7717;
A[18][8] =    1.2848;
A[18][9] =   -1.6597;
A[18][10] =    1.5125;
A[18][11] =    1.6607;
A[18][12] =   -0.0303;
A[18][13] =    2.5017;
A[18][14] =    2.3207;
A[18][15] =   -1.1261;
A[18][16] =   -0.0659;
A[18][17] =   -3.1951;
A[18][18] =    3.2831;
A[18][19] =    1.4567;
A[18][20] =   -1.9420;
A[18][21] =    1.3801;
A[18][22] =    3.5830;
A[18][23] =    0.5560;
A[18][24] =    3.9415;
A[18][25] =   -3.6976;
A[18][26] =    3.3004;
A[18][27] =    4.0685;
A[18][28] =   -1.6135;
A[18][29] =    3.1234;
A[18][30] =   -0.5424;
A[18][31] =   -0.0798;
A[18][32] =    0.3066;
A[18][33] =   -0.0945;
A[18][34] =   -0.9636;
A[18][35] =   -0.5737;
A[18][36] =    0.3471;
A[18][37] =   -0.2553;
A[18][38] =    0.9898;
A[18][39] =    0.6684;
A[19][0] =    0.4675;
A[19][1] =   -0.0660;
A[19][2] =    0.6178;
A[19][3] =    0.7731;
A[19][4] =    1.2326;
A[19][5] =   -1.0910;
A[19][6] =    0.3785;
A[19][7] =   -0.2942;
A[19][8] =    1.7793;
A[19][9] =   -1.4681;
A[19][10] =    1.5339;
A[19][11] =   -1.8827;
A[19][12] =    1.6224;
A[19][13] =    1.9823;
A[19][14] =   -1.2865;
A[19][15] =   -2.2538;
A[19][16] =    0.3733;
A[19][17] =    3.2257;
A[19][18] =    1.7680;
A[19][19] =   -3.2175;
A[19][20] =    2.5851;
A[19][21] =    0.6987;
A[19][22] =   -4.0859;
A[19][23] =   -0.3760;
A[19][24] =   -4.2321;
A[19][25] =    0.1537;
A[19][26] =   -4.7261;
A[19][27] =    2.0474;
A[19][28] =   -3.1355;
A[19][29] =    2.9749;
A[19][30] =    0.4498;
A[19][31] =    0.9689;
A[19][32] =   -0.5368;
A[19][33] =    0.4970;
A[19][34] =   -0.4628;
A[19][35] =    0.0755;
A[19][36] =    0.2501;
A[19][37] =   -0.8197;
A[19][38] =    0.5858;
A[19][39] =    0.5057;
A[20][0] =    0.0332;
A[20][1] =    0.5613;
A[20][2] =   -0.9048;
A[20][3] =    0.0287;
A[20][4] =    1.0366;
A[20][5] =   -0.1879;
A[20][6] =    0.5699;
A[20][7] =    1.4434;
A[20][8] =    1.8315;
A[20][9] =   -0.3108;
A[20][10] =   -1.4280;
A[20][11] =   -1.6158;
A[20][12] =   -1.6343;
A[20][13] =    1.4335;
A[20][14] =    2.5822;
A[20][15] =    1.8381;
A[20][16] =    2.8616;
A[20][17] =   -2.9280;
A[20][18] =   -0.5310;
A[20][19] =   -3.4582;
A[20][20] =    0.3998;
A[20][21] =   -1.4969;
A[20][22] =    0.9867;
A[20][23] =   -0.4512;
A[20][24] =   -3.6997;
A[20][25] =    0.6959;
A[20][26] =    2.6960;
A[20][27] =    1.0543;
A[20][28] =   -4.1319;
A[20][29] =   -1.8454;
A[20][30] =   -0.0838;
A[20][31] =    0.5287;
A[20][32] =    0.1655;
A[20][33] =    0.1291;
A[20][34] =    0.8364;
A[20][35] =    0.8916;
A[20][36] =    0.5850;
A[20][37] =    0.7500;
A[20][38] =    0.5463;
A[20][39] =   -0.4205;
A[21][0] =   -0.5040;
A[21][1] =    0.0239;
A[21][2] =   -0.9138;
A[21][3] =   -1.1527;
A[21][4] =   -1.3877;
A[21][5] =    0.9345;
A[21][6] =    0.0513;
A[21][7] =   -0.5692;
A[21][8] =   -0.7785;
A[21][9] =   -0.7937;
A[21][10] =    1.4529;
A[21][11] =    1.0703;
A[21][12] =   -1.4126;
A[21][13] =   -1.4499;
A[21][14] =   -0.8580;
A[21][15] =    1.8376;
A[21][16] =    0.7384;
A[21][17] =    1.0683;
A[21][18] =   -2.1960;
A[21][19] =   -0.1270;
A[21][20] =    2.4610;
A[21][21] =   -3.8228;
A[21][22] =   -0.5429;
A[21][23] =    4.0800;
A[21][24] =   -2.7391;
A[21][25] =   -1.9177;
A[21][26] =   -4.2734;
A[21][27] =   -5.4704;
A[21][28] =   -0.3541;
A[21][29] =   -0.2514;
A[21][30] =    0.1870;
A[21][31] =    0.0474;
A[21][32] =   -0.5394;
A[21][33] =   -0.8731;
A[21][34] =   -0.0726;
A[21][35] =    0.5678;
A[21][36] =    0.5460;
A[21][37] =    0.6517;
A[21][38] =    0.4606;
A[21][39] =   -0.6983;
A[22][0] =    0.4908;
A[22][1] =    0.2678;
A[22][2] =   -0.8265;
A[22][3] =    0.0287;
A[22][4] =   -0.4570;
A[22][5] =    0.2518;
A[22][6] =    0.8346;
A[22][7] =   -0.6688;
A[22][8] =   -0.6460;
A[22][9] =   -1.3179;
A[22][10] =   -0.6011;
A[22][11] =    0.3290;
A[22][12] =    1.7375;
A[22][13] =   -2.3485;
A[22][14] =    2.5475;
A[22][15] =   -2.1074;
A[22][16] =    2.1423;
A[22][17] =    1.6169;
A[22][18] =    0.6627;
A[22][19] =   -1.7356;
A[22][20] =    0.6358;
A[22][21] =   -0.7707;
A[22][22] =    1.8182;
A[22][23] =    2.7732;
A[22][24] =   -0.2606;
A[22][25] =    1.4322;
A[22][26] =    0.4506;
A[22][27] =   -5.6980;
A[22][28] =   -0.2719;
A[22][29] =   -0.8669;
A[22][30] =   -0.8303;
A[22][31] =   -0.4739;
A[22][32] =    0.4613;
A[22][33] =    0.9280;
A[22][34] =    0.4845;
A[22][35] =    0.9229;
A[22][36] =   -0.7159;
A[22][37] =   -0.0803;
A[22][38] =    0.5801;
A[22][39] =    0.1511;
A[23][0] =   -0.3072;
A[23][1] =   -0.9672;
A[23][2] =   -0.0648;
A[23][3] =   -0.1319;
A[23][4] =   -0.7364;
A[23][5] =   -0.9645;
A[23][6] =    1.3265;
A[23][7] =    0.1621;
A[23][8] =    0.7061;
A[23][9] =   -0.4245;
A[23][10] =    0.5253;
A[23][11] =   -0.9362;
A[23][12] =   -1.8023;
A[23][13] =   -2.5739;
A[23][14] =    0.5768;
A[23][15] =    0.7443;
A[23][16] =   -2.6200;
A[23][17] =    0.8751;
A[23][18] =    2.0002;
A[23][19] =    0.8895;
A[23][20] =    1.1360;
A[23][21] =    3.2966;
A[23][22] =    2.4072;
A[23][23] =   -1.5395;
A[23][24] =    2.5586;
A[23][25] =    4.1241;
A[23][26] =   -1.5482;
A[23][27] =    5.8877;
A[23][28] =    5.8976;
A[23][29] =   -1.4894;
A[23][30] =   -0.2577;
A[23][31] =   -0.3813;
A[23][32] =   -0.4188;
A[23][33] =   -0.8533;
A[23][34] =   -0.8767;
A[23][35] =   -0.0380;
A[23][36] =    0.8110;
A[23][37] =    0.2828;
A[23][38] =    0.9171;
A[23][39] =    0.2944;
A[24][0] =   -0.6136;
A[24][1] =    0.6496;
A[24][2] =   -0.0516;
A[24][3] =   -0.4362;
A[24][4] =   -0.6261;
A[24][5] =    1.3155;
A[24][6] =    0.2333;
A[24][7] =   -0.0072;
A[24][8] =    0.1203;
A[24][9] =   -1.4285;
A[24][10] =    1.1852;
A[24][11] =    1.9971;
A[24][12] =    2.2779;
A[24][13] =    1.1263;
A[24][14] =   -1.1927;
A[24][15] =    2.8806;
A[24][16] =    0.6380;
A[24][17] =    2.2151;
A[24][18] =   -0.8826;
A[24][19] =   -2.7070;
A[24][20] =   -0.0593;
A[24][21] =    2.6456;
A[24][22] =    0.2483;
A[24][23] =    3.8904;
A[24][24] =   -0.5128;
A[24][25] =    4.1011;
A[24][26] =    0.0915;
A[24][27] =    0.3555;
A[24][28] =    4.0226;
A[24][29] =    1.0748;
A[24][30] =   -0.6798;
A[24][31] =   -0.3172;
A[24][32] =   -0.6388;
A[24][33] =   -0.7046;
A[24][34] =    0.5534;
A[24][35] =    0.5801;
A[24][36] =    0.9767;
A[24][37] =    0.8792;
A[24][38] =   -0.5921;
A[24][39] =   -0.9333;
A[25][0] =    0.3894;
A[25][1] =    0.4208;
A[25][2] =    0.3362;
A[25][3] =    0.8348;
A[25][4] =   -0.3135;
A[25][5] =   -1.2453;
A[25][6] =   -0.0621;
A[25][7] =    1.0144;
A[25][8] =    0.1971;
A[25][9] =   -1.1018;
A[25][10] =   -0.1837;
A[25][11] =   -1.4204;
A[25][12] =    0.4214;
A[25][13] =    2.5202;
A[25][14] =    1.1011;
A[25][15] =   -0.9007;
A[25][16] =   -0.1604;
A[25][17] =   -2.1949;
A[25][18] =    2.7993;
A[25][19] =   -0.4368;
A[25][20] =   -1.2198;
A[25][21] =    3.5642;
A[25][22] =   -2.1740;
A[25][23] =    4.3199;
A[25][24] =    3.5604;
A[25][25] =   -0.5365;
A[25][26] =   -2.7942;
A[25][27] =    5.8506;
A[25][28] =   -4.4468;
A[25][29] =    2.1786;
A[25][30] =    0.2267;
A[25][31] =    0.9888;
A[25][32] =   -0.5715;
A[25][33] =   -0.3608;
A[25][34] =    0.1118;
A[25][35] =    0.1707;
A[25][36] =   -0.5327;
A[25][37] =   -0.5472;
A[25][38] =    0.0485;
A[25][39] =    0.9400;
A[26][0] =   -0.4431;
A[26][1] =    0.4497;
A[26][2] =    0.1595;
A[26][3] =   -0.1770;
A[26][4] =   -0.6043;
A[26][5] =    1.1908;
A[26][6] =   -0.9946;
A[26][7] =    0.5209;
A[26][8] =   -1.2315;
A[26][9] =    1.1757;
A[26][10] =    0.2117;
A[26][11] =    0.0599;
A[26][12] =    0.8563;
A[26][13] =   -0.0945;
A[26][14] =   -2.7628;
A[26][15] =   -1.0004;
A[26][16] =    0.2793;
A[26][17] =    0.4118;
A[26][18] =    1.2392;
A[26][19] =   -3.2088;
A[26][20] =    1.7837;
A[26][21] =   -0.2665;
A[26][22] =   -0.0450;
A[26][23] =    1.9919;
A[26][24] =   -2.4461;
A[26][25] =   -3.5328;
A[26][26] =    4.1827;
A[26][27] =    0.8432;
A[26][28] =    1.2559;
A[26][29] =   -0.2799;
A[26][30] =   -0.6229;
A[26][31] =   -0.4046;
A[26][32] =    0.1749;
A[26][33] =   -0.1174;
A[26][34] =   -0.5470;
A[26][35] =   -0.4194;
A[26][36] =    0.7214;
A[26][37] =   -0.1270;
A[26][38] =   -0.2424;
A[26][39] =   -0.5187;
A[27][0] =   -0.1616;
A[27][1] =   -1.0929;
A[27][2] =    0.6715;
A[27][3] =   -0.0534;
A[27][4] =    0.6227;
A[27][5] =   -0.1248;
A[27][6] =    0.0349;
A[27][7] =    0.1015;
A[27][8] =    0.5633;
A[27][9] =   -0.8972;
A[27][10] =   -0.9837;
A[27][11] =   -1.0628;
A[27][12] =    1.5797;
A[27][13] =    1.1676;
A[27][14] =   -2.4813;
A[27][15] =    1.4719;
A[27][16] =    1.2623;
A[27][17] =   -0.2388;
A[27][18] =    3.3479;
A[27][19] =   -1.7590;
A[27][20] =   -1.8474;
A[27][21] =   -2.1306;
A[27][22] =    0.0208;
A[27][23] =    2.1242;
A[27][24] =    3.6694;
A[27][25] =   -1.0077;
A[27][26] =   -0.2204;
A[27][27] =   -1.6374;
A[27][28] =   -4.9071;
A[27][29] =    1.3073;
A[27][30] =   -0.5281;
A[27][31] =   -0.1893;
A[27][32] =   -0.4387;
A[27][33] =    0.5063;
A[27][34] =    0.5303;
A[27][35] =   -0.8203;
A[27][36] =   -0.3458;
A[27][37] =   -0.7581;
A[27][38] =   -0.9650;
A[27][39] =    0.5167;
A[28][0] =   -0.5482;
A[28][1] =    0.8970;
A[28][2] =    0.9204;
A[28][3] =   -0.9235;
A[28][4] =    0.2552;
A[28][5] =    0.9998;
A[28][6] =   -0.4993;
A[28][7] =   -0.3886;
A[28][8] =   -1.4600;
A[28][9] =    1.8289;
A[28][10] =   -1.2338;
A[28][11] =    1.9913;
A[28][12] =   -0.9956;
A[28][13] =    1.6734;
A[28][14] =    1.0870;
A[28][15] =   -2.6109;
A[28][16] =   -2.8336;
A[28][17] =    1.1992;
A[28][18] =    1.4741;
A[28][19] =    1.2852;
A[28][20] =   -1.2545;
A[28][21] =   -1.7148;
A[28][22] =    2.1337;
A[28][23] =    0.4510;
A[28][24] =   -2.3090;
A[28][25] =   -0.3629;
A[28][26] =   -1.7074;
A[28][27] =   -0.2360;
A[28][28] =   -1.6854;
A[28][29] =   -0.0182;
A[28][30] =    0.3777;
A[28][31] =    0.0498;
A[28][32] =    0.9477;
A[28][33] =    0.6580;
A[28][34] =    0.9803;
A[28][35] =   -0.1151;
A[28][36] =   -0.4458;
A[28][37] =    0.8296;
A[28][38] =   -0.3470;
A[28][39] =   -0.1768;
A[29][0] =   -0.4220;
A[29][1] =   -0.5802;
A[29][2] =   -0.8271;
A[29][3] =   -1.1440;
A[29][4] =    0.5537;
A[29][5] =    1.0662;
A[29][6] =   -0.1077;
A[29][7] =   -0.5553;
A[29][8] =    1.2875;
A[29][9] =    1.9164;
A[29][10] =   -1.1389;
A[29][11] =   -1.2382;
A[29][12] =    0.8079;
A[29][13] =    0.3402;
A[29][14] =    2.4826;
A[29][15] =   -0.9998;
A[29][16] =   -1.7381;
A[29][17] =   -1.4366;
A[29][18] =    0.2888;
A[29][19] =   -2.3575;
A[29][20] =    0.9108;
A[29][21] =   -2.5183;
A[29][22] =   -1.7130;
A[29][23] =    0.0680;
A[29][24] =    0.0132;
A[29][25] =   -2.3877;
A[29][26] =   -0.9612;
A[29][27] =    4.1427;
A[29][28] =   -0.9130;
A[29][29] =    2.2400;
A[29][30] =   -0.3358;
A[29][31] =   -0.3749;
A[29][32] =   -0.1642;
A[29][33] =    0.7557;
A[29][34] =    0.8666;
A[29][35] =   -0.0521;
A[29][36] =    0.0949;
A[29][37] =    0.9284;
A[29][38] =   -0.8306;
A[29][39] =    0.5499;
A[30][0] =    0.3836;
A[30][1] =   -1.0798;
A[30][2] =   -0.2644;
A[30][3] =   -0.5623;
A[30][4] =    0.1669;
A[30][5] =    0.8580;
A[30][6] =   -1.5197;
A[30][7] =   -0.0405;
A[30][8] =    1.1629;
A[30][9] =    0.0064;
A[30][10] =   -1.0769;
A[30][11] =   -1.4100;
A[30][12] =   -0.9460;
A[30][13] =   -2.0958;
A[30][14] =   -0.1736;
A[30][15] =    2.0438;
A[30][16] =    2.5242;
A[30][17] =   -2.5902;
A[30][18] =   -2.5559;
A[30][19] =   -1.6487;
A[30][20] =    3.7645;
A[30][21] =   -2.0325;
A[30][22] =   -2.2489;
A[30][23] =   -1.4361;
A[30][24] =   -2.6232;
A[30][25] =    1.0632;
A[30][26] =    0.5448;
A[30][27] =   -1.2252;
A[30][28] =    1.0863;
A[30][29] =   -2.5982;
A[30][30] =    0.1339;
A[30][31] =    0.3502;
A[30][32] =    0.6181;
A[30][33] =    0.9044;
A[30][34] =    0.2056;
A[30][35] =   -0.8155;
A[30][36] =   -0.2294;
A[30][37] =    0.0311;
A[30][38] =    0.7391;
A[30][39] =   -0.0343;
A[31][0] =    0.0204;
A[31][1] =   -0.3859;
A[31][2] =    1.0870;
A[31][3] =   -1.0874;
A[31][4] =    1.3066;
A[31][5] =    1.1281;
A[31][6] =   -0.5586;
A[31][7] =    1.2531;
A[31][8] =   -0.5385;
A[31][9] =    1.4037;
A[31][10] =   -0.8334;
A[31][11] =   -0.9011;
A[31][12] =   -0.6876;
A[31][13] =    0.4799;
A[31][14] =   -0.8874;
A[31][15] =    2.7171;
A[31][16] =   -1.3016;
A[31][17] =    0.6805;
A[31][18] =   -3.3033;
A[31][19] =   -0.0399;
A[31][20] =    2.1979;
A[31][21] =   -3.3725;
A[31][22] =    3.9822;
A[31][23] =    3.0095;
A[31][24] =    1.7224;
A[31][25] =   -2.8134;
A[31][26] =    2.7235;
A[31][27] =    4.7269;
A[31][28] =   -3.6515;
A[31][29] =   -2.0417;
A[31][30] =   -0.6964;
A[31][31] =    0.2239;
A[31][32] =    0.0752;
A[31][33] =    0.1318;
A[31][34] =   -0.0786;
A[31][35] =   -0.5584;
A[31][36] =   -0.7095;
A[31][37] =    0.1198;
A[31][38] =   -0.7959;
A[31][39] =   -0.0589;
 double b[m];
b[0] =  -10.6122;
b[1] =    0.7071;
b[2] =   -1.3419;
b[3] =   12.6202;
b[4] =    7.5327;
b[5] =   -2.8849;
b[6] =    2.2998;
b[7] =   -2.2035;
b[8] =    3.2117;
b[9] =    2.8975;
b[10] =   -0.5786;
b[11] =   14.6433;
b[12] =    1.4225;
b[13] =    5.0573;
b[14] =   -3.4711;
b[15] =   -1.2736;
b[16] =    6.0272;
b[17] =   16.9135;
b[18] =    0.1656;
b[19] =    5.4535;
b[20] =   -1.1597;
b[21] =   -0.9071;
b[22] =    2.2571;
b[23] =    9.4001;
b[24] =   12.9436;
b[25] =    0.7378;
b[26] =   -0.7542;
b[27] =   -0.8998;
b[28] =   -6.8887;
b[29] =   -6.3387;
b[30] =    0.9547;
b[31] =   -7.6797;
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
 f_val = f_temp2;
 FILE *file_output;
 file_output = fopen("output.out","w");
 fprintf(file_output, "%30.15f\n" ,f_val);
    fclose(file_output);


 return 0;
 }