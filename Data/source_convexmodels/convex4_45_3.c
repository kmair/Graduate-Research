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
 n = 45;
 m = 36;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double A[m][n];
A[0][0] =   -0.4671;
A[0][1] =   -0.5516;
A[0][2] =    1.0852;
A[0][3] =   -0.5420;
A[0][4] =    0.0283;
A[0][5] =   -0.0445;
A[0][6] =   -0.0043;
A[0][7] =    0.7002;
A[0][8] =    1.5081;
A[0][9] =    0.6358;
A[0][10] =    0.5005;
A[0][11] =   -0.5681;
A[0][12] =    1.8205;
A[0][13] =    1.8083;
A[0][14] =    0.5898;
A[0][15] =    1.7239;
A[0][16] =    0.0149;
A[0][17] =   -0.6261;
A[0][18] =   -2.0978;
A[0][19] =   -1.8346;
A[0][20] =    0.5893;
A[0][21] =   -2.4592;
A[0][22] =   -0.7080;
A[0][23] =   -1.0788;
A[0][24] =   -2.6323;
A[0][25] =    0.3367;
A[0][26] =   -2.1976;
A[0][27] =    0.6399;
A[0][28] =   -3.6011;
A[0][29] =   -0.6869;
A[0][30] =    2.1631;
A[0][31] =    0.1263;
A[0][32] =   -0.1426;
A[0][33] =    0.5582;
A[0][34] =   -4.1728;
A[0][35] =    4.0696;
A[0][36] =    0.4961;
A[0][37] =    0.4445;
A[0][38] =   -0.2773;
A[0][39] =    0.7100;
A[0][40] =   -0.4714;
A[0][41] =    0.2543;
A[0][42] =    0.3988;
A[0][43] =   -0.8982;
A[0][44] =   -0.8862;
A[1][0] =    0.4030;
A[1][1] =    0.2334;
A[1][2] =    0.6223;
A[1][3] =   -0.8768;
A[1][4] =   -0.8894;
A[1][5] =    0.4003;
A[1][6] =   -1.4138;
A[1][7] =    0.7349;
A[1][8] =    0.6674;
A[1][9] =    1.6323;
A[1][10] =   -0.1484;
A[1][11] =    0.2916;
A[1][12] =    1.3420;
A[1][13] =    0.8577;
A[1][14] =    1.2925;
A[1][15] =    1.9772;
A[1][16] =    0.7579;
A[1][17] =   -1.7192;
A[1][18] =   -0.4114;
A[1][19] =    1.6829;
A[1][20] =    0.4304;
A[1][21] =   -2.5497;
A[1][22] =   -1.2738;
A[1][23] =   -2.9171;
A[1][24] =   -3.0995;
A[1][25] =   -2.2155;
A[1][26] =    0.3164;
A[1][27] =    2.7617;
A[1][28] =    0.4431;
A[1][29] =    0.8608;
A[1][30] =   -0.2105;
A[1][31] =    4.8366;
A[1][32] =   -2.3753;
A[1][33] =   -1.0457;
A[1][34] =    0.2277;
A[1][35] =   -2.7332;
A[1][36] =   -0.5356;
A[1][37] =    0.9886;
A[1][38] =   -0.5570;
A[1][39] =    0.1119;
A[1][40] =   -0.3762;
A[1][41] =    0.2115;
A[1][42] =   -0.0860;
A[1][43] =   -0.4091;
A[1][44] =    0.2881;
A[2][0] =   -0.1997;
A[2][1] =    0.5743;
A[2][2] =   -0.2352;
A[2][3] =    0.5933;
A[2][4] =    0.9530;
A[2][5] =    0.8121;
A[2][6] =   -0.6230;
A[2][7] =    1.4100;
A[2][8] =    1.5476;
A[2][9] =   -0.1792;
A[2][10] =   -1.7514;
A[2][11] =    0.5868;
A[2][12] =    0.8846;
A[2][13] =    1.2869;
A[2][14] =    0.9152;
A[2][15] =   -0.1067;
A[2][16] =   -1.4474;
A[2][17] =    0.9652;
A[2][18] =    2.3279;
A[2][19] =   -2.4025;
A[2][20] =    1.8944;
A[2][21] =   -0.3558;
A[2][22] =    1.4615;
A[2][23] =   -1.2407;
A[2][24] =    0.1317;
A[2][25] =   -0.0350;
A[2][26] =    2.8869;
A[2][27] =   -2.6939;
A[2][28] =   -0.1011;
A[2][29] =    2.0360;
A[2][30] =    0.0155;
A[2][31] =   -1.3617;
A[2][32] =   -3.0071;
A[2][33] =    0.7154;
A[2][34] =   -4.8512;
A[2][35] =   -2.5822;
A[2][36] =   -0.5759;
A[2][37] =   -0.4056;
A[2][38] =    0.7309;
A[2][39] =    0.6820;
A[2][40] =   -0.4411;
A[2][41] =    0.3514;
A[2][42] =   -0.6293;
A[2][43] =    0.6656;
A[2][44] =    0.7123;
A[3][0] =   -0.1798;
A[3][1] =   -0.2078;
A[3][2] =   -0.5404;
A[3][3] =    0.4477;
A[3][4] =    0.1997;
A[3][5] =    1.0463;
A[3][6] =    0.0597;
A[3][7] =   -1.1146;
A[3][8] =   -1.0633;
A[3][9] =    1.5173;
A[3][10] =    0.3668;
A[3][11] =    1.7449;
A[3][12] =    0.9960;
A[3][13] =    2.0613;
A[3][14] =    0.0305;
A[3][15] =   -0.0448;
A[3][16] =    1.7031;
A[3][17] =    1.8902;
A[3][18] =    0.0731;
A[3][19] =   -0.7527;
A[3][20] =   -1.9870;
A[3][21] =    2.0635;
A[3][22] =   -0.9150;
A[3][23] =    0.8680;
A[3][24] =   -3.0206;
A[3][25] =    0.9183;
A[3][26] =    2.9290;
A[3][27] =    3.2986;
A[3][28] =    2.2380;
A[3][29] =   -2.9195;
A[3][30] =    0.7923;
A[3][31] =    3.5203;
A[3][32] =   -5.2164;
A[3][33] =   -1.6899;
A[3][34] =    1.8406;
A[3][35] =   -1.1055;
A[3][36] =    0.7790;
A[3][37] =   -0.7762;
A[3][38] =   -0.8835;
A[3][39] =   -0.0709;
A[3][40] =    0.6285;
A[3][41] =   -0.9309;
A[3][42] =   -0.2474;
A[3][43] =    0.7922;
A[3][44] =   -0.3005;
A[4][0] =    0.1368;
A[4][1] =   -0.3362;
A[4][2] =   -0.7477;
A[4][3] =    0.3574;
A[4][4] =    0.2644;
A[4][5] =   -1.0238;
A[4][6] =   -0.2002;
A[4][7] =    0.5741;
A[4][8] =   -0.8127;
A[4][9] =   -0.0191;
A[4][10] =   -1.7552;
A[4][11] =    1.1866;
A[4][12] =    1.8402;
A[4][13] =   -0.4693;
A[4][14] =    1.2312;
A[4][15] =    0.1803;
A[4][16] =   -0.2043;
A[4][17] =    0.0778;
A[4][18] =   -1.5396;
A[4][19] =    1.5312;
A[4][20] =    2.0036;
A[4][21] =    0.2530;
A[4][22] =    2.2780;
A[4][23] =    1.8170;
A[4][24] =    3.0975;
A[4][25] =    0.5215;
A[4][26] =    1.5885;
A[4][27] =   -0.5826;
A[4][28] =   -2.3336;
A[4][29] =   -0.9760;
A[4][30] =    0.1026;
A[4][31] =   -3.1960;
A[4][32] =   -3.4594;
A[4][33] =   -1.7955;
A[4][34] =   -4.6983;
A[4][35] =   -1.7516;
A[4][36] =   -0.3288;
A[4][37] =    0.4052;
A[4][38] =    0.8187;
A[4][39] =    0.1934;
A[4][40] =   -0.1233;
A[4][41] =   -0.5541;
A[4][42] =   -0.1380;
A[4][43] =   -0.0076;
A[4][44] =    0.9690;
A[5][0] =   -0.1995;
A[5][1] =    0.6695;
A[5][2] =   -0.1123;
A[5][3] =    0.6841;
A[5][4] =   -0.3227;
A[5][5] =    1.0165;
A[5][6] =    1.0143;
A[5][7] =   -0.4710;
A[5][8] =   -1.0340;
A[5][9] =   -0.7698;
A[5][10] =   -0.5824;
A[5][11] =   -0.1631;
A[5][12] =    1.2053;
A[5][13] =    0.9957;
A[5][14] =    0.5726;
A[5][15] =    1.9864;
A[5][16] =    0.0794;
A[5][17] =   -0.0803;
A[5][18] =   -1.1642;
A[5][19] =    0.6152;
A[5][20] =   -2.5062;
A[5][21] =    0.9496;
A[5][22] =   -1.1761;
A[5][23] =    0.3787;
A[5][24] =    2.6868;
A[5][25] =    0.5029;
A[5][26] =    3.2601;
A[5][27] =   -2.8824;
A[5][28] =    1.2961;
A[5][29] =    3.9392;
A[5][30] =    1.3178;
A[5][31] =    3.0108;
A[5][32] =   -2.5636;
A[5][33] =    0.0215;
A[5][34] =   -6.1584;
A[5][35] =   -1.4736;
A[5][36] =    0.7938;
A[5][37] =    0.2152;
A[5][38] =   -0.8267;
A[5][39] =    0.0287;
A[5][40] =    0.2351;
A[5][41] =   -0.2920;
A[5][42] =   -0.8358;
A[5][43] =   -0.9542;
A[5][44] =   -0.6316;
A[6][0] =    0.3257;
A[6][1] =   -0.0716;
A[6][2] =   -0.3689;
A[6][3] =   -0.5827;
A[6][4] =   -0.2155;
A[6][5] =   -0.3807;
A[6][6] =    0.2998;
A[6][7] =   -1.3348;
A[6][8] =   -0.2757;
A[6][9] =   -1.7198;
A[6][10] =    1.3347;
A[6][11] =    0.6195;
A[6][12] =    0.8121;
A[6][13] =    0.2010;
A[6][14] =   -0.2160;
A[6][15] =   -1.9113;
A[6][16] =   -1.7879;
A[6][17] =    0.7208;
A[6][18] =    0.0905;
A[6][19] =   -1.7513;
A[6][20] =    1.2531;
A[6][21] =   -0.6759;
A[6][22] =   -0.6926;
A[6][23] =   -0.6796;
A[6][24] =    1.5691;
A[6][25] =   -2.7011;
A[6][26] =   -0.5628;
A[6][27] =   -2.9967;
A[6][28] =   -3.6470;
A[6][29] =    2.7938;
A[6][30] =    1.4557;
A[6][31] =    4.1325;
A[6][32] =    3.7953;
A[6][33] =   -1.5681;
A[6][34] =    5.4073;
A[6][35] =    3.5499;
A[6][36] =   -0.9565;
A[6][37] =    0.7411;
A[6][38] =   -0.3569;
A[6][39] =   -0.6931;
A[6][40] =    0.3651;
A[6][41] =   -0.8367;
A[6][42] =    0.5402;
A[6][43] =   -0.3642;
A[6][44] =    0.8717;
A[7][0] =    0.2018;
A[7][1] =    0.0055;
A[7][2] =    0.4441;
A[7][3] =   -1.0182;
A[7][4] =   -0.8634;
A[7][5] =   -0.6909;
A[7][6] =   -1.2541;
A[7][7] =   -1.4985;
A[7][8] =    0.8364;
A[7][9] =    0.7410;
A[7][10] =   -1.2095;
A[7][11] =    0.6282;
A[7][12] =    1.1036;
A[7][13] =   -0.4256;
A[7][14] =   -1.1165;
A[7][15] =    1.0051;
A[7][16] =   -0.8035;
A[7][17] =   -1.5017;
A[7][18] =   -1.3826;
A[7][19] =    2.4965;
A[7][20] =    0.7142;
A[7][21] =    2.1463;
A[7][22] =   -2.1382;
A[7][23] =   -0.5779;
A[7][24] =   -2.9775;
A[7][25] =   -2.1837;
A[7][26] =    1.7624;
A[7][27] =   -2.4600;
A[7][28] =   -1.9429;
A[7][29] =   -1.7471;
A[7][30] =   -1.1392;
A[7][31] =    4.1260;
A[7][32] =    3.6114;
A[7][33] =   -0.9204;
A[7][34] =    2.1952;
A[7][35] =    4.4448;
A[7][36] =    0.0298;
A[7][37] =   -0.3943;
A[7][38] =    0.3743;
A[7][39] =   -0.2441;
A[7][40] =    0.5500;
A[7][41] =   -0.0337;
A[7][42] =   -0.2478;
A[7][43] =    0.2758;
A[7][44] =   -0.6055;
A[8][0] =   -0.6917;
A[8][1] =   -0.4468;
A[8][2] =   -0.7830;
A[8][3] =    1.1510;
A[8][4] =    0.9608;
A[8][5] =   -0.5005;
A[8][6] =   -0.7250;
A[8][7] =    1.3023;
A[8][8] =    1.0513;
A[8][9] =    1.5889;
A[8][10] =    0.5686;
A[8][11] =   -0.1037;
A[8][12] =    0.0739;
A[8][13] =    1.4556;
A[8][14] =   -0.4248;
A[8][15] =    0.5200;
A[8][16] =    1.0571;
A[8][17] =   -0.8701;
A[8][18] =    0.1831;
A[8][19] =    2.0951;
A[8][20] =   -2.0674;
A[8][21] =    0.2065;
A[8][22] =    0.5245;
A[8][23] =    0.2078;
A[8][24] =   -3.0049;
A[8][25] =   -3.1989;
A[8][26] =   -0.9226;
A[8][27] =   -0.9433;
A[8][28] =   -2.6453;
A[8][29] =    2.1894;
A[8][30] =    1.1688;
A[8][31] =   -1.0355;
A[8][32] =   -4.9079;
A[8][33] =   -2.2081;
A[8][34] =   -5.8107;
A[8][35] =    0.1911;
A[8][36] =   -0.7178;
A[8][37] =   -0.5909;
A[8][38] =   -0.2400;
A[8][39] =   -0.7459;
A[8][40] =    0.2531;
A[8][41] =    0.6585;
A[8][42] =   -0.5493;
A[8][43] =   -0.3456;
A[8][44] =    0.9575;
A[9][0] =    0.1514;
A[9][1] =   -0.2538;
A[9][2] =   -0.5534;
A[9][3] =   -0.9941;
A[9][4] =    0.8529;
A[9][5] =    1.1886;
A[9][6] =   -1.2647;
A[9][7] =    0.5385;
A[9][8] =    1.0343;
A[9][9] =    1.7393;
A[9][10] =   -0.2451;
A[9][11] =   -1.1153;
A[9][12] =    0.5060;
A[9][13] =   -0.0185;
A[9][14] =    1.6848;
A[9][15] =    0.6316;
A[9][16] =   -0.9986;
A[9][17] =   -1.4324;
A[9][18] =    0.6913;
A[9][19] =    2.4825;
A[9][20] =   -2.0017;
A[9][21] =    0.3755;
A[9][22] =    0.5645;
A[9][23] =   -1.0077;
A[9][24] =    0.5865;
A[9][25] =   -3.2482;
A[9][26] =   -1.8197;
A[9][27] =    2.6213;
A[9][28] =   -0.3635;
A[9][29] =   -1.4653;
A[9][30] =    1.0887;
A[9][31] =    0.2101;
A[9][32] =   -2.0195;
A[9][33] =    0.9853;
A[9][34] =    3.2882;
A[9][35] =    2.0210;
A[9][36] =    0.0106;
A[9][37] =    0.9331;
A[9][38] =   -0.7145;
A[9][39] =    0.6082;
A[9][40] =   -0.6158;
A[9][41] =   -0.7950;
A[9][42] =    0.4577;
A[9][43] =   -0.1989;
A[9][44] =   -0.2227;
A[10][0] =   -0.6250;
A[10][1] =    0.1351;
A[10][2] =    0.4646;
A[10][3] =    0.4733;
A[10][4] =    0.8337;
A[10][5] =   -1.3089;
A[10][6] =   -0.7617;
A[10][7] =    0.1104;
A[10][8] =   -0.4266;
A[10][9] =   -0.5113;
A[10][10] =    1.1394;
A[10][11] =   -1.7183;
A[10][12] =    1.3056;
A[10][13] =   -1.0801;
A[10][14] =   -1.3770;
A[10][15] =   -0.4699;
A[10][16] =   -0.5176;
A[10][17] =    0.0046;
A[10][18] =   -0.2859;
A[10][19] =    1.0185;
A[10][20] =   -0.6930;
A[10][21] =    1.7581;
A[10][22] =    2.2232;
A[10][23] =   -2.7690;
A[10][24] =    1.4222;
A[10][25] =    2.5006;
A[10][26] =    0.8227;
A[10][27] =    3.6627;
A[10][28] =    3.3740;
A[10][29] =    1.9509;
A[10][30] =   -0.4248;
A[10][31] =    3.2306;
A[10][32] =   -2.2458;
A[10][33] =    0.1269;
A[10][34] =    2.3831;
A[10][35] =    4.1224;
A[10][36] =    0.6232;
A[10][37] =   -0.7367;
A[10][38] =    0.6030;
A[10][39] =   -0.7394;
A[10][40] =    0.6310;
A[10][41] =   -0.9844;
A[10][42] =    0.7633;
A[10][43] =    0.0579;
A[10][44] =    0.2582;
A[11][0] =    0.6100;
A[11][1] =   -0.4822;
A[11][2] =   -0.2190;
A[11][3] =    0.2495;
A[11][4] =   -0.6140;
A[11][5] =   -1.3263;
A[11][6] =    0.8260;
A[11][7] =   -0.5077;
A[11][8] =   -1.5365;
A[11][9] =   -0.7665;
A[11][10] =    1.5224;
A[11][11] =   -1.6091;
A[11][12] =    1.1754;
A[11][13] =   -0.9353;
A[11][14] =   -0.3949;
A[11][15] =   -1.7608;
A[11][16] =    1.0486;
A[11][17] =    0.8294;
A[11][18] =    0.4338;
A[11][19] =   -2.5895;
A[11][20] =    2.4417;
A[11][21] =    1.3299;
A[11][22] =   -0.3425;
A[11][23] =    2.7017;
A[11][24] =    0.9848;
A[11][25] =    2.6234;
A[11][26] =    3.3754;
A[11][27] =    1.4942;
A[11][28] =    3.9083;
A[11][29] =   -3.9334;
A[11][30] =   -0.2588;
A[11][31] =   -1.5557;
A[11][32] =    0.1041;
A[11][33] =    0.1681;
A[11][34] =   -3.8203;
A[11][35] =   -1.0982;
A[11][36] =    0.0387;
A[11][37] =    0.6572;
A[11][38] =    0.9436;
A[11][39] =   -0.9093;
A[11][40] =    0.5502;
A[11][41] =    0.7107;
A[11][42] =   -0.6398;
A[11][43] =    0.6448;
A[11][44] =   -0.8203;
A[12][0] =    0.4867;
A[12][1] =    0.6120;
A[12][2] =    0.1409;
A[12][3] =    0.2934;
A[12][4] =    0.2815;
A[12][5] =   -1.2107;
A[12][6] =    0.1288;
A[12][7] =    0.2001;
A[12][8] =   -0.7151;
A[12][9] =    0.1337;
A[12][10] =   -1.0680;
A[12][11] =    0.0195;
A[12][12] =   -1.0507;
A[12][13] =   -0.7207;
A[12][14] =   -0.4031;
A[12][15] =   -0.8518;
A[12][16] =   -0.6635;
A[12][17] =    0.6269;
A[12][18] =    2.3112;
A[12][19] =    1.1126;
A[12][20] =   -0.6151;
A[12][21] =    1.1903;
A[12][22] =    0.8632;
A[12][23] =   -0.8085;
A[12][24] =    1.1711;
A[12][25] =    2.4973;
A[12][26] =   -2.2393;
A[12][27] =    1.4082;
A[12][28] =    1.7784;
A[12][29] =    2.0764;
A[12][30] =    4.3292;
A[12][31] =   -0.6239;
A[12][32] =    5.4744;
A[12][33] =   -0.9719;
A[12][34] =    1.7933;
A[12][35] =   -3.8013;
A[12][36] =    0.2458;
A[12][37] =   -0.3097;
A[12][38] =    0.5010;
A[12][39] =   -0.2381;
A[12][40] =   -0.5093;
A[12][41] =   -0.0727;
A[12][42] =   -0.8496;
A[12][43] =    0.1640;
A[12][44] =    0.3223;
A[13][0] =   -0.4098;
A[13][1] =   -0.6520;
A[13][2] =   -0.1966;
A[13][3] =   -0.9619;
A[13][4] =    0.0591;
A[13][5] =   -0.4054;
A[13][6] =    1.1630;
A[13][7] =    0.9918;
A[13][8] =   -1.2447;
A[13][9] =    0.7602;
A[13][10] =   -1.2284;
A[13][11] =   -1.6648;
A[13][12] =    0.7733;
A[13][13] =   -0.3303;
A[13][14] =   -0.2532;
A[13][15] =    1.4073;
A[13][16] =   -0.5367;
A[13][17] =    0.1769;
A[13][18] =   -0.1006;
A[13][19] =    1.5788;
A[13][20] =    1.4327;
A[13][21] =    0.2920;
A[13][22] =    1.5864;
A[13][23] =   -2.1828;
A[13][24] =   -1.1737;
A[13][25] =    2.4049;
A[13][26] =    0.3316;
A[13][27] =    0.4056;
A[13][28] =   -0.9574;
A[13][29] =   -2.1743;
A[13][30] =   -1.5571;
A[13][31] =    4.6101;
A[13][32] =   -3.1612;
A[13][33] =   -0.5554;
A[13][34] =    3.7921;
A[13][35] =    2.9779;
A[13][36] =   -0.8714;
A[13][37] =    0.6535;
A[13][38] =    0.5781;
A[13][39] =   -0.3838;
A[13][40] =   -0.4136;
A[13][41] =   -0.0107;
A[13][42] =   -0.9828;
A[13][43] =    0.8340;
A[13][44] =    0.6399;
A[14][0] =   -0.2176;
A[14][1] =   -0.6327;
A[14][2] =    0.3028;
A[14][3] =   -0.7652;
A[14][4] =   -0.6589;
A[14][5] =   -0.0731;
A[14][6] =   -1.0948;
A[14][7] =    1.0074;
A[14][8] =   -1.1755;
A[14][9] =    1.3478;
A[14][10] =    1.2236;
A[14][11] =    1.0951;
A[14][12] =    1.3550;
A[14][13] =    1.8365;
A[14][14] =    0.7083;
A[14][15] =    0.7656;
A[14][16] =   -0.8996;
A[14][17] =   -1.3726;
A[14][18] =    2.0615;
A[14][19] =   -1.9675;
A[14][20] =   -1.5705;
A[14][21] =   -2.0418;
A[14][22] =   -0.3120;
A[14][23] =    2.7260;
A[14][24] =    0.4454;
A[14][25] =    2.9752;
A[14][26] =    0.1386;
A[14][27] =    1.0694;
A[14][28] =    3.7034;
A[14][29] =   -0.6161;
A[14][30] =   -0.8779;
A[14][31] =    0.5170;
A[14][32] =   -4.5535;
A[14][33] =    0.0072;
A[14][34] =   -1.4254;
A[14][35] =    0.1958;
A[14][36] =    0.9314;
A[14][37] =   -0.3770;
A[14][38] =    0.6171;
A[14][39] =    0.8644;
A[14][40] =    0.7248;
A[14][41] =   -0.6869;
A[14][42] =   -0.2677;
A[14][43] =    0.7262;
A[14][44] =    0.1557;
A[15][0] =    0.5397;
A[15][1] =   -0.6544;
A[15][2] =    0.5094;
A[15][3] =    0.0667;
A[15][4] =   -0.0488;
A[15][5] =    0.1417;
A[15][6] =    1.2595;
A[15][7] =    0.0204;
A[15][8] =   -0.0664;
A[15][9] =    1.2729;
A[15][10] =   -0.2360;
A[15][11] =    1.3041;
A[15][12] =    1.7168;
A[15][13] =    0.1865;
A[15][14] =    1.3692;
A[15][15] =   -0.2057;
A[15][16] =   -0.0510;
A[15][17] =    1.0768;
A[15][18] =    1.5365;
A[15][19] =    1.0649;
A[15][20] =    0.9063;
A[15][21] =   -2.3669;
A[15][22] =    2.4650;
A[15][23] =    3.0573;
A[15][24] =   -1.6144;
A[15][25] =   -1.0772;
A[15][26] =   -1.8539;
A[15][27] =    2.6910;
A[15][28] =    0.4969;
A[15][29] =    3.5706;
A[15][30] =   -2.5485;
A[15][31] =   -2.4910;
A[15][32] =    1.1614;
A[15][33] =    1.6422;
A[15][34] =   -3.4321;
A[15][35] =   -3.8593;
A[15][36] =    0.8111;
A[15][37] =    0.9666;
A[15][38] =   -0.0105;
A[15][39] =   -0.9098;
A[15][40] =   -0.2074;
A[15][41] =   -0.9505;
A[15][42] =   -0.6798;
A[15][43] =    0.8811;
A[15][44] =   -0.4155;
A[16][0] =   -0.7161;
A[16][1] =   -0.4268;
A[16][2] =    0.6097;
A[16][3] =    0.3016;
A[16][4] =    1.0161;
A[16][5] =   -1.2939;
A[16][6] =    0.3208;
A[16][7] =   -0.9979;
A[16][8] =   -0.9284;
A[16][9] =    0.8385;
A[16][10] =   -1.6317;
A[16][11] =   -0.8154;
A[16][12] =   -0.7701;
A[16][13] =   -1.3392;
A[16][14] =   -1.2501;
A[16][15] =    0.9039;
A[16][16] =    0.8020;
A[16][17] =    0.5090;
A[16][18] =   -0.3838;
A[16][19] =   -0.2430;
A[16][20] =    1.3526;
A[16][21] =    1.8729;
A[16][22] =    0.0525;
A[16][23] =   -0.7615;
A[16][24] =   -3.1145;
A[16][25] =   -2.5188;
A[16][26] =    2.6703;
A[16][27] =    0.7838;
A[16][28] =   -2.6197;
A[16][29] =    0.0759;
A[16][30] =   -2.0053;
A[16][31] =    1.8166;
A[16][32] =    2.3327;
A[16][33] =   -0.5810;
A[16][34] =    5.5655;
A[16][35] =   -1.8390;
A[16][36] =    0.6793;
A[16][37] =   -0.3254;
A[16][38] =   -0.1691;
A[16][39] =    0.4654;
A[16][40] =    0.0283;
A[16][41] =    0.7061;
A[16][42] =   -0.5883;
A[16][43] =   -0.8008;
A[16][44] =   -0.3532;
A[17][0] =    0.5747;
A[17][1] =    0.4190;
A[17][2] =    0.1562;
A[17][3] =   -0.9983;
A[17][4] =   -0.7314;
A[17][5] =    1.2703;
A[17][6] =   -1.0869;
A[17][7] =   -0.6064;
A[17][8] =   -0.4375;
A[17][9] =    0.4257;
A[17][10] =   -1.2599;
A[17][11] =   -1.1889;
A[17][12] =   -1.5398;
A[17][13] =   -0.9503;
A[17][14] =   -1.0547;
A[17][15] =   -1.6556;
A[17][16] =   -1.6377;
A[17][17] =   -1.9842;
A[17][18] =   -0.0581;
A[17][19] =    0.4499;
A[17][20] =   -2.4010;
A[17][21] =   -2.5943;
A[17][22] =   -1.6701;
A[17][23] =    2.5267;
A[17][24] =   -0.1514;
A[17][25] =    0.8602;
A[17][26] =   -1.9662;
A[17][27] =   -2.7558;
A[17][28] =    2.5617;
A[17][29] =    0.8295;
A[17][30] =   -3.9809;
A[17][31] =   -1.2964;
A[17][32] =    1.5214;
A[17][33] =    0.7607;
A[17][34] =   -5.1563;
A[17][35] =   -1.9114;
A[17][36] =    0.3831;
A[17][37] =    0.9435;
A[17][38] =   -0.4805;
A[17][39] =    0.7313;
A[17][40] =   -0.5798;
A[17][41] =    0.4535;
A[17][42] =    0.6945;
A[17][43] =   -0.5222;
A[17][44] =   -0.8584;
A[18][0] =   -0.5299;
A[18][1] =    0.2766;
A[18][2] =   -1.0773;
A[18][3] =    0.9935;
A[18][4] =    0.6499;
A[18][5] =    0.4052;
A[18][6] =   -0.0851;
A[18][7] =    1.2887;
A[18][8] =   -1.0430;
A[18][9] =    0.5206;
A[18][10] =   -1.3396;
A[18][11] =   -0.7576;
A[18][12] =   -1.9330;
A[18][13] =   -0.6433;
A[18][14] =    0.1872;
A[18][15] =   -0.2815;
A[18][16] =    1.4418;
A[18][17] =    0.4446;
A[18][18] =    0.1759;
A[18][19] =    0.0876;
A[18][20] =   -1.2459;
A[18][21] =   -0.9745;
A[18][22] =    2.7477;
A[18][23] =   -0.0233;
A[18][24] =    2.9334;
A[18][25] =    2.4399;
A[18][26] =   -1.9168;
A[18][27] =   -1.0299;
A[18][28] =    1.0821;
A[18][29] =    1.4285;
A[18][30] =   -0.1038;
A[18][31] =    4.4794;
A[18][32] =    2.3068;
A[18][33] =   -0.9565;
A[18][34] =   -4.6319;
A[18][35] =    2.3805;
A[18][36] =   -0.5645;
A[18][37] =   -0.6223;
A[18][38] =   -0.5352;
A[18][39] =   -0.4830;
A[18][40] =   -0.2852;
A[18][41] =   -0.4947;
A[18][42] =   -0.9773;
A[18][43] =   -0.5881;
A[18][44] =   -0.6602;
A[19][0] =    0.5296;
A[19][1] =   -0.3920;
A[19][2] =   -0.0198;
A[19][3] =   -0.0348;
A[19][4] =   -1.0104;
A[19][5] =   -0.1967;
A[19][6] =    1.3257;
A[19][7] =   -1.3709;
A[19][8] =    0.2454;
A[19][9] =    0.0627;
A[19][10] =   -0.2118;
A[19][11] =   -1.1123;
A[19][12] =   -0.4761;
A[19][13] =   -1.5680;
A[19][14] =    1.3576;
A[19][15] =    0.1271;
A[19][16] =    1.9935;
A[19][17] =    1.1508;
A[19][18] =    1.5337;
A[19][19] =    0.0924;
A[19][20] =   -2.8380;
A[19][21] =   -1.2105;
A[19][22] =   -2.5168;
A[19][23] =   -1.0649;
A[19][24] =   -2.1162;
A[19][25] =   -1.5138;
A[19][26] =    2.7133;
A[19][27] =   -2.5170;
A[19][28] =   -2.8398;
A[19][29] =   -1.3835;
A[19][30] =    4.2182;
A[19][31] =   -1.3505;
A[19][32] =    3.2931;
A[19][33] =    0.7256;
A[19][34] =    5.8341;
A[19][35] =   -2.2808;
A[19][36] =   -0.3766;
A[19][37] =   -0.4535;
A[19][38] =    0.7083;
A[19][39] =   -0.8594;
A[19][40] =    0.1339;
A[19][41] =    0.1557;
A[19][42] =    0.4885;
A[19][43] =    0.3607;
A[19][44] =    0.7060;
A[20][0] =   -0.3889;
A[20][1] =   -0.6939;
A[20][2] =   -0.5591;
A[20][3] =    1.1800;
A[20][4] =   -0.6367;
A[20][5] =    0.0980;
A[20][6] =    0.8313;
A[20][7] =    0.7475;
A[20][8] =   -0.8019;
A[20][9] =   -0.8834;
A[20][10] =    0.1239;
A[20][11] =   -0.2961;
A[20][12] =    1.2986;
A[20][13] =   -0.2978;
A[20][14] =   -0.0960;
A[20][15] =   -2.1930;
A[20][16] =   -1.0142;
A[20][17] =   -0.0250;
A[20][18] =    0.7784;
A[20][19] =    0.4699;
A[20][20] =   -0.4764;
A[20][21] =    0.3037;
A[20][22] =   -2.4856;
A[20][23] =   -2.9351;
A[20][24] =    1.0384;
A[20][25] =   -0.7200;
A[20][26] =    1.2236;
A[20][27] =   -0.7624;
A[20][28] =   -3.0082;
A[20][29] =    2.5973;
A[20][30] =    2.8786;
A[20][31] =    1.5089;
A[20][32] =    0.1502;
A[20][33] =    2.0410;
A[20][34] =   -3.6555;
A[20][35] =    3.5695;
A[20][36] =    0.3721;
A[20][37] =    0.9508;
A[20][38] =   -0.7814;
A[20][39] =   -0.3657;
A[20][40] =    0.2743;
A[20][41] =    0.4129;
A[20][42] =    0.9629;
A[20][43] =   -0.5712;
A[20][44] =    0.1778;
A[21][0] =    0.2405;
A[21][1] =    0.1582;
A[21][2] =    0.7292;
A[21][3] =    0.4609;
A[21][4] =   -0.3312;
A[21][5] =    1.3701;
A[21][6] =    1.2149;
A[21][7] =   -0.2584;
A[21][8] =   -0.5328;
A[21][9] =   -0.9540;
A[21][10] =   -1.0589;
A[21][11] =   -1.6426;
A[21][12] =   -0.8473;
A[21][13] =    0.5265;
A[21][14] =   -1.0692;
A[21][15] =   -2.0129;
A[21][16] =   -1.6737;
A[21][17] =   -0.3263;
A[21][18] =    0.1845;
A[21][19] =    1.8331;
A[21][20] =    0.3957;
A[21][21] =    0.5381;
A[21][22] =    1.3779;
A[21][23] =    3.1153;
A[21][24] =   -0.6480;
A[21][25] =   -1.8880;
A[21][26] =   -0.9932;
A[21][27] =    1.1045;
A[21][28] =    0.8787;
A[21][29] =   -2.1048;
A[21][30] =   -2.6167;
A[21][31] =    0.7982;
A[21][32] =    4.6991;
A[21][33] =   -1.5583;
A[21][34] =    5.4780;
A[21][35] =    1.1221;
A[21][36] =    0.3023;
A[21][37] =    0.0962;
A[21][38] =   -0.0834;
A[21][39] =   -0.6946;
A[21][40] =   -0.4808;
A[21][41] =   -0.9846;
A[21][42] =    0.6978;
A[21][43] =   -0.5193;
A[21][44] =   -0.2866;
A[22][0] =   -0.6538;
A[22][1] =    0.3737;
A[22][2] =    0.6281;
A[22][3] =    0.2547;
A[22][4] =    1.2747;
A[22][5] =   -0.9843;
A[22][6] =   -1.0520;
A[22][7] =    1.0714;
A[22][8] =   -0.6061;
A[22][9] =   -0.2026;
A[22][10] =    1.2554;
A[22][11] =    0.4407;
A[22][12] =   -2.0072;
A[22][13] =    1.1713;
A[22][14] =    0.8285;
A[22][15] =    1.5960;
A[22][16] =    1.8929;
A[22][17] =   -1.9752;
A[22][18] =    1.0631;
A[22][19] =    0.4731;
A[22][20] =    1.2308;
A[22][21] =    3.0460;
A[22][22] =   -1.3486;
A[22][23] =   -3.1454;
A[22][24] =   -0.7611;
A[22][25] =    3.1345;
A[22][26] =    1.4752;
A[22][27] =    1.5217;
A[22][28] =    0.3302;
A[22][29] =   -3.0010;
A[22][30] =   -2.1724;
A[22][31] =    0.2842;
A[22][32] =    0.0040;
A[22][33] =   -0.6663;
A[22][34] =    4.1056;
A[22][35] =    3.3892;
A[22][36] =    0.9937;
A[22][37] =   -0.6180;
A[22][38] =    0.1184;
A[22][39] =   -0.1066;
A[22][40] =    0.8765;
A[22][41] =   -0.4337;
A[22][42] =   -0.2679;
A[22][43] =   -0.5768;
A[22][44] =   -0.4094;
A[23][0] =   -0.1166;
A[23][1] =   -0.2328;
A[23][2] =   -0.6193;
A[23][3] =    0.5451;
A[23][4] =    0.4733;
A[23][5] =    1.4017;
A[23][6] =   -0.9892;
A[23][7] =    0.3165;
A[23][8] =    0.5472;
A[23][9] =   -1.0563;
A[23][10] =    0.8513;
A[23][11] =    1.3706;
A[23][12] =   -0.7727;
A[23][13] =    0.7329;
A[23][14] =    1.2477;
A[23][15] =   -1.1261;
A[23][16] =    0.3724;
A[23][17] =    0.0093;
A[23][18] =   -0.8168;
A[23][19] =    2.2280;
A[23][20] =   -1.9213;
A[23][21] =   -0.4709;
A[23][22] =   -0.4649;
A[23][23] =    0.8960;
A[23][24] =    1.1743;
A[23][25] =    0.2368;
A[23][26] =    2.5624;
A[23][27] =    1.7307;
A[23][28] =    2.1608;
A[23][29] =    1.0134;
A[23][30] =    4.2509;
A[23][31] =    4.8416;
A[23][32] =   -5.4039;
A[23][33] =    1.1620;
A[23][34] =    1.2755;
A[23][35] =    0.3931;
A[23][36] =   -0.7017;
A[23][37] =   -0.8187;
A[23][38] =    0.3185;
A[23][39] =   -0.0048;
A[23][40] =   -0.8282;
A[23][41] =   -0.3845;
A[23][42] =   -0.8864;
A[23][43] =   -0.6492;
A[23][44] =   -0.5028;
A[24][0] =   -0.6682;
A[24][1] =   -0.6294;
A[24][2] =    0.0996;
A[24][3] =   -0.8610;
A[24][4] =    0.6989;
A[24][5] =    0.3391;
A[24][6] =    0.3573;
A[24][7] =   -0.7911;
A[24][8] =    1.3153;
A[24][9] =   -1.2309;
A[24][10] =   -0.9595;
A[24][11] =    0.3748;
A[24][12] =   -0.5857;
A[24][13] =    1.0568;
A[24][14] =    1.7367;
A[24][15] =   -0.0300;
A[24][16] =   -1.6420;
A[24][17] =   -1.9459;
A[24][18] =    1.2891;
A[24][19] =   -0.0151;
A[24][20] =   -0.2439;
A[24][21] =    0.3327;
A[24][22] =   -2.6995;
A[24][23] =    0.2583;
A[24][24] =   -0.7246;
A[24][25] =   -1.9524;
A[24][26] =    2.8550;
A[24][27] =    0.0166;
A[24][28] =    2.2852;
A[24][29] =   -0.4899;
A[24][30] =   -1.1487;
A[24][31] =   -0.7548;
A[24][32] =    0.5080;
A[24][33] =    0.6251;
A[24][34] =    5.5554;
A[24][35] =    3.5524;
A[24][36] =    0.3592;
A[24][37] =   -0.5025;
A[24][38] =   -0.7555;
A[24][39] =   -0.7752;
A[24][40] =   -0.2223;
A[24][41] =    0.3511;
A[24][42] =    0.0706;
A[24][43] =    0.9065;
A[24][44] =   -0.6698;
A[25][0] =   -0.2774;
A[25][1] =    0.6646;
A[25][2] =   -0.8563;
A[25][3] =    0.2633;
A[25][4] =   -0.8247;
A[25][5] =   -1.3426;
A[25][6] =    0.7804;
A[25][7] =    0.3414;
A[25][8] =   -0.0312;
A[25][9] =    1.5469;
A[25][10] =    1.3279;
A[25][11] =    1.7189;
A[25][12] =    0.5820;
A[25][13] =   -0.4701;
A[25][14] =   -0.2987;
A[25][15] =    1.6428;
A[25][16] =   -0.5137;
A[25][17] =    0.4823;
A[25][18] =   -1.8072;
A[25][19] =   -2.0846;
A[25][20] =   -2.4535;
A[25][21] =    1.4972;
A[25][22] =    1.2201;
A[25][23] =   -1.6664;
A[25][24] =   -2.4119;
A[25][25] =    3.2749;
A[25][26] =    3.0888;
A[25][27] =    2.3460;
A[25][28] =    3.1439;
A[25][29] =   -4.0147;
A[25][30] =    3.4937;
A[25][31] =   -3.2325;
A[25][32] =    1.5478;
A[25][33] =    1.0301;
A[25][34] =    4.1496;
A[25][35] =   -0.4238;
A[25][36] =   -0.6005;
A[25][37] =    0.1206;
A[25][38] =    0.4396;
A[25][39] =   -0.3706;
A[25][40] =   -0.7922;
A[25][41] =   -0.8534;
A[25][42] =    0.8483;
A[25][43] =   -0.1728;
A[25][44] =   -0.7036;
A[26][0] =   -0.5877;
A[26][1] =   -0.7126;
A[26][2] =    0.1958;
A[26][3] =    0.0363;
A[26][4] =    0.0816;
A[26][5] =   -0.9004;
A[26][6] =   -0.1997;
A[26][7] =    0.0338;
A[26][8] =    1.4937;
A[26][9] =   -0.4780;
A[26][10] =   -0.1520;
A[26][11] =    1.6573;
A[26][12] =   -0.1384;
A[26][13] =    1.3366;
A[26][14] =   -0.2389;
A[26][15] =   -0.7959;
A[26][16] =    1.2394;
A[26][17] =   -0.3875;
A[26][18] =   -0.3469;
A[26][19] =   -1.7654;
A[26][20] =   -0.1724;
A[26][21] =    1.4522;
A[26][22] =   -0.1517;
A[26][23] =    2.5613;
A[26][24] =   -1.5925;
A[26][25] =   -2.5395;
A[26][26] =   -0.3470;
A[26][27] =    2.3237;
A[26][28] =   -2.1855;
A[26][29] =   -3.5252;
A[26][30] =    2.7833;
A[26][31] =   -3.1533;
A[26][32] =    2.9771;
A[26][33] =    1.5409;
A[26][34] =    6.4084;
A[26][35] =    3.1813;
A[26][36] =    0.7485;
A[26][37] =    0.9995;
A[26][38] =   -0.8036;
A[26][39] =   -0.0538;
A[26][40] =   -0.1835;
A[26][41] =    0.4775;
A[26][42] =   -0.9535;
A[26][43] =    0.6255;
A[26][44] =   -0.6133;
A[27][0] =   -0.1053;
A[27][1] =   -0.0606;
A[27][2] =    0.9171;
A[27][3] =    0.0481;
A[27][4] =   -0.8885;
A[27][5] =    0.1172;
A[27][6] =    1.2122;
A[27][7] =    1.2073;
A[27][8] =    1.4496;
A[27][9] =    0.9084;
A[27][10] =    0.7721;
A[27][11] =   -0.0302;
A[27][12] =   -0.5593;
A[27][13] =    1.6256;
A[27][14] =    0.5619;
A[27][15] =   -1.4789;
A[27][16] =    0.6263;
A[27][17] =    1.7109;
A[27][18] =    1.9645;
A[27][19] =    0.9272;
A[27][20] =   -0.0041;
A[27][21] =    1.5230;
A[27][22] =    2.3736;
A[27][23] =   -2.1670;
A[27][24] =    2.3271;
A[27][25] =   -0.3834;
A[27][26] =   -2.1593;
A[27][27] =   -3.1951;
A[27][28] =    0.6031;
A[27][29] =    2.9344;
A[27][30] =    3.5544;
A[27][31] =    4.9000;
A[27][32] =    0.6265;
A[27][33] =   -1.6144;
A[27][34] =   -6.5506;
A[27][35] =    2.9771;
A[27][36] =   -0.3418;
A[27][37] =    0.0738;
A[27][38] =   -0.5297;
A[27][39] =    0.5402;
A[27][40] =    0.8150;
A[27][41] =   -0.5888;
A[27][42] =   -0.1562;
A[27][43] =   -0.0866;
A[27][44] =   -0.2050;
A[28][0] =   -0.5960;
A[28][1] =   -0.5278;
A[28][2] =   -0.5362;
A[28][3] =    0.5016;
A[28][4] =   -1.2266;
A[28][5] =    0.9195;
A[28][6] =   -0.1885;
A[28][7] =    0.1780;
A[28][8] =   -0.6445;
A[28][9] =    1.6593;
A[28][10] =   -1.0759;
A[28][11] =    1.8045;
A[28][12] =   -0.3782;
A[28][13] =    1.3664;
A[28][14] =   -1.3562;
A[28][15] =    2.1109;
A[28][16] =    1.2056;
A[28][17] =   -0.7522;
A[28][18] =   -1.2414;
A[28][19] =   -1.7985;
A[28][20] =    1.0102;
A[28][21] =   -0.3595;
A[28][22] =    1.6179;
A[28][23] =   -0.4707;
A[28][24] =    2.4236;
A[28][25] =   -3.2017;
A[28][26] =    2.5347;
A[28][27] =   -2.5643;
A[28][28] =    2.3642;
A[28][29] =   -0.8352;
A[28][30] =   -3.4791;
A[28][31] =   -3.5013;
A[28][32] =   -0.3970;
A[28][33] =    1.5024;
A[28][34] =   -3.6463;
A[28][35] =   -1.7169;
A[28][36] =    0.5943;
A[28][37] =    0.7348;
A[28][38] =   -0.1234;
A[28][39] =    0.9121;
A[28][40] =   -0.4602;
A[28][41] =    0.9495;
A[28][42] =    0.6470;
A[28][43] =    0.6187;
A[28][44] =    0.1719;
A[29][0] =   -0.0612;
A[29][1] =   -0.5253;
A[29][2] =    0.9951;
A[29][3] =   -0.9152;
A[29][4] =   -0.3679;
A[29][5] =   -0.0473;
A[29][6] =    0.3515;
A[29][7] =   -0.4815;
A[29][8] =    0.6864;
A[29][9] =    0.5117;
A[29][10] =   -0.4301;
A[29][11] =   -0.9971;
A[29][12] =   -0.0342;
A[29][13] =    1.6272;
A[29][14] =   -1.3766;
A[29][15] =    0.3604;
A[29][16] =    1.6102;
A[29][17] =    0.4464;
A[29][18] =    0.0108;
A[29][19] =   -0.8669;
A[29][20] =    1.8157;
A[29][21] =   -0.6319;
A[29][22] =    0.1247;
A[29][23] =    0.5329;
A[29][24] =    2.8977;
A[29][25] =    2.5859;
A[29][26] =   -1.1906;
A[29][27] =   -1.4653;
A[29][28] =   -0.2431;
A[29][29] =    2.1712;
A[29][30] =    0.1238;
A[29][31] =   -0.4795;
A[29][32] =   -3.5991;
A[29][33] =   -2.3193;
A[29][34] =    5.2976;
A[29][35] =    2.6797;
A[29][36] =    0.9658;
A[29][37] =    0.3637;
A[29][38] =    0.7007;
A[29][39] =    0.3041;
A[29][40] =   -0.5107;
A[29][41] =    0.2583;
A[29][42] =    0.3529;
A[29][43] =   -0.6662;
A[29][44] =   -0.2076;
A[30][0] =   -0.0537;
A[30][1] =    0.6875;
A[30][2] =    0.1673;
A[30][3] =   -0.0539;
A[30][4] =   -0.9127;
A[30][5] =   -1.4011;
A[30][6] =    1.2637;
A[30][7] =   -1.3757;
A[30][8] =    0.1868;
A[30][9] =   -1.7194;
A[30][10] =   -1.3698;
A[30][11] =   -1.0940;
A[30][12] =   -1.3865;
A[30][13] =   -0.9015;
A[30][14] =    0.1199;
A[30][15] =    2.1326;
A[30][16] =   -0.1300;
A[30][17] =   -1.4035;
A[30][18] =    1.0309;
A[30][19] =    0.8363;
A[30][20] =    0.5075;
A[30][21] =   -1.5047;
A[30][22] =   -0.4350;
A[30][23] =   -1.8855;
A[30][24] =   -0.6528;
A[30][25] =    1.2540;
A[30][26] =    1.1187;
A[30][27] =   -2.7488;
A[30][28] =    0.6487;
A[30][29] =   -1.9254;
A[30][30] =    3.6771;
A[30][31] =    0.5914;
A[30][32] =    0.3156;
A[30][33] =    1.7596;
A[30][34] =    1.1972;
A[30][35] =   -3.4195;
A[30][36] =   -0.4976;
A[30][37] =   -0.8200;
A[30][38] =    0.6416;
A[30][39] =   -0.1323;
A[30][40] =    0.2801;
A[30][41] =   -0.1101;
A[30][42] =    0.1520;
A[30][43] =    0.4043;
A[30][44] =    0.3811;
A[31][0] =   -0.0059;
A[31][1] =   -0.6667;
A[31][2] =    0.4535;
A[31][3] =   -0.3361;
A[31][4] =    0.3882;
A[31][5] =    0.8510;
A[31][6] =    1.0013;
A[31][7] =    0.1036;
A[31][8] =   -1.4652;
A[31][9] =   -1.1034;
A[31][10] =   -0.6314;
A[31][11] =   -0.7604;
A[31][12] =   -1.4287;
A[31][13] =   -0.2087;
A[31][14] =   -2.0122;
A[31][15] =    1.8302;
A[31][16] =   -1.3524;
A[31][17] =    1.4887;
A[31][18] =   -0.0694;
A[31][19] =   -0.5629;
A[31][20] =    1.4746;
A[31][21] =   -0.4873;
A[31][22] =    1.5627;
A[31][23] =   -2.5402;
A[31][24] =    1.6623;
A[31][25] =   -0.7509;
A[31][26] =   -0.2056;
A[31][27] =    0.7647;
A[31][28] =   -3.4085;
A[31][29] =   -2.2459;
A[31][30] =   -3.9191;
A[31][31] =   -2.2382;
A[31][32] =    2.2702;
A[31][33] =   -1.7130;
A[31][34] =   -4.7866;
A[31][35] =    1.3062;
A[31][36] =    0.5666;
A[31][37] =   -0.9376;
A[31][38] =   -0.0462;
A[31][39] =   -0.6392;
A[31][40] =    0.8172;
A[31][41] =    0.1053;
A[31][42] =    0.7973;
A[31][43] =    0.6686;
A[31][44] =   -0.8712;
A[32][0] =    0.5716;
A[32][1] =    0.1302;
A[32][2] =   -0.0576;
A[32][3] =   -0.3666;
A[32][4] =    0.0627;
A[32][5] =    0.6273;
A[32][6] =   -0.8533;
A[32][7] =   -0.8560;
A[32][8] =    0.7677;
A[32][9] =   -0.1186;
A[32][10] =    0.0211;
A[32][11] =   -0.6263;
A[32][12] =   -0.1949;
A[32][13] =   -1.4816;
A[32][14] =    0.5845;
A[32][15] =    1.7370;
A[32][16] =    1.1076;
A[32][17] =    0.3725;
A[32][18] =    1.3414;
A[32][19] =    1.3627;
A[32][20] =    1.0594;
A[32][21] =   -0.2724;
A[32][22] =   -0.6028;
A[32][23] =    1.4170;
A[32][24] =    1.4777;
A[32][25] =   -3.1002;
A[32][26] =    3.0070;
A[32][27] =    2.8115;
A[32][28] =   -1.9868;
A[32][29] =    1.5700;
A[32][30] =    1.0233;
A[32][31] =    4.8074;
A[32][32] =    4.8421;
A[32][33] =   -0.8283;
A[32][34] =    3.4469;
A[32][35] =    0.4506;
A[32][36] =    0.5355;
A[32][37] =   -0.8503;
A[32][38] =    0.9311;
A[32][39] =    0.0971;
A[32][40] =   -0.9690;
A[32][41] =   -0.5656;
A[32][42] =   -0.2686;
A[32][43] =   -0.7753;
A[32][44] =   -0.6005;
A[33][0] =   -0.6710;
A[33][1] =   -0.2034;
A[33][2] =    0.7649;
A[33][3] =    0.5728;
A[33][4] =    1.0004;
A[33][5] =   -0.0660;
A[33][6] =    1.3776;
A[33][7] =   -0.6373;
A[33][8] =   -0.1231;
A[33][9] =    0.8494;
A[33][10] =    0.5005;
A[33][11] =    0.6699;
A[33][12] =   -1.5638;
A[33][13] =    0.1644;
A[33][14] =   -0.8100;
A[33][15] =   -1.8695;
A[33][16] =    1.9142;
A[33][17] =   -1.2875;
A[33][18] =    2.3304;
A[33][19] =   -0.6240;
A[33][20] =    1.8857;
A[33][21] =   -1.7274;
A[33][22] =   -0.0778;
A[33][23] =   -0.9914;
A[33][24] =   -1.7731;
A[33][25] =    2.9113;
A[33][26] =    1.7303;
A[33][27] =    2.1932;
A[33][28] =   -2.2986;
A[33][29] =   -1.4647;
A[33][30] =    0.9210;
A[33][31] =    0.3614;
A[33][32] =   -4.6331;
A[33][33] =    1.9650;
A[33][34] =    5.2408;
A[33][35] =   -1.7899;
A[33][36] =    0.0803;
A[33][37] =   -0.1242;
A[33][38] =   -0.6272;
A[33][39] =   -0.9752;
A[33][40] =   -0.2909;
A[33][41] =   -0.1148;
A[33][42] =   -0.5389;
A[33][43] =   -0.2344;
A[33][44] =    0.0089;
A[34][0] =   -0.2428;
A[34][1] =    0.2607;
A[34][2] =    0.7653;
A[34][3] =    0.2685;
A[34][4] =    0.0765;
A[34][5] =   -0.6960;
A[34][6] =   -0.0517;
A[34][7] =    0.3618;
A[34][8] =    0.8434;
A[34][9] =    0.5033;
A[34][10] =   -0.5654;
A[34][11] =    1.7255;
A[34][12] =    0.5854;
A[34][13] =    1.3457;
A[34][14] =    0.9961;
A[34][15] =    0.5077;
A[34][16] =   -0.8857;
A[34][17] =    1.0733;
A[34][18] =   -1.4513;
A[34][19] =    0.7522;
A[34][20] =   -0.2137;
A[34][21] =   -2.6628;
A[34][22] =   -2.8285;
A[34][23] =    3.1844;
A[34][24] =   -1.2945;
A[34][25] =    2.0090;
A[34][26] =   -1.0223;
A[34][27] =   -3.4170;
A[34][28] =    0.5400;
A[34][29] =    2.6304;
A[34][30] =   -3.3025;
A[34][31] =    4.5930;
A[34][32] =    2.5606;
A[34][33] =    0.6011;
A[34][34] =    1.1866;
A[34][35] =   -2.3809;
A[34][36] =   -0.0170;
A[34][37] =   -0.0978;
A[34][38] =    0.6192;
A[34][39] =   -0.7085;
A[34][40] =    0.4788;
A[34][41] =   -0.1202;
A[34][42] =   -0.3267;
A[34][43] =   -0.3782;
A[34][44] =   -0.8359;
A[35][0] =   -0.4419;
A[35][1] =    0.3003;
A[35][2] =    0.2716;
A[35][3] =    0.0161;
A[35][4] =    1.0354;
A[35][5] =    0.2826;
A[35][6] =    0.5739;
A[35][7] =    0.7688;
A[35][8] =   -0.6109;
A[35][9] =    1.0887;
A[35][10] =    1.4323;
A[35][11] =    0.5639;
A[35][12] =   -0.1951;
A[35][13] =    1.9994;
A[35][14] =    1.8224;
A[35][15] =    0.7558;
A[35][16] =   -0.4613;
A[35][17] =   -0.4740;
A[35][18] =   -1.3783;
A[35][19] =    1.5698;
A[35][20] =    0.2492;
A[35][21] =   -0.7520;
A[35][22] =    1.6545;
A[35][23] =   -3.0752;
A[35][24] =    2.8067;
A[35][25] =    0.2678;
A[35][26] =    2.7777;
A[35][27] =    1.2784;
A[35][28] =   -2.5147;
A[35][29] =    1.5097;
A[35][30] =    3.2427;
A[35][31] =   -4.3749;
A[35][32] =   -0.8131;
A[35][33] =   -2.1010;
A[35][34] =   -4.4070;
A[35][35] =    2.2546;
A[35][36] =    0.0768;
A[35][37] =    0.1696;
A[35][38] =    0.5068;
A[35][39] =   -0.1085;
A[35][40] =   -0.4252;
A[35][41] =    0.2718;
A[35][42] =    0.8156;
A[35][43] =    0.3590;
A[35][44] =   -0.2841;
 double b[m];
b[0] =    1.6255;
b[1] =    6.5646;
b[2] =    1.9610;
b[3] =    6.7451;
b[4] =    2.5910;
b[5] =    9.7996;
b[6] =   -3.1832;
b[7] =   -8.8993;
b[8] =    8.0953;
b[9] =    9.5424;
b[10] =    5.9015;
b[11] =   -7.8359;
b[12] =    3.1021;
b[13] =    3.1504;
b[14] =    0.6620;
b[15] =    2.6540;
b[16] =   -0.5860;
b[17] =  -10.8106;
b[18] =    7.8610;
b[19] =    1.8871;
b[20] =    6.5039;
b[21] =   -5.4452;
b[22] =   -3.5707;
b[23] =   10.1543;
b[24] =   -5.2840;
b[25] =   -1.0599;
b[26] =   -1.6146;
b[27] =    9.8860;
b[28] =    1.4519;
b[29] =    4.1568;
b[30] =   -3.5295;
b[31] =   -0.9331;
b[32] =    7.9096;
b[33] =    3.0279;
b[34] =   -8.3247;
b[35] =   11.0344;
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