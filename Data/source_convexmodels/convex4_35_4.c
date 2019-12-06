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
 n = 35;
 m = 28;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double A[m][n];
A[0][0] =    0.4604;
A[0][1] =   -0.8961;
A[0][2] =    0.3413;
A[0][3] =   -0.0955;
A[0][4] =   -0.7868;
A[0][5] =   -0.5874;
A[0][6] =   -0.8042;
A[0][7] =   -0.7637;
A[0][8] =   -1.1738;
A[0][9] =    1.6221;
A[0][10] =    1.2522;
A[0][11] =    1.7932;
A[0][12] =    1.1963;
A[0][13] =   -0.5492;
A[0][14] =    2.2160;
A[0][15] =   -2.4490;
A[0][16] =   -0.3789;
A[0][17] =   -0.8201;
A[0][18] =   -1.4231;
A[0][19] =   -2.5386;
A[0][20] =   -3.0339;
A[0][21] =    1.2535;
A[0][22] =   -0.6992;
A[0][23] =    0.5532;
A[0][24] =   -1.0909;
A[0][25] =    1.7475;
A[0][26] =    0.8860;
A[0][27] =   -0.6775;
A[0][28] =    1.1407;
A[0][29] =   -0.3881;
A[0][30] =    0.2457;
A[0][31] =    0.2433;
A[0][32] =    0.5984;
A[0][33] =    0.6266;
A[0][34] =    0.8795;
A[1][0] =   -0.6984;
A[1][1] =    0.3326;
A[1][2] =    0.9686;
A[1][3] =    0.6552;
A[1][4] =   -0.7358;
A[1][5] =    0.5929;
A[1][6] =   -0.9579;
A[1][7] =   -0.8060;
A[1][8] =    0.3953;
A[1][9] =    1.0552;
A[1][10] =   -1.2044;
A[1][11] =   -0.3358;
A[1][12] =    1.1841;
A[1][13] =    0.1532;
A[1][14] =   -1.4301;
A[1][15] =   -1.9449;
A[1][16] =   -1.5439;
A[1][17] =   -0.5295;
A[1][18] =    1.8730;
A[1][19] =   -0.6488;
A[1][20] =   -2.4929;
A[1][21] =    1.5150;
A[1][22] =   -1.6438;
A[1][23] =    3.1180;
A[1][24] =    1.5339;
A[1][25] =   -3.2038;
A[1][26] =    0.6049;
A[1][27] =    2.8182;
A[1][28] =   -2.3827;
A[1][29] =    0.8440;
A[1][30] =   -0.9105;
A[1][31] =   -0.5239;
A[1][32] =    0.9132;
A[1][33] =   -0.4240;
A[1][34] =   -0.9520;
A[2][0] =    0.1285;
A[2][1] =   -0.0766;
A[2][2] =    0.6738;
A[2][3] =   -0.3252;
A[2][4] =   -0.0340;
A[2][5] =    0.8564;
A[2][6] =   -0.8750;
A[2][7] =    0.0361;
A[2][8] =   -0.9841;
A[2][9] =   -0.7309;
A[2][10] =   -1.9896;
A[2][11] =   -1.1335;
A[2][12] =    0.1254;
A[2][13] =   -1.5662;
A[2][14] =    1.4143;
A[2][15] =   -2.5628;
A[2][16] =   -1.7153;
A[2][17] =   -0.8572;
A[2][18] =   -1.1626;
A[2][19] =    0.7127;
A[2][20] =    0.5450;
A[2][21] =   -0.7463;
A[2][22] =    2.3069;
A[2][23] =    0.9101;
A[2][24] =    0.8014;
A[2][25] =   -2.2434;
A[2][26] =    1.1066;
A[2][27] =    3.3679;
A[2][28] =   -4.9495;
A[2][29] =    0.7566;
A[2][30] =    0.0100;
A[2][31] =    0.6845;
A[2][32] =    0.1590;
A[2][33] =   -0.8002;
A[2][34] =   -0.3794;
A[3][0] =    0.4634;
A[3][1] =    0.6508;
A[3][2] =   -0.9557;
A[3][3] =   -1.0573;
A[3][4] =    0.3058;
A[3][5] =    0.3172;
A[3][6] =   -0.3108;
A[3][7] =   -1.1457;
A[3][8] =   -1.5892;
A[3][9] =    0.8707;
A[3][10] =    0.5023;
A[3][11] =   -1.2170;
A[3][12] =    2.2353;
A[3][13] =   -2.2465;
A[3][14] =    1.9465;
A[3][15] =    1.1898;
A[3][16] =   -1.4522;
A[3][17] =    2.0093;
A[3][18] =    0.3090;
A[3][19] =    2.0820;
A[3][20] =   -3.1568;
A[3][21] =    3.1362;
A[3][22] =   -0.9487;
A[3][23] =   -2.3597;
A[3][24] =    0.4269;
A[3][25] =   -1.7429;
A[3][26] =   -0.8411;
A[3][27] =   -3.2466;
A[3][28] =    3.9514;
A[3][29] =   -0.3500;
A[3][30] =    0.4771;
A[3][31] =   -0.1668;
A[3][32] =    0.1419;
A[3][33] =    0.2025;
A[3][34] =   -0.8118;
A[4][0] =    0.5622;
A[4][1] =    0.1247;
A[4][2] =    0.3361;
A[4][3] =    0.5272;
A[4][4] =    0.7369;
A[4][5] =    0.9476;
A[4][6] =    1.3799;
A[4][7] =    0.0169;
A[4][8] =   -0.1828;
A[4][9] =    0.4751;
A[4][10] =   -1.3076;
A[4][11] =   -1.7972;
A[4][12] =    0.5401;
A[4][13] =    0.6761;
A[4][14] =   -0.8007;
A[4][15] =   -2.5552;
A[4][16] =    2.2539;
A[4][17] =   -0.8798;
A[4][18] =   -2.6171;
A[4][19] =   -1.4924;
A[4][20] =   -1.0277;
A[4][21] =   -0.0053;
A[4][22] =    2.5421;
A[4][23] =    3.1415;
A[4][24] =   -2.1570;
A[4][25] =   -3.1717;
A[4][26] =   -0.8397;
A[4][27] =   -3.6621;
A[4][28] =    2.9811;
A[4][29] =   -0.6001;
A[4][30] =    0.8621;
A[4][31] =   -0.7563;
A[4][32] =   -0.5902;
A[4][33] =   -0.2594;
A[4][34] =    0.8474;
A[5][0] =   -0.3240;
A[5][1] =   -0.6824;
A[5][2] =   -0.0784;
A[5][3] =   -0.9477;
A[5][4] =    0.0024;
A[5][5] =   -1.1516;
A[5][6] =    0.1748;
A[5][7] =   -1.0141;
A[5][8] =   -0.7806;
A[5][9] =   -0.9661;
A[5][10] =    0.7582;
A[5][11] =   -0.9451;
A[5][12] =    1.8908;
A[5][13] =   -1.2841;
A[5][14] =   -0.6705;
A[5][15] =    0.3114;
A[5][16] =   -0.8411;
A[5][17] =   -1.3315;
A[5][18] =   -0.2394;
A[5][19] =    0.2029;
A[5][20] =   -2.7323;
A[5][21] =    0.3501;
A[5][22] =    0.9355;
A[5][23] =    0.2959;
A[5][24] =    1.1005;
A[5][25] =   -0.9413;
A[5][26] =    1.5033;
A[5][27] =   -0.5761;
A[5][28] =   -4.4534;
A[5][29] =   -0.1728;
A[5][30] =    0.3693;
A[5][31] =   -0.1752;
A[5][32] =    0.7427;
A[5][33] =   -0.9213;
A[5][34] =    0.7289;
A[6][0] =   -0.5086;
A[6][1] =    0.6393;
A[6][2] =   -0.8226;
A[6][3] =   -0.8532;
A[6][4] =    0.9933;
A[6][5] =   -0.1400;
A[6][6] =   -0.1030;
A[6][7] =   -1.0291;
A[6][8] =    0.1393;
A[6][9] =    0.7851;
A[6][10] =   -1.4470;
A[6][11] =   -1.8045;
A[6][12] =   -1.0884;
A[6][13] =    1.2802;
A[6][14] =   -0.6924;
A[6][15] =    1.8424;
A[6][16] =    1.4129;
A[6][17] =   -2.8684;
A[6][18] =   -0.3994;
A[6][19] =   -0.0358;
A[6][20] =    1.3168;
A[6][21] =    2.2239;
A[6][22] =    3.1037;
A[6][23] =    0.5508;
A[6][24] =    1.4357;
A[6][25] =    3.1973;
A[6][26] =   -0.0477;
A[6][27] =    1.8804;
A[6][28] =   -4.6327;
A[6][29] =   -0.5973;
A[6][30] =   -0.7433;
A[6][31] =   -0.4167;
A[6][32] =   -0.2767;
A[6][33] =    0.4765;
A[6][34] =    0.1827;
A[7][0] =   -0.6443;
A[7][1] =   -0.7362;
A[7][2] =   -0.1984;
A[7][3] =    0.2207;
A[7][4] =   -0.9275;
A[7][5] =    0.0745;
A[7][6] =   -1.2676;
A[7][7] =   -0.7943;
A[7][8] =   -1.2832;
A[7][9] =   -1.6661;
A[7][10] =   -1.1667;
A[7][11] =    1.9357;
A[7][12] =    0.6496;
A[7][13] =    1.3177;
A[7][14] =   -0.5594;
A[7][15] =    2.6955;
A[7][16] =    1.2939;
A[7][17] =   -2.7491;
A[7][18] =   -1.6785;
A[7][19] =   -1.2586;
A[7][20] =    0.6489;
A[7][21] =   -3.4597;
A[7][22] =    0.0193;
A[7][23] =   -0.9379;
A[7][24] =    1.4226;
A[7][25] =   -2.8646;
A[7][26] =    0.5083;
A[7][27] =   -1.7309;
A[7][28] =   -0.7426;
A[7][29] =    0.1746;
A[7][30] =   -0.8585;
A[7][31] =    0.2300;
A[7][32] =   -0.3070;
A[7][33] =   -0.7872;
A[7][34] =    0.0794;
A[8][0] =   -0.3031;
A[8][1] =    0.3066;
A[8][2] =    0.1936;
A[8][3] =    0.0132;
A[8][4] =   -0.3612;
A[8][5] =   -0.7046;
A[8][6] =    0.0062;
A[8][7] =   -1.0256;
A[8][8] =    1.3263;
A[8][9] =   -0.2285;
A[8][10] =    0.5740;
A[8][11] =   -1.0040;
A[8][12] =   -2.0430;
A[8][13] =    2.1996;
A[8][14] =    0.2408;
A[8][15] =    0.1733;
A[8][16] =   -1.0440;
A[8][17] =    2.6925;
A[8][18] =   -2.4184;
A[8][19] =   -0.7068;
A[8][20] =    3.1295;
A[8][21] =   -1.2239;
A[8][22] =   -1.2587;
A[8][23] =   -3.5029;
A[8][24] =   -3.5103;
A[8][25] =   -0.0024;
A[8][26] =    1.1870;
A[8][27] =   -0.7713;
A[8][28] =    2.3486;
A[8][29] =   -0.5675;
A[8][30] =    0.8836;
A[8][31] =    0.0234;
A[8][32] =    0.0990;
A[8][33] =   -0.2291;
A[8][34] =    0.4973;
A[9][0] =    0.5388;
A[9][1] =   -0.4443;
A[9][2] =    0.9334;
A[9][3] =   -0.0086;
A[9][4] =   -1.0645;
A[9][5] =   -0.1247;
A[9][6] =   -0.8337;
A[9][7] =    1.2146;
A[9][8] =   -1.3131;
A[9][9] =    0.7347;
A[9][10] =   -1.2417;
A[9][11] =   -0.9400;
A[9][12] =   -1.4127;
A[9][13] =    1.0217;
A[9][14] =   -1.3435;
A[9][15] =    0.9627;
A[9][16] =    1.5227;
A[9][17] =   -2.8677;
A[9][18] =   -1.0487;
A[9][19] =   -0.3663;
A[9][20] =   -2.4023;
A[9][21] =   -0.0278;
A[9][22] =   -1.6881;
A[9][23] =   -2.2499;
A[9][24] =   -3.7334;
A[9][25] =    2.5407;
A[9][26] =   -1.3593;
A[9][27] =    2.7171;
A[9][28] =   -4.2262;
A[9][29] =   -0.7998;
A[9][30] =   -0.0012;
A[9][31] =    0.7051;
A[9][32] =    0.1324;
A[9][33] =   -0.6718;
A[9][34] =    0.6750;
A[10][0] =    0.6959;
A[10][1] =    0.7903;
A[10][2] =    0.1681;
A[10][3] =    0.5789;
A[10][4] =    0.0322;
A[10][5] =   -0.1966;
A[10][6] =    1.2767;
A[10][7] =   -1.2935;
A[10][8] =    1.3145;
A[10][9] =    1.2606;
A[10][10] =    0.2210;
A[10][11] =   -0.5889;
A[10][12] =   -1.3801;
A[10][13] =   -1.1135;
A[10][14] =    1.9134;
A[10][15] =   -1.9827;
A[10][16] =   -1.4224;
A[10][17] =    2.2540;
A[10][18] =    1.3738;
A[10][19] =   -3.1160;
A[10][20] =    1.7255;
A[10][21] =   -1.2427;
A[10][22] =   -0.4135;
A[10][23] =    0.1998;
A[10][24] =    1.0712;
A[10][25] =   -3.3258;
A[10][26] =   -0.5452;
A[10][27] =   -3.0328;
A[10][28] =   -3.1954;
A[10][29] =   -0.7341;
A[10][30] =    0.5971;
A[10][31] =    0.8441;
A[10][32] =    0.0536;
A[10][33] =   -0.1918;
A[10][34] =    0.7121;
A[11][0] =    0.5480;
A[11][1] =    0.3532;
A[11][2] =    0.1835;
A[11][3] =    0.6537;
A[11][4] =    0.4343;
A[11][5] =    0.9843;
A[11][6] =    0.7745;
A[11][7] =    0.9239;
A[11][8] =    0.6440;
A[11][9] =    1.3073;
A[11][10] =    0.8871;
A[11][11] =   -1.0787;
A[11][12] =    2.1848;
A[11][13] =    2.1907;
A[11][14] =   -2.0286;
A[11][15] =   -2.5252;
A[11][16] =   -1.9854;
A[11][17] =   -2.0958;
A[11][18] =    2.7209;
A[11][19] =    2.3302;
A[11][20] =    1.0751;
A[11][21] =    2.4847;
A[11][22] =    0.6847;
A[11][23] =    1.0833;
A[11][24] =    3.3831;
A[11][25] =   -0.3216;
A[11][26] =    1.2593;
A[11][27] =   -2.1871;
A[11][28] =    4.9820;
A[11][29] =   -0.6584;
A[11][30] =    0.9250;
A[11][31] =    0.0499;
A[11][32] =    0.8784;
A[11][33] =    0.1638;
A[11][34] =    0.0181;
A[12][0] =   -0.1628;
A[12][1] =   -0.2956;
A[12][2] =   -0.1826;
A[12][3] =   -0.4245;
A[12][4] =    1.0475;
A[12][5] =    0.9931;
A[12][6] =   -1.3330;
A[12][7] =    0.1820;
A[12][8] =   -1.5968;
A[12][9] =    0.3734;
A[12][10] =    0.7367;
A[12][11] =    0.7675;
A[12][12] =   -1.4235;
A[12][13] =   -1.6644;
A[12][14] =    1.5875;
A[12][15] =   -0.4688;
A[12][16] =   -1.7439;
A[12][17] =   -2.4174;
A[12][18] =   -3.0014;
A[12][19] =    3.4285;
A[12][20] =    3.2110;
A[12][21] =    0.9651;
A[12][22] =    2.4146;
A[12][23] =   -2.0911;
A[12][24] =    3.7861;
A[12][25] =   -1.7224;
A[12][26] =   -1.3222;
A[12][27] =   -1.0761;
A[12][28] =   -1.3187;
A[12][29] =    0.4462;
A[12][30] =   -0.6678;
A[12][31] =    0.1026;
A[12][32] =    0.5343;
A[12][33] =   -0.4700;
A[12][34] =   -0.9463;
A[13][0] =    0.7367;
A[13][1] =   -0.6244;
A[13][2] =   -0.5872;
A[13][3] =   -0.4758;
A[13][4] =   -0.7758;
A[13][5] =   -0.9869;
A[13][6] =    0.0794;
A[13][7] =   -1.4711;
A[13][8] =    1.3706;
A[13][9] =   -1.0736;
A[13][10] =    1.4046;
A[13][11] =   -2.0717;
A[13][12] =   -1.5284;
A[13][13] =    0.8995;
A[13][14] =   -2.1949;
A[13][15] =    0.4933;
A[13][16] =    0.4212;
A[13][17] =   -1.5733;
A[13][18] =    2.3844;
A[13][19] =   -2.0797;
A[13][20] =   -0.8041;
A[13][21] =    0.1000;
A[13][22] =   -1.5696;
A[13][23] =   -3.0827;
A[13][24] =    2.3415;
A[13][25] =    4.7213;
A[13][26] =   -0.8361;
A[13][27] =    2.0971;
A[13][28] =   -0.8148;
A[13][29] =   -0.9143;
A[13][30] =    0.2798;
A[13][31] =    0.4905;
A[13][32] =    0.0815;
A[13][33] =   -0.4513;
A[13][34] =   -0.8176;
A[14][0] =    0.5182;
A[14][1] =    0.1670;
A[14][2] =   -0.6171;
A[14][3] =   -0.4933;
A[14][4] =    0.3343;
A[14][5] =   -0.6769;
A[14][6] =   -0.4217;
A[14][7] =   -1.2474;
A[14][8] =   -0.7662;
A[14][9] =   -1.4187;
A[14][10] =   -1.4675;
A[14][11] =   -0.8771;
A[14][12] =   -2.1917;
A[14][13] =   -0.6225;
A[14][14] =   -2.3141;
A[14][15] =    1.4546;
A[14][16] =   -1.9610;
A[14][17] =   -0.1965;
A[14][18] =    0.0219;
A[14][19] =    2.4788;
A[14][20] =    2.2316;
A[14][21] =    1.9486;
A[14][22] =    1.3065;
A[14][23] =    1.4242;
A[14][24] =    1.3577;
A[14][25] =    0.9845;
A[14][26] =   -0.8308;
A[14][27] =   -3.2835;
A[14][28] =   -4.2111;
A[14][29] =    0.4010;
A[14][30] =   -0.6084;
A[14][31] =    0.3389;
A[14][32] =    0.5214;
A[14][33] =   -0.5197;
A[14][34] =   -0.2380;
A[15][0] =   -0.8006;
A[15][1] =    0.8764;
A[15][2] =   -0.4025;
A[15][3] =    0.5857;
A[15][4] =   -0.6268;
A[15][5] =    1.0354;
A[15][6] =   -0.8465;
A[15][7] =    0.2457;
A[15][8] =   -0.2442;
A[15][9] =    1.3947;
A[15][10] =    0.1539;
A[15][11] =    1.5028;
A[15][12] =   -0.3647;
A[15][13] =    0.9688;
A[15][14] =    0.2011;
A[15][15] =   -0.0682;
A[15][16] =    0.5445;
A[15][17] =    2.4728;
A[15][18] =    2.1233;
A[15][19] =    2.2213;
A[15][20] =    1.8278;
A[15][21] =    0.0877;
A[15][22] =   -1.9800;
A[15][23] =    0.2855;
A[15][24] =    2.6873;
A[15][25] =   -4.6489;
A[15][26] =    0.3631;
A[15][27] =   -2.2426;
A[15][28] =   -3.9147;
A[15][29] =    0.9293;
A[15][30] =   -0.7247;
A[15][31] =    0.6320;
A[15][32] =   -0.7324;
A[15][33] =    0.0491;
A[15][34] =   -0.7631;
A[16][0] =    0.1564;
A[16][1] =   -0.8198;
A[16][2] =    0.7642;
A[16][3] =   -0.5064;
A[16][4] =    0.4127;
A[16][5] =    0.5368;
A[16][6] =   -1.2625;
A[16][7] =    1.4046;
A[16][8] =   -0.4865;
A[16][9] =    1.5801;
A[16][10] =   -0.6330;
A[16][11] =   -0.9769;
A[16][12] =   -2.1394;
A[16][13] =   -1.4657;
A[16][14] =   -1.6422;
A[16][15] =   -0.6194;
A[16][16] =   -0.4766;
A[16][17] =    1.0874;
A[16][18] =   -2.3548;
A[16][19] =   -1.5000;
A[16][20] =   -2.7204;
A[16][21] =   -1.8243;
A[16][22] =   -0.3647;
A[16][23] =    1.9484;
A[16][24] =   -2.4436;
A[16][25] =    4.4503;
A[16][26] =   -1.0078;
A[16][27] =    2.1147;
A[16][28] =   -0.0758;
A[16][29] =    0.5510;
A[16][30] =    0.7494;
A[16][31] =    0.4636;
A[16][32] =    0.9516;
A[16][33] =   -0.9765;
A[16][34] =   -0.4187;
A[17][0] =   -0.1940;
A[17][1] =   -0.5041;
A[17][2] =    0.0268;
A[17][3] =   -0.7855;
A[17][4] =    0.2640;
A[17][5] =   -1.0106;
A[17][6] =   -0.5214;
A[17][7] =    1.5623;
A[17][8] =    1.5452;
A[17][9] =   -1.5766;
A[17][10] =   -0.4169;
A[17][11] =   -1.2123;
A[17][12] =    2.1315;
A[17][13] =    1.1865;
A[17][14] =    1.7232;
A[17][15] =   -1.0848;
A[17][16] =   -2.2144;
A[17][17] =   -2.2571;
A[17][18] =    1.3530;
A[17][19] =   -0.6657;
A[17][20] =    1.0626;
A[17][21] =   -1.8969;
A[17][22] =   -2.4239;
A[17][23] =    1.4682;
A[17][24] =   -3.5436;
A[17][25] =    4.7351;
A[17][26] =   -1.0987;
A[17][27] =    3.1609;
A[17][28] =    4.9605;
A[17][29] =   -0.5611;
A[17][30] =   -0.1094;
A[17][31] =   -0.0128;
A[17][32] =    0.5789;
A[17][33] =    0.8099;
A[17][34] =    0.8162;
A[18][0] =   -0.7489;
A[18][1] =    0.1577;
A[18][2] =   -0.5018;
A[18][3] =   -0.9086;
A[18][4] =    0.3399;
A[18][5] =    1.1645;
A[18][6] =   -1.3530;
A[18][7] =    0.0602;
A[18][8] =   -1.3717;
A[18][9] =   -1.2422;
A[18][10] =    0.0517;
A[18][11] =   -1.9752;
A[18][12] =   -1.2356;
A[18][13] =    2.0497;
A[18][14] =   -0.9633;
A[18][15] =    0.2835;
A[18][16] =   -1.4632;
A[18][17] =    0.5217;
A[18][18] =    2.6181;
A[18][19] =    1.7408;
A[18][20] =   -0.4566;
A[18][21] =    1.9231;
A[18][22] =    1.9875;
A[18][23] =   -0.9080;
A[18][24] =   -3.2547;
A[18][25] =    0.6018;
A[18][26] =    0.8553;
A[18][27] =   -1.4487;
A[18][28] =    0.2683;
A[18][29] =    0.0491;
A[18][30] =   -0.6779;
A[18][31] =   -0.4638;
A[18][32] =    0.2937;
A[18][33] =   -0.8772;
A[18][34] =    0.0631;
A[19][0] =    0.2243;
A[19][1] =    0.7779;
A[19][2] =    0.1607;
A[19][3] =    0.9961;
A[19][4] =   -0.8219;
A[19][5] =   -0.8957;
A[19][6] =    1.0451;
A[19][7] =    0.6759;
A[19][8] =   -0.5870;
A[19][9] =    0.9159;
A[19][10] =   -0.1878;
A[19][11] =   -1.1522;
A[19][12] =    2.2577;
A[19][13] =   -2.2384;
A[19][14] =   -1.6710;
A[19][15] =   -2.1685;
A[19][16] =    0.7649;
A[19][17] =    0.9063;
A[19][18] =    1.3774;
A[19][19] =    2.9742;
A[19][20] =   -1.2883;
A[19][21] =   -0.3600;
A[19][22] =    1.5053;
A[19][23] =    1.0125;
A[19][24] =   -2.7050;
A[19][25] =   -2.2267;
A[19][26] =   -0.0522;
A[19][27] =    1.2167;
A[19][28] =    0.5312;
A[19][29] =    0.6949;
A[19][30] =    0.3837;
A[19][31] =    0.0462;
A[19][32] =   -0.8741;
A[19][33] =    0.2437;
A[19][34] =    0.0653;
A[20][0] =    0.5132;
A[20][1] =    0.3341;
A[20][2] =    0.2694;
A[20][3] =   -0.5856;
A[20][4] =   -0.0007;
A[20][5] =   -0.5144;
A[20][6] =   -0.0984;
A[20][7] =   -0.6818;
A[20][8] =    0.3447;
A[20][9] =    0.4439;
A[20][10] =    0.2917;
A[20][11] =    1.0060;
A[20][12] =    0.0125;
A[20][13] =    1.0340;
A[20][14] =   -1.6661;
A[20][15] =    0.2384;
A[20][16] =    0.6562;
A[20][17] =   -1.5512;
A[20][18] =    0.3788;
A[20][19] =   -2.9550;
A[20][20] =    1.7300;
A[20][21] =    2.8258;
A[20][22] =    2.6880;
A[20][23] =    1.9550;
A[20][24] =    3.2116;
A[20][25] =    2.4605;
A[20][26] =    0.0154;
A[20][27] =   -0.4055;
A[20][28] =    1.7343;
A[20][29] =   -0.4203;
A[20][30] =    0.6644;
A[20][31] =   -0.0612;
A[20][32] =    0.6885;
A[20][33] =    0.4301;
A[20][34] =   -0.1747;
A[21][0] =   -0.3924;
A[21][1] =    0.3997;
A[21][2] =   -0.8778;
A[21][3] =    0.2793;
A[21][4] =    0.2768;
A[21][5] =    0.9368;
A[21][6] =    0.2886;
A[21][7] =   -1.0606;
A[21][8] =   -1.2506;
A[21][9] =    0.4683;
A[21][10] =   -0.8864;
A[21][11] =   -2.1721;
A[21][12] =   -1.1245;
A[21][13] =    0.2576;
A[21][14] =    0.4453;
A[21][15] =   -0.8911;
A[21][16] =   -0.5523;
A[21][17] =   -2.5847;
A[21][18] =    1.8743;
A[21][19] =   -1.4121;
A[21][20] =    0.1989;
A[21][21] =    3.1076;
A[21][22] =   -2.0499;
A[21][23] =    0.0699;
A[21][24] =   -0.9116;
A[21][25] =    1.7932;
A[21][26] =    1.3433;
A[21][27] =   -0.3523;
A[21][28] =    2.1091;
A[21][29] =    0.4101;
A[21][30] =    0.2878;
A[21][31] =    0.7132;
A[21][32] =   -0.0116;
A[21][33] =   -0.3517;
A[21][34] =    0.6793;
A[22][0] =    0.6907;
A[22][1] =   -0.7361;
A[22][2] =    0.2608;
A[22][3] =    0.0973;
A[22][4] =    1.0113;
A[22][5] =    0.5550;
A[22][6] =    0.1469;
A[22][7] =   -0.7747;
A[22][8] =   -1.3574;
A[22][9] =   -0.6400;
A[22][10] =   -1.2689;
A[22][11] =    1.1469;
A[22][12] =    1.5603;
A[22][13] =    2.0980;
A[22][14] =    0.2088;
A[22][15] =   -2.5797;
A[22][16] =   -1.3258;
A[22][17] =    2.7338;
A[22][18] =   -2.1963;
A[22][19] =    3.3671;
A[22][20] =    0.0149;
A[22][21] =    3.1113;
A[22][22] =   -1.3899;
A[22][23] =    3.6021;
A[22][24] =    3.5601;
A[22][25] =   -0.8336;
A[22][26] =    0.1294;
A[22][27] =   -0.0798;
A[22][28] =   -2.4302;
A[22][29] =    0.0733;
A[22][30] =   -0.5521;
A[22][31] =   -0.6518;
A[22][32] =   -0.8933;
A[22][33] =    0.7350;
A[22][34] =    0.6160;
A[23][0] =    0.6316;
A[23][1] =   -0.4282;
A[23][2] =    0.9251;
A[23][3] =    0.3854;
A[23][4] =    0.0660;
A[23][5] =    0.4119;
A[23][6] =   -0.4961;
A[23][7] =    1.2500;
A[23][8] =   -0.7343;
A[23][9] =    0.7735;
A[23][10] =   -0.0592;
A[23][11] =    1.4334;
A[23][12] =   -1.6013;
A[23][13] =   -1.6231;
A[23][14] =   -1.5572;
A[23][15] =    1.3593;
A[23][16] =    0.1389;
A[23][17] =    1.5852;
A[23][18] =    1.3012;
A[23][19] =   -2.5977;
A[23][20] =    1.1864;
A[23][21] =    0.6860;
A[23][22] =    2.9811;
A[23][23] =    0.7959;
A[23][24] =    0.5499;
A[23][25] =   -2.2708;
A[23][26] =   -1.4552;
A[23][27] =    0.7966;
A[23][28] =   -0.2353;
A[23][29] =    0.5024;
A[23][30] =    0.6751;
A[23][31] =   -0.1111;
A[23][32] =   -0.0720;
A[23][33] =    0.5927;
A[23][34] =   -0.9861;
A[24][0] =   -0.1078;
A[24][1] =   -0.6456;
A[24][2] =    0.0555;
A[24][3] =   -0.6556;
A[24][4] =    0.3356;
A[24][5] =    1.2391;
A[24][6] =    0.2142;
A[24][7] =   -0.1333;
A[24][8] =   -0.6752;
A[24][9] =    0.5827;
A[24][10] =   -0.8971;
A[24][11] =    1.0925;
A[24][12] =    2.2165;
A[24][13] =    0.7682;
A[24][14] =   -1.4095;
A[24][15] =    0.2217;
A[24][16] =    0.6509;
A[24][17] =    0.7634;
A[24][18] =   -2.2734;
A[24][19] =   -1.8897;
A[24][20] =   -3.2900;
A[24][21] =   -0.3892;
A[24][22] =    3.4837;
A[24][23] =    0.5831;
A[24][24] =    2.1716;
A[24][25] =    2.7295;
A[24][26] =   -0.1956;
A[24][27] =    1.1678;
A[24][28] =   -1.0749;
A[24][29] =    0.8787;
A[24][30] =   -0.2584;
A[24][31] =   -0.6977;
A[24][32] =    0.3583;
A[24][33] =   -0.2198;
A[24][34] =   -0.8065;
A[25][0] =    0.6096;
A[25][1] =    0.4784;
A[25][2] =    0.1396;
A[25][3] =   -0.5014;
A[25][4] =   -0.1128;
A[25][5] =    0.8596;
A[25][6] =    0.2162;
A[25][7] =   -0.8385;
A[25][8] =    1.4542;
A[25][9] =    0.5493;
A[25][10] =    1.4129;
A[25][11] =   -0.0353;
A[25][12] =    0.6666;
A[25][13] =   -0.6237;
A[25][14] =    1.1641;
A[25][15] =    2.1959;
A[25][16] =    1.5737;
A[25][17] =    2.0280;
A[25][18] =    2.1901;
A[25][19] =   -3.0979;
A[25][20] =    1.7308;
A[25][21] =    2.1167;
A[25][22] =    2.7880;
A[25][23] =   -3.3060;
A[25][24] =   -0.8207;
A[25][25] =   -0.4123;
A[25][26] =   -0.0021;
A[25][27] =   -3.1939;
A[25][28] =    2.4926;
A[25][29] =   -0.2666;
A[25][30] =   -0.3884;
A[25][31] =   -0.0226;
A[25][32] =    0.1965;
A[25][33] =    0.3746;
A[25][34] =   -0.7825;
A[26][0] =   -0.7926;
A[26][1] =   -0.6980;
A[26][2] =   -0.8407;
A[26][3] =   -0.3128;
A[26][4] =    0.1535;
A[26][5] =    0.7757;
A[26][6] =    0.7989;
A[26][7] =   -0.3392;
A[26][8] =    0.8498;
A[26][9] =    0.5660;
A[26][10] =    0.9250;
A[26][11] =   -2.0735;
A[26][12] =   -1.4779;
A[26][13] =   -2.1917;
A[26][14] =    0.1128;
A[26][15] =    1.8219;
A[26][16] =   -1.5502;
A[26][17] =    2.4105;
A[26][18] =    0.4444;
A[26][19] =   -0.6723;
A[26][20] =   -2.0509;
A[26][21] =    3.1960;
A[26][22] =   -2.7684;
A[26][23] =   -0.4513;
A[26][24] =   -2.1998;
A[26][25] =    4.5239;
A[26][26] =    0.5299;
A[26][27] =    1.3712;
A[26][28] =   -1.5040;
A[26][29] =   -0.7233;
A[26][30] =    0.8963;
A[26][31] =   -0.6488;
A[26][32] =   -0.1384;
A[26][33] =    0.2678;
A[26][34] =    0.6551;
A[27][0] =    0.6595;
A[27][1] =    0.3556;
A[27][2] =    0.4432;
A[27][3] =   -0.5525;
A[27][4] =    0.9381;
A[27][5] =   -1.2356;
A[27][6] =    1.0919;
A[27][7] =   -1.2749;
A[27][8] =   -0.5371;
A[27][9] =    0.3771;
A[27][10] =    1.6789;
A[27][11] =    1.7848;
A[27][12] =   -1.4829;
A[27][13] =    1.5350;
A[27][14] =    1.5138;
A[27][15] =   -2.1279;
A[27][16] =    0.0262;
A[27][17] =   -0.4622;
A[27][18] =    1.4618;
A[27][19] =    0.2854;
A[27][20] =    1.3892;
A[27][21] =   -1.3732;
A[27][22] =    0.9908;
A[27][23] =    0.1325;
A[27][24] =    0.4312;
A[27][25] =    4.3023;
A[27][26] =    0.8425;
A[27][27] =    3.2562;
A[27][28] =   -0.0316;
A[27][29] =    0.3852;
A[27][30] =   -0.5980;
A[27][31] =   -0.5425;
A[27][32] =   -0.8727;
A[27][33] =    0.0652;
A[27][34] =   -0.0790;
 double b[m];
b[0] =    7.1404;
b[1] =    3.5765;
b[2] =    8.5294;
b[3] =    2.2971;
b[4] =    9.5530;
b[5] =    4.7524;
b[6] =   -0.3670;
b[7] =    0.2387;
b[8] =   -0.3972;
b[9] =    5.9552;
b[10] =   -0.7284;
b[11] =   -0.7032;
b[12] =    1.8595;
b[13] =   -2.2928;
b[14] =    0.7651;
b[15] =  -10.8349;
b[16] =    7.6917;
b[17] =    4.2698;
b[18] =    0.8091;
b[19] =    3.7058;
b[20] =    0.8622;
b[21] =    4.0308;
b[22] =   -2.4799;
b[23] =    0.5537;
b[24] =    4.4295;
b[25] =   -1.1005;
b[26] =    1.7014;
b[27] =   -6.1126;
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