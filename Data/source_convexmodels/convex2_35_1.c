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
A[0][0] =   -0.2442;
A[0][1] =    0.3813;
A[0][2] =    0.6165;
A[0][3] =   -0.3972;
A[0][4] =   -0.3515;
A[0][5] =   -0.7697;
A[0][6] =   -0.1791;
A[0][7] =    0.2594;
A[0][8] =    0.9841;
A[0][9] =   -0.1457;
A[0][10] =   -0.3315;
A[0][11] =    0.5492;
A[0][12] =   -0.2089;
A[0][13] =   -1.6557;
A[0][14] =   -0.3766;
A[0][15] =   -0.1167;
A[0][16] =    0.6011;
A[0][17] =    0.4502;
A[0][18] =    0.1924;
A[0][19] =   -0.5180;
A[0][20] =   -0.3507;
A[0][21] =   -1.0870;
A[0][22] =    1.6890;
A[0][23] =    0.1676;
A[0][24] =    0.0199;
A[0][25] =   -0.2160;
A[0][26] =    0.0141;
A[0][27] =   -1.5580;
A[0][28] =    3.3076;
A[0][29] =    0.0687;
A[0][30] =    0.3995;
A[0][31] =    1.3872;
A[0][32] =    0.2612;
A[0][33] =    0.0436;
A[0][34] =    0.9418;
A[1][0] =   -0.4407;
A[1][1] =    0.2015;
A[1][2] =    0.4872;
A[1][3] =    0.0396;
A[1][4] =   -0.8466;
A[1][5] =   -0.0068;
A[1][6] =    0.9903;
A[1][7] =    0.4800;
A[1][8] =   -0.5293;
A[1][9] =   -1.1773;
A[1][10] =   -1.0055;
A[1][11] =   -1.4504;
A[1][12] =   -0.6737;
A[1][13] =    0.8946;
A[1][14] =   -0.5572;
A[1][15] =    0.0282;
A[1][16] =    0.9511;
A[1][17] =    0.6933;
A[1][18] =   -0.1779;
A[1][19] =    1.5167;
A[1][20] =    1.7290;
A[1][21] =    0.9366;
A[1][22] =   -0.3459;
A[1][23] =    1.0374;
A[1][24] =    2.4609;
A[1][25] =   -0.7646;
A[1][26] =   -0.0593;
A[1][27] =    0.0476;
A[1][28] =    1.7638;
A[1][29] =    0.0451;
A[1][30] =    0.9850;
A[1][31] =   -1.7041;
A[1][32] =    0.3472;
A[1][33] =   -0.4905;
A[1][34] =    0.5438;
A[2][0] =    0.4870;
A[2][1] =   -0.2166;
A[2][2] =   -0.3779;
A[2][3] =    0.8672;
A[2][4] =    0.8257;
A[2][5] =   -0.5126;
A[2][6] =    0.2314;
A[2][7] =    0.1244;
A[2][8] =   -0.4981;
A[2][9] =    1.3450;
A[2][10] =    0.7524;
A[2][11] =    0.7986;
A[2][12] =    1.1537;
A[2][13] =    0.7584;
A[2][14] =    0.8463;
A[2][15] =    1.0150;
A[2][16] =    1.8184;
A[2][17] =    1.7591;
A[2][18] =   -1.5776;
A[2][19] =   -1.3408;
A[2][20] =   -0.0914;
A[2][21] =   -0.8187;
A[2][22] =   -0.9917;
A[2][23] =   -2.4783;
A[2][24] =    2.3693;
A[2][25] =    0.6526;
A[2][26] =   -0.0555;
A[2][27] =    1.3721;
A[2][28] =    0.5735;
A[2][29] =   -0.1721;
A[2][30] =    1.0532;
A[2][31] =    0.1035;
A[2][32] =    0.8350;
A[2][33] =    0.3474;
A[2][34] =    0.2117;
A[3][0] =    0.4941;
A[3][1] =   -0.3492;
A[3][2] =    0.3595;
A[3][3] =    0.3368;
A[3][4] =   -0.7849;
A[3][5] =   -0.7353;
A[3][6] =    0.9150;
A[3][7] =   -0.1097;
A[3][8] =    0.1615;
A[3][9] =   -0.8571;
A[3][10] =   -1.3163;
A[3][11] =   -0.9991;
A[3][12] =   -1.3555;
A[3][13] =    1.2067;
A[3][14] =   -0.4509;
A[3][15] =   -1.2567;
A[3][16] =   -0.7728;
A[3][17] =   -0.2142;
A[3][18] =   -0.0499;
A[3][19] =    1.8172;
A[3][20] =    1.9282;
A[3][21] =    1.2064;
A[3][22] =    0.3815;
A[3][23] =   -1.5348;
A[3][24] =   -2.4452;
A[3][25] =   -1.0629;
A[3][26] =    0.0150;
A[3][27] =    0.5958;
A[3][28] =    3.1978;
A[3][29] =   -0.0155;
A[3][30] =    1.2587;
A[3][31] =   -0.6755;
A[3][32] =    0.8152;
A[3][33] =    0.7182;
A[3][34] =    0.6848;
A[4][0] =   -0.5416;
A[4][1] =   -0.7547;
A[4][2] =    0.7401;
A[4][3] =   -0.1801;
A[4][4] =    0.8297;
A[4][5] =   -0.0940;
A[4][6] =   -0.9171;
A[4][7] =   -0.3357;
A[4][8] =    0.6665;
A[4][9] =   -1.1671;
A[4][10] =   -1.2241;
A[4][11] =    0.1843;
A[4][12] =    0.3487;
A[4][13] =   -0.8513;
A[4][14] =   -0.2643;
A[4][15] =   -1.0160;
A[4][16] =    1.0324;
A[4][17] =    0.5564;
A[4][18] =    0.3110;
A[4][19] =   -0.1850;
A[4][20] =   -1.1780;
A[4][21] =    1.4239;
A[4][22] =   -0.7675;
A[4][23] =    1.1722;
A[4][24] =    2.2713;
A[4][25] =    0.7399;
A[4][26] =    0.0693;
A[4][27] =   -1.6123;
A[4][28] =   -2.0154;
A[4][29] =   -0.0592;
A[4][30] =    0.8692;
A[4][31] =   -0.6231;
A[4][32] =   -0.8724;
A[4][33] =   -0.0549;
A[4][34] =    0.3572;
A[5][0] =    0.3572;
A[5][1] =    0.3405;
A[5][2] =    0.7971;
A[5][3] =   -0.7227;
A[5][4] =   -0.4936;
A[5][5] =   -0.8843;
A[5][6] =   -0.6358;
A[5][7] =   -0.9557;
A[5][8] =    1.0127;
A[5][9] =    0.1224;
A[5][10] =    1.3177;
A[5][11] =    0.0990;
A[5][12] =   -0.4873;
A[5][13] =    0.3912;
A[5][14] =    0.2459;
A[5][15] =   -1.0670;
A[5][16] =   -0.8713;
A[5][17] =   -1.2041;
A[5][18] =   -0.7134;
A[5][19] =   -1.5374;
A[5][20] =    0.1258;
A[5][21] =   -2.1492;
A[5][22] =    1.7649;
A[5][23] =    2.4612;
A[5][24] =    0.0206;
A[5][25] =    1.5565;
A[5][26] =   -0.1131;
A[5][27] =    1.5898;
A[5][28] =   -2.0778;
A[5][29] =    0.0418;
A[5][30] =   -1.1924;
A[5][31] =    0.9116;
A[5][32] =    0.2745;
A[5][33] =    0.8251;
A[5][34] =    0.8470;
A[6][0] =   -0.4948;
A[6][1] =    0.6667;
A[6][2] =   -0.4157;
A[6][3] =   -0.1556;
A[6][4] =   -0.7648;
A[6][5] =   -0.5031;
A[6][6] =   -0.7734;
A[6][7] =   -0.1216;
A[6][8] =   -0.3337;
A[6][9] =   -0.9603;
A[6][10] =    0.2506;
A[6][11] =   -0.6033;
A[6][12] =   -0.7210;
A[6][13] =   -1.5368;
A[6][14] =    0.3033;
A[6][15] =   -1.7155;
A[6][16] =    1.6014;
A[6][17] =    0.2519;
A[6][18] =   -0.3693;
A[6][19] =   -1.0564;
A[6][20] =   -1.2049;
A[6][21] =   -2.3636;
A[6][22] =   -1.0551;
A[6][23] =   -1.5043;
A[6][24] =   -1.6901;
A[6][25] =    2.1644;
A[6][26] =    0.0560;
A[6][27] =   -1.7226;
A[6][28] =    3.4303;
A[6][29] =    0.0445;
A[6][30] =   -0.5633;
A[6][31] =    1.3147;
A[6][32] =   -0.0378;
A[6][33] =   -0.8750;
A[6][34] =    0.9495;
A[7][0] =    0.1468;
A[7][1] =    0.3835;
A[7][2] =   -0.6193;
A[7][3] =    0.0872;
A[7][4] =    0.4216;
A[7][5] =   -0.0162;
A[7][6] =   -0.6239;
A[7][7] =   -0.6066;
A[7][8] =   -0.4836;
A[7][9] =   -1.4086;
A[7][10] =    1.0723;
A[7][11] =    0.6159;
A[7][12] =    0.2195;
A[7][13] =   -1.3183;
A[7][14] =   -1.6222;
A[7][15] =    1.2322;
A[7][16] =    0.6797;
A[7][17] =   -1.6545;
A[7][18] =   -1.4544;
A[7][19] =    0.0946;
A[7][20] =    1.5981;
A[7][21] =    1.0987;
A[7][22] =   -0.2919;
A[7][23] =   -1.3267;
A[7][24] =    0.6511;
A[7][25] =    0.9975;
A[7][26] =   -0.0234;
A[7][27] =    1.8350;
A[7][28] =    0.1162;
A[7][29] =   -0.0777;
A[7][30] =   -1.1120;
A[7][31] =    0.8198;
A[7][32] =   -0.4250;
A[7][33] =   -0.0249;
A[7][34] =   -0.8838;
A[8][0] =    0.4013;
A[8][1] =    0.1451;
A[8][2] =   -0.2319;
A[8][3] =   -0.4912;
A[8][4] =    0.4686;
A[8][5] =   -0.2943;
A[8][6] =    0.7507;
A[8][7] =    0.4885;
A[8][8] =    0.2587;
A[8][9] =    0.1307;
A[8][10] =   -0.3585;
A[8][11] =    1.3916;
A[8][12] =   -1.1514;
A[8][13] =   -0.5170;
A[8][14] =   -1.4074;
A[8][15] =   -1.6875;
A[8][16] =   -0.8306;
A[8][17] =   -0.8668;
A[8][18] =    1.5063;
A[8][19] =    1.2442;
A[8][20] =    0.1215;
A[8][21] =    0.7917;
A[8][22] =    1.3169;
A[8][23] =   -0.8186;
A[8][24] =    2.0564;
A[8][25] =   -1.5931;
A[8][26] =    0.0496;
A[8][27] =    0.7465;
A[8][28] =   -1.8503;
A[8][29] =    0.1346;
A[8][30] =    0.5716;
A[8][31] =   -1.3490;
A[8][32] =    0.4431;
A[8][33] =    0.8301;
A[8][34] =   -0.9472;
A[9][0] =   -0.3952;
A[9][1] =    0.7672;
A[9][2] =   -0.4967;
A[9][3] =   -0.9032;
A[9][4] =    0.7291;
A[9][5] =    0.1714;
A[9][6] =    0.2290;
A[9][7] =    0.5278;
A[9][8] =   -0.0269;
A[9][9] =    0.3717;
A[9][10] =   -0.4182;
A[9][11] =    0.1378;
A[9][12] =   -1.1426;
A[9][13] =   -0.0955;
A[9][14] =    0.0035;
A[9][15] =   -1.6921;
A[9][16] =   -0.7033;
A[9][17] =    0.1837;
A[9][18] =    0.7910;
A[9][19] =   -0.5144;
A[9][20] =   -0.7092;
A[9][21] =    0.4109;
A[9][22] =   -0.5783;
A[9][23] =   -1.7928;
A[9][24] =   -0.4390;
A[9][25] =   -1.6915;
A[9][26] =    0.0333;
A[9][27] =    1.3536;
A[9][28] =   -0.8041;
A[9][29] =   -0.1299;
A[9][30] =   -0.1332;
A[9][31] =    1.8816;
A[9][32] =    0.0098;
A[9][33] =   -0.0204;
A[9][34] =    0.4195;
A[10][0] =   -0.6416;
A[10][1] =   -0.2362;
A[10][2] =   -0.8488;
A[10][3] =    0.0752;
A[10][4] =    0.1899;
A[10][5] =   -0.5058;
A[10][6] =    0.5125;
A[10][7] =   -0.5195;
A[10][8] =   -0.6368;
A[10][9] =   -0.3040;
A[10][10] =   -0.6228;
A[10][11] =   -0.7416;
A[10][12] =    1.3618;
A[10][13] =   -1.4315;
A[10][14] =    1.4397;
A[10][15] =    1.1726;
A[10][16] =    0.8637;
A[10][17] =   -1.3320;
A[10][18] =   -1.4154;
A[10][19] =   -2.0811;
A[10][20] =   -1.1468;
A[10][21] =    2.0181;
A[10][22] =   -1.2912;
A[10][23] =   -0.8120;
A[10][24] =    2.3583;
A[10][25] =   -0.4986;
A[10][26] =    0.0349;
A[10][27] =   -1.2402;
A[10][28] =   -2.0295;
A[10][29] =   -0.1963;
A[10][30] =   -0.4236;
A[10][31] =    0.2360;
A[10][32] =    0.3546;
A[10][33] =   -0.0314;
A[10][34] =    0.2799;
A[11][0] =    0.4160;
A[11][1] =   -0.6935;
A[11][2] =   -0.5714;
A[11][3] =    0.6946;
A[11][4] =   -0.0338;
A[11][5] =    0.7369;
A[11][6] =    0.0893;
A[11][7] =   -1.0781;
A[11][8] =    0.9279;
A[11][9] =    0.1920;
A[11][10] =   -1.2412;
A[11][11] =   -1.4742;
A[11][12] =   -0.5145;
A[11][13] =    0.6197;
A[11][14] =   -0.8688;
A[11][15] =    0.9300;
A[11][16] =   -1.2582;
A[11][17] =   -0.9347;
A[11][18] =    0.5000;
A[11][19] =    2.2240;
A[11][20] =   -1.5638;
A[11][21] =   -1.6021;
A[11][22] =   -0.5244;
A[11][23] =    0.3285;
A[11][24] =    0.1663;
A[11][25] =    1.4892;
A[11][26] =    0.1084;
A[11][27] =    0.7124;
A[11][28] =   -1.4248;
A[11][29] =   -0.1124;
A[11][30] =    0.8601;
A[11][31] =   -0.1099;
A[11][32] =    0.6048;
A[11][33] =   -0.4517;
A[11][34] =   -0.0641;
A[12][0] =   -0.6317;
A[12][1] =   -0.0222;
A[12][2] =   -0.4590;
A[12][3] =   -0.2223;
A[12][4] =   -0.3307;
A[12][5] =    0.1275;
A[12][6] =   -0.6034;
A[12][7] =    0.6209;
A[12][8] =    0.0781;
A[12][9] =   -1.1854;
A[12][10] =    0.4361;
A[12][11] =    0.7924;
A[12][12] =   -0.9465;
A[12][13] =    0.0861;
A[12][14] =    0.5264;
A[12][15] =    1.2301;
A[12][16] =   -0.2378;
A[12][17] =   -1.1161;
A[12][18] =    0.9231;
A[12][19] =    0.1052;
A[12][20] =    0.4686;
A[12][21] =    2.2730;
A[12][22] =   -1.5036;
A[12][23] =   -0.1937;
A[12][24] =    1.8870;
A[12][25] =    1.5992;
A[12][26] =    0.0037;
A[12][27] =    1.3031;
A[12][28] =    1.3290;
A[12][29] =    0.0139;
A[12][30] =   -1.2471;
A[12][31] =   -0.4132;
A[12][32] =    0.3667;
A[12][33] =   -0.8178;
A[12][34] =    0.1143;
A[13][0] =    0.3304;
A[13][1] =   -0.5674;
A[13][2] =    0.8907;
A[13][3] =    0.5970;
A[13][4] =   -0.0678;
A[13][5] =    0.6240;
A[13][6] =    0.5183;
A[13][7] =    1.0584;
A[13][8] =    0.5772;
A[13][9] =    0.0026;
A[13][10] =    1.2976;
A[13][11] =   -1.5149;
A[13][12] =   -1.3411;
A[13][13] =    0.0540;
A[13][14] =   -1.4967;
A[13][15] =    1.4928;
A[13][16] =   -0.5398;
A[13][17] =    0.2038;
A[13][18] =   -2.1223;
A[13][19] =    1.5700;
A[13][20] =    1.4685;
A[13][21] =   -1.3504;
A[13][22] =   -0.2931;
A[13][23] =    1.9447;
A[13][24] =    1.4273;
A[13][25] =    0.5555;
A[13][26] =   -0.1151;
A[13][27] =   -1.0115;
A[13][28] =    0.8666;
A[13][29] =   -0.0625;
A[13][30] =   -0.8781;
A[13][31] =   -1.6402;
A[13][32] =   -0.7690;
A[13][33] =    0.3319;
A[13][34] =   -0.8113;
A[14][0] =   -0.2203;
A[14][1] =   -0.4117;
A[14][2] =    0.2776;
A[14][3] =   -0.6350;
A[14][4] =   -0.0785;
A[14][5] =   -0.6274;
A[14][6] =   -0.6517;
A[14][7] =    0.2358;
A[14][8] =   -0.2371;
A[14][9] =   -1.1774;
A[14][10] =    0.1330;
A[14][11] =   -0.7219;
A[14][12] =    1.2890;
A[14][13] =    1.4993;
A[14][14] =   -1.6649;
A[14][15] =   -1.6701;
A[14][16] =   -1.1415;
A[14][17] =    0.2381;
A[14][18] =    0.0939;
A[14][19] =    1.7901;
A[14][20] =    1.6476;
A[14][21] =   -2.2238;
A[14][22] =    0.5853;
A[14][23] =   -1.9003;
A[14][24] =    0.8161;
A[14][25] =   -1.8961;
A[14][26] =    0.0776;
A[14][27] =   -1.4027;
A[14][28] =   -2.8018;
A[14][29] =   -0.1327;
A[14][30] =   -0.6210;
A[14][31] =   -0.8824;
A[14][32] =    0.9153;
A[14][33] =   -0.7489;
A[14][34] =    0.6398;
A[15][0] =   -0.1150;
A[15][1] =   -0.8453;
A[15][2] =   -0.6847;
A[15][3] =    0.1834;
A[15][4] =    0.1654;
A[15][5] =   -0.3514;
A[15][6] =    0.7190;
A[15][7] =   -0.3055;
A[15][8] =    0.5532;
A[15][9] =   -0.7054;
A[15][10] =    0.5091;
A[15][11] =    0.1541;
A[15][12] =    1.4303;
A[15][13] =   -1.2538;
A[15][14] =    0.0692;
A[15][15] =   -1.0682;
A[15][16] =    0.1337;
A[15][17] =    0.5030;
A[15][18] =   -1.0838;
A[15][19] =    0.8478;
A[15][20] =    1.6281;
A[15][21] =   -1.3053;
A[15][22] =   -1.3256;
A[15][23] =   -1.9560;
A[15][24] =    2.3521;
A[15][25] =   -1.7425;
A[15][26] =    0.0299;
A[15][27] =   -1.6783;
A[15][28] =   -1.5121;
A[15][29] =    0.0444;
A[15][30] =    0.7128;
A[15][31] =   -0.5249;
A[15][32] =   -0.3289;
A[15][33] =   -0.7824;
A[15][34] =   -0.2505;
A[16][0] =   -0.4198;
A[16][1] =    0.4466;
A[16][2] =    0.5819;
A[16][3] =    0.6187;
A[16][4] =    0.4327;
A[16][5] =    0.1446;
A[16][6] =    0.6593;
A[16][7] =    1.0091;
A[16][8] =    0.5078;
A[16][9] =   -0.9301;
A[16][10] =    1.2734;
A[16][11] =   -1.1127;
A[16][12] =   -0.4404;
A[16][13] =    0.5133;
A[16][14] =    0.5244;
A[16][15] =    0.6685;
A[16][16] =    1.3239;
A[16][17] =   -2.0843;
A[16][18] =    0.0370;
A[16][19] =    0.0815;
A[16][20] =   -1.2513;
A[16][21] =    1.0006;
A[16][22] =    1.8362;
A[16][23] =   -0.5369;
A[16][24] =    1.0880;
A[16][25] =    2.0032;
A[16][26] =    0.0256;
A[16][27] =    0.5217;
A[16][28] =    0.7163;
A[16][29] =   -0.0854;
A[16][30] =   -0.2628;
A[16][31] =    1.1727;
A[16][32] =   -0.3634;
A[16][33] =   -0.1142;
A[16][34] =    0.8245;
A[17][0] =    0.6130;
A[17][1] =    0.4475;
A[17][2] =    0.4147;
A[17][3] =   -0.3249;
A[17][4] =    0.9213;
A[17][5] =   -0.7147;
A[17][6] =    0.4953;
A[17][7] =    0.8111;
A[17][8] =   -0.5049;
A[17][9] =   -1.3292;
A[17][10] =    0.2236;
A[17][11] =   -0.1661;
A[17][12] =   -1.2643;
A[17][13] =    1.1820;
A[17][14] =   -0.3643;
A[17][15] =    1.6438;
A[17][16] =   -0.1084;
A[17][17] =   -1.3032;
A[17][18] =    2.1294;
A[17][19] =   -0.2652;
A[17][20] =    0.0391;
A[17][21] =   -0.7098;
A[17][22] =    1.1440;
A[17][23] =    0.9772;
A[17][24] =   -1.4886;
A[17][25] =    0.0854;
A[17][26] =    0.0487;
A[17][27] =   -1.5773;
A[17][28] =    2.6042;
A[17][29] =   -0.0785;
A[17][30] =   -0.5116;
A[17][31] =   -1.9264;
A[17][32] =    0.3367;
A[17][33] =    0.1062;
A[17][34] =    0.6788;
A[18][0] =   -0.4997;
A[18][1] =   -0.8217;
A[18][2] =    0.3841;
A[18][3] =   -0.1720;
A[18][4] =    0.6050;
A[18][5] =    0.3180;
A[18][6] =    0.4399;
A[18][7] =   -0.1410;
A[18][8] =   -1.0451;
A[18][9] =    1.0709;
A[18][10] =    1.2365;
A[18][11] =   -1.1025;
A[18][12] =    1.2319;
A[18][13] =   -0.5246;
A[18][14] =   -0.4109;
A[18][15] =    0.2592;
A[18][16] =    0.5274;
A[18][17] =   -0.0991;
A[18][18] =    1.4319;
A[18][19] =   -0.5475;
A[18][20] =    0.7997;
A[18][21] =    1.0089;
A[18][22] =   -0.8901;
A[18][23] =    1.9297;
A[18][24] =    1.1145;
A[18][25] =   -0.1730;
A[18][26] =    0.0083;
A[18][27] =    1.0123;
A[18][28] =    2.2310;
A[18][29] =    0.0003;
A[18][30] =   -0.1730;
A[18][31] =   -1.0667;
A[18][32] =   -0.7064;
A[18][33] =    0.9251;
A[18][34] =    0.1858;
A[19][0] =   -0.0420;
A[19][1] =   -0.6383;
A[19][2] =    0.1025;
A[19][3] =   -0.1562;
A[19][4] =   -0.4068;
A[19][5] =   -0.6034;
A[19][6] =    0.0458;
A[19][7] =   -0.7535;
A[19][8] =    0.2969;
A[19][9] =   -1.2580;
A[19][10] =    1.2806;
A[19][11] =    1.1563;
A[19][12] =   -0.3642;
A[19][13] =   -1.4703;
A[19][14] =   -0.4488;
A[19][15] =    0.2148;
A[19][16] =   -1.2073;
A[19][17] =   -2.0112;
A[19][18] =    1.6143;
A[19][19] =   -2.1386;
A[19][20] =   -1.9326;
A[19][21] =    0.4271;
A[19][22] =    1.5412;
A[19][23] =   -2.2539;
A[19][24] =   -1.4353;
A[19][25] =   -2.7595;
A[19][26] =   -0.1037;
A[19][27] =    0.2136;
A[19][28] =   -0.8840;
A[19][29] =    0.0114;
A[19][30] =   -0.4215;
A[19][31] =    0.7382;
A[19][32] =   -0.4440;
A[19][33] =    0.1430;
A[19][34] =   -0.8081;
A[20][0] =    0.4817;
A[20][1] =   -0.7118;
A[20][2] =   -0.2131;
A[20][3] =    0.1428;
A[20][4] =    0.6121;
A[20][5] =   -0.6083;
A[20][6] =    0.8197;
A[20][7] =    0.3290;
A[20][8] =   -0.0788;
A[20][9] =   -0.0922;
A[20][10] =   -0.6359;
A[20][11] =    0.1543;
A[20][12] =   -1.0701;
A[20][13] =   -1.6632;
A[20][14] =    0.8713;
A[20][15] =    1.5215;
A[20][16] =    1.9825;
A[20][17] =   -0.1047;
A[20][18] =    1.7846;
A[20][19] =   -1.2979;
A[20][20] =    0.2030;
A[20][21] =   -1.8842;
A[20][22] =   -0.2034;
A[20][23] =   -1.0322;
A[20][24] =    0.2210;
A[20][25] =    1.4369;
A[20][26] =    0.0131;
A[20][27] =   -1.5180;
A[20][28] =   -0.1203;
A[20][29] =   -0.1929;
A[20][30] =   -0.1104;
A[20][31] =   -1.8283;
A[20][32] =    0.3188;
A[20][33] =   -0.8482;
A[20][34] =   -0.5234;
A[21][0] =   -0.3471;
A[21][1] =   -0.7158;
A[21][2] =   -0.2735;
A[21][3] =   -0.6790;
A[21][4] =    0.0520;
A[21][5] =    0.5860;
A[21][6] =   -0.4357;
A[21][7] =   -0.7378;
A[21][8] =    1.1283;
A[21][9] =    0.3053;
A[21][10] =    1.3200;
A[21][11] =    0.2603;
A[21][12] =    0.4441;
A[21][13] =   -1.6792;
A[21][14] =    0.3852;
A[21][15] =    0.6418;
A[21][16] =   -1.9829;
A[21][17] =   -1.7673;
A[21][18] =   -0.9021;
A[21][19] =   -1.8944;
A[21][20] =   -0.9539;
A[21][21] =    0.7071;
A[21][22] =    0.8699;
A[21][23] =    2.2536;
A[21][24] =   -0.6725;
A[21][25] =    0.0763;
A[21][26] =   -0.1023;
A[21][27] =    0.8543;
A[21][28] =    1.9635;
A[21][29] =   -0.1516;
A[21][30] =   -0.2473;
A[21][31] =    1.1300;
A[21][32] =    0.0845;
A[21][33] =   -0.2751;
A[21][34] =   -0.9534;
A[22][0] =   -0.0941;
A[22][1] =    0.1548;
A[22][2] =   -0.7429;
A[22][3] =   -0.7237;
A[22][4] =   -0.9088;
A[22][5] =   -0.8359;
A[22][6] =   -0.7741;
A[22][7] =   -0.3922;
A[22][8] =   -0.4105;
A[22][9] =   -0.4047;
A[22][10] =    0.9422;
A[22][11] =    0.0948;
A[22][12] =   -1.4449;
A[22][13] =   -0.7065;
A[22][14] =   -0.8979;
A[22][15] =   -0.5815;
A[22][16] =    0.4595;
A[22][17] =   -1.0406;
A[22][18] =    1.7099;
A[22][19] =    1.3562;
A[22][20] =   -0.5579;
A[22][21] =    1.1131;
A[22][22] =   -1.0491;
A[22][23] =    1.2962;
A[22][24] =    0.7281;
A[22][25] =   -0.3240;
A[22][26] =    0.0595;
A[22][27] =   -0.2988;
A[22][28] =   -1.9860;
A[22][29] =   -0.1433;
A[22][30] =    1.2595;
A[22][31] =    0.6352;
A[22][32] =   -0.4318;
A[22][33] =   -0.9610;
A[22][34] =   -0.6919;
A[23][0] =    0.6776;
A[23][1] =   -0.0066;
A[23][2] =   -0.5157;
A[23][3] =   -0.6517;
A[23][4] =    0.4742;
A[23][5] =    0.1573;
A[23][6] =   -0.2895;
A[23][7] =   -0.7632;
A[23][8] =    0.4824;
A[23][9] =   -0.1528;
A[23][10] =    0.0755;
A[23][11] =    1.1069;
A[23][12] =   -1.3559;
A[23][13] =   -0.9612;
A[23][14] =    1.4638;
A[23][15] =   -0.5508;
A[23][16] =    1.2928;
A[23][17] =    0.3389;
A[23][18] =   -1.1014;
A[23][19] =   -0.4486;
A[23][20] =    0.1600;
A[23][21] =    1.7500;
A[23][22] =    1.1396;
A[23][23] =   -2.2064;
A[23][24] =    2.6716;
A[23][25] =    2.6464;
A[23][26] =   -0.1098;
A[23][27] =    0.6389;
A[23][28] =   -2.4370;
A[23][29] =    0.1041;
A[23][30] =    1.2617;
A[23][31] =    1.0066;
A[23][32] =    0.9524;
A[23][33] =   -0.0722;
A[23][34] =    0.7609;
A[24][0] =    0.5863;
A[24][1] =   -0.0120;
A[24][2] =   -0.7372;
A[24][3] =   -0.1102;
A[24][4] =    0.6469;
A[24][5] =    0.3857;
A[24][6] =   -0.7600;
A[24][7] =   -0.7364;
A[24][8] =   -0.0877;
A[24][9] =    0.8426;
A[24][10] =    0.9573;
A[24][11] =    0.0221;
A[24][12] =    0.1220;
A[24][13] =   -0.6744;
A[24][14] =   -0.4066;
A[24][15] =    1.4960;
A[24][16] =   -0.3424;
A[24][17] =   -1.4567;
A[24][18] =   -1.3852;
A[24][19] =   -2.0566;
A[24][20] =   -0.3470;
A[24][21] =   -2.2989;
A[24][22] =    1.1579;
A[24][23] =   -2.3422;
A[24][24] =    1.5173;
A[24][25] =   -0.8349;
A[24][26] =    0.1084;
A[24][27] =    1.1468;
A[24][28] =   -1.7887;
A[24][29] =   -0.1163;
A[24][30] =   -0.1114;
A[24][31] =    0.0697;
A[24][32] =    0.1492;
A[24][33] =    0.9118;
A[24][34] =   -0.7621;
A[25][0] =   -0.6266;
A[25][1] =    0.2514;
A[25][2] =    0.4650;
A[25][3] =   -0.0515;
A[25][4] =   -0.5072;
A[25][5] =    0.7633;
A[25][6] =    0.2106;
A[25][7] =   -0.5388;
A[25][8] =   -0.6144;
A[25][9] =    0.8925;
A[25][10] =    0.5890;
A[25][11] =    0.7273;
A[25][12] =    1.3093;
A[25][13] =    0.3248;
A[25][14] =   -0.4356;
A[25][15] =   -0.6630;
A[25][16] =    1.5791;
A[25][17] =   -0.3748;
A[25][18] =   -1.4101;
A[25][19] =   -0.8383;
A[25][20] =   -1.2654;
A[25][21] =   -1.6798;
A[25][22] =    1.0279;
A[25][23] =    2.1211;
A[25][24] =    1.1883;
A[25][25] =   -1.6862;
A[25][26] =   -0.0298;
A[25][27] =    1.4576;
A[25][28] =    3.3722;
A[25][29] =    0.0014;
A[25][30] =   -0.3657;
A[25][31] =    1.9349;
A[25][32] =    0.4544;
A[25][33] =    0.9551;
A[25][34] =    0.7199;
A[26][0] =    0.0654;
A[26][1] =    0.5781;
A[26][2] =   -0.7711;
A[26][3] =    0.3414;
A[26][4] =   -0.0926;
A[26][5] =   -0.3977;
A[26][6] =   -0.2786;
A[26][7] =   -0.0380;
A[26][8] =   -0.0570;
A[26][9] =   -0.5604;
A[26][10] =    1.1218;
A[26][11] =    1.1004;
A[26][12] =   -1.0433;
A[26][13] =    0.2168;
A[26][14] =   -0.5859;
A[26][15] =   -1.5885;
A[26][16] =    0.0479;
A[26][17] =    1.6405;
A[26][18] =    1.6248;
A[26][19] =    1.6958;
A[26][20] =   -2.2269;
A[26][21] =    0.1213;
A[26][22] =   -1.4362;
A[26][23] =   -0.6427;
A[26][24] =    1.8772;
A[26][25] =   -0.1194;
A[26][26] =    0.1174;
A[26][27] =    0.6290;
A[26][28] =    0.2881;
A[26][29] =   -0.1057;
A[26][30] =   -0.4599;
A[26][31] =   -1.2709;
A[26][32] =    0.7586;
A[26][33] =   -0.7141;
A[26][34] =    0.7310;
A[27][0] =   -0.6835;
A[27][1] =   -0.8192;
A[27][2] =    0.1537;
A[27][3] =   -0.2572;
A[27][4] =    0.7149;
A[27][5] =    0.6799;
A[27][6] =    0.1494;
A[27][7] =   -0.1563;
A[27][8] =    0.2787;
A[27][9] =   -0.7201;
A[27][10] =    0.2028;
A[27][11] =   -1.1873;
A[27][12] =   -1.1751;
A[27][13] =   -0.9601;
A[27][14] =    1.3899;
A[27][15] =    0.2334;
A[27][16] =    1.8396;
A[27][17] =    1.4794;
A[27][18] =   -0.2493;
A[27][19] =    1.9707;
A[27][20] =   -2.2381;
A[27][21] =   -1.3169;
A[27][22] =   -1.6256;
A[27][23] =   -2.4047;
A[27][24] =   -1.3045;
A[27][25] =    2.1692;
A[27][26] =    0.0611;
A[27][27] =    1.2301;
A[27][28] =    1.1636;
A[27][29] =    0.0644;
A[27][30] =   -0.0326;
A[27][31] =   -0.2485;
A[27][32] =    0.3242;
A[27][33] =   -0.4544;
A[27][34] =    0.7909;
 double b[m];
b[0] =    1.9275;
b[1] =    0.4238;
b[2] =    4.2573;
b[3] =   -4.9327;
b[4] =    3.7256;
b[5] =    0.2171;
b[6] =    4.8415;
b[7] =    5.0956;
b[8] =   -1.0095;
b[9] =   -1.6981;
b[10] =    6.6974;
b[11] =    6.0340;
b[12] =    2.4119;
b[13] =    2.3190;
b[14] =    0.3626;
b[15] =    4.2035;
b[16] =    7.5691;
b[17] =    1.3133;
b[18] =    0.5555;
b[19] =    3.9237;
b[20] =    7.8071;
b[21] =    0.9603;
b[22] =   -0.2385;
b[23] =    4.3275;
b[24] =    7.3475;
b[25] =   -0.1736;
b[26] =    0.9907;
b[27] =    8.8013;
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
