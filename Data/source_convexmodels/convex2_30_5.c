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
 n = 30;
 m = 24;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double A[m][n];
A[0][0] =    0.6788;
A[0][1] =    0.1490;
A[0][2] =    0.5642;
A[0][3] =   -0.3757;
A[0][4] =    1.1172;
A[0][5] =   -0.2841;
A[0][6] =    1.0502;
A[0][7] =    1.2477;
A[0][8] =   -0.2102;
A[0][9] =    1.5804;
A[0][10] =    1.2013;
A[0][11] =    2.1558;
A[0][12] =   -0.5866;
A[0][13] =    1.5858;
A[0][14] =    2.0046;
A[0][15] =    0.7865;
A[0][16] =    2.6920;
A[0][17] =   -1.5485;
A[0][18] =    2.9771;
A[0][19] =    2.8369;
A[0][20] =    1.5171;
A[0][21] =    1.6862;
A[0][22] =   -0.0092;
A[0][23] =   -0.2107;
A[0][24] =   -0.6359;
A[0][25] =   -0.1808;
A[0][26] =    0.7918;
A[0][27] =   -0.4392;
A[0][28] =    0.3366;
A[0][29] =    0.4539;
A[1][0] =    0.5678;
A[1][1] =   -0.8138;
A[1][2] =   -0.7693;
A[1][3] =   -0.7742;
A[1][4] =   -0.7614;
A[1][5] =   -0.5599;
A[1][6] =   -1.1784;
A[1][7] =   -0.3734;
A[1][8] =    1.3611;
A[1][9] =    0.1404;
A[1][10] =    1.1400;
A[1][11] =   -0.9782;
A[1][12] =    1.0389;
A[1][13] =   -2.2168;
A[1][14] =   -1.3590;
A[1][15] =    1.0990;
A[1][16] =   -2.5358;
A[1][17] =   -1.6980;
A[1][18] =   -2.5556;
A[1][19] =    1.2120;
A[1][20] =   -0.0552;
A[1][21] =    2.6457;
A[1][22] =   -1.5709;
A[1][23] =   -0.0271;
A[1][24] =   -0.6596;
A[1][25] =    0.6897;
A[1][26] =    0.8672;
A[1][27] =   -0.7645;
A[1][28] =   -0.2398;
A[1][29] =   -0.7085;
A[2][0] =    0.1802;
A[2][1] =   -0.0237;
A[2][2] =   -1.1005;
A[2][3] =    1.0300;
A[2][4] =   -1.1055;
A[2][5] =    0.8254;
A[2][6] =   -0.4719;
A[2][7] =    1.6864;
A[2][8] =   -1.0649;
A[2][9] =    1.0118;
A[2][10] =    0.6662;
A[2][11] =   -1.2128;
A[2][12] =   -1.8083;
A[2][13] =   -0.0682;
A[2][14] =    1.0371;
A[2][15] =    2.4160;
A[2][16] =   -1.6638;
A[2][17] =   -2.3449;
A[2][18] =   -1.9870;
A[2][19] =   -0.2016;
A[2][20] =    0.4178;
A[2][21] =   -1.2337;
A[2][22] =    0.1038;
A[2][23] =   -0.8930;
A[2][24] =    0.0007;
A[2][25] =    0.8436;
A[2][26] =    1.2997;
A[2][27] =    0.4171;
A[2][28] =    0.0781;
A[2][29] =   -0.5845;
A[3][0] =   -0.1848;
A[3][1] =    0.3793;
A[3][2] =    0.1956;
A[3][3] =    0.0231;
A[3][4] =   -0.1312;
A[3][5] =   -0.0191;
A[3][6] =    0.9427;
A[3][7] =   -0.6326;
A[3][8] =   -1.4145;
A[3][9] =   -0.3302;
A[3][10] =   -0.4599;
A[3][11] =   -1.6204;
A[3][12] =   -2.0518;
A[3][13] =    2.5139;
A[3][14] =   -1.2201;
A[3][15] =    1.5704;
A[3][16] =    1.4574;
A[3][17] =    0.2346;
A[3][18] =    0.0854;
A[3][19] =   -1.2620;
A[3][20] =    1.7350;
A[3][21] =    2.8652;
A[3][22] =    1.0017;
A[3][23] =   -1.1295;
A[3][24] =    0.5522;
A[3][25] =    0.1620;
A[3][26] =    0.8706;
A[3][27] =    0.1527;
A[3][28] =    0.8746;
A[3][29] =   -0.0159;
A[4][0] =    0.6041;
A[4][1] =    0.0606;
A[4][2] =    0.6857;
A[4][3] =    1.0502;
A[4][4] =   -0.2517;
A[4][5] =   -0.0722;
A[4][6] =   -1.0092;
A[4][7] =   -1.5701;
A[4][8] =   -0.9505;
A[4][9] =   -1.8869;
A[4][10] =   -1.1019;
A[4][11] =    1.7878;
A[4][12] =    0.3314;
A[4][13] =   -0.5618;
A[4][14] =   -0.3120;
A[4][15] =   -1.0108;
A[4][16] =    1.0067;
A[4][17] =   -1.4802;
A[4][18] =    1.6783;
A[4][19] =   -0.4917;
A[4][20] =    3.1793;
A[4][21] =    1.0514;
A[4][22] =   -0.9462;
A[4][23] =    1.2599;
A[4][24] =    0.5896;
A[4][25] =    0.6923;
A[4][26] =   -0.2937;
A[4][27] =   -0.7447;
A[4][28] =    0.7879;
A[4][29] =   -0.4161;
A[5][0] =    0.5620;
A[5][1] =    0.7972;
A[5][2] =    0.4163;
A[5][3] =   -0.0697;
A[5][4] =    1.1350;
A[5][5] =   -0.8755;
A[5][6] =    0.6668;
A[5][7] =   -0.5267;
A[5][8] =    0.3967;
A[5][9] =   -1.1889;
A[5][10] =   -1.5867;
A[5][11] =   -0.3076;
A[5][12] =    1.9198;
A[5][13] =    1.0274;
A[5][14] =   -2.1275;
A[5][15] =    0.6000;
A[5][16] =    0.3941;
A[5][17] =    2.0144;
A[5][18] =   -0.8493;
A[5][19] =   -0.6441;
A[5][20] =   -0.7392;
A[5][21] =   -0.5853;
A[5][22] =    0.4319;
A[5][23] =   -1.0609;
A[5][24] =   -0.2072;
A[5][25] =   -0.2904;
A[5][26] =   -2.1012;
A[5][27] =   -0.6702;
A[5][28] =   -0.5505;
A[5][29] =    0.3252;
A[6][0] =   -0.6317;
A[6][1] =    0.0025;
A[6][2] =   -0.4628;
A[6][3] =    1.0021;
A[6][4] =    0.4088;
A[6][5] =   -0.9864;
A[6][6] =   -0.0729;
A[6][7] =    1.1277;
A[6][8] =    0.3440;
A[6][9] =   -1.0841;
A[6][10] =   -1.2218;
A[6][11] =    0.2719;
A[6][12] =    0.5389;
A[6][13] =   -1.4527;
A[6][14] =   -1.0586;
A[6][15] =   -2.0161;
A[6][16] =   -2.0712;
A[6][17] =   -2.4109;
A[6][18] =   -1.2428;
A[6][19] =   -2.4604;
A[6][20] =    1.0310;
A[6][21] =    2.3366;
A[6][22] =    1.6059;
A[6][23] =   -0.4734;
A[6][24] =    0.0560;
A[6][25] =    0.7122;
A[6][26] =    4.1145;
A[6][27] =    0.4439;
A[6][28] =   -0.8247;
A[6][29] =   -0.7964;
A[7][0] =   -0.5809;
A[7][1] =   -0.7379;
A[7][2] =    0.0804;
A[7][3] =    0.6752;
A[7][4] =    0.5556;
A[7][5] =   -0.4863;
A[7][6] =   -0.5266;
A[7][7] =    1.0997;
A[7][8] =    0.1257;
A[7][9] =    0.2306;
A[7][10] =    0.5139;
A[7][11] =   -2.1613;
A[7][12] =    1.4869;
A[7][13] =   -0.0028;
A[7][14] =    2.4451;
A[7][15] =   -2.7767;
A[7][16] =    2.7641;
A[7][17] =    1.7607;
A[7][18] =   -2.0925;
A[7][19] =   -0.1352;
A[7][20] =   -3.0436;
A[7][21] =    3.4699;
A[7][22] =    1.8743;
A[7][23] =    0.8714;
A[7][24] =   -0.1990;
A[7][25] =    0.7924;
A[7][26] =    3.9346;
A[7][27] =   -0.9444;
A[7][28] =   -0.5691;
A[7][29] =    0.5398;
A[8][0] =   -0.3686;
A[8][1] =   -0.6383;
A[8][2] =   -0.7457;
A[8][3] =   -0.0132;
A[8][4] =   -0.9423;
A[8][5] =    1.1931;
A[8][6] =    0.2729;
A[8][7] =   -0.5886;
A[8][8] =    0.5458;
A[8][9] =   -1.6767;
A[8][10] =    0.3884;
A[8][11] =   -0.1004;
A[8][12] =   -2.2634;
A[8][13] =   -2.2633;
A[8][14] =   -0.4390;
A[8][15] =   -0.4455;
A[8][16] =    2.8981;
A[8][17] =    0.3683;
A[8][18] =    2.4373;
A[8][19] =    1.7132;
A[8][20] =    2.6694;
A[8][21] =   -0.8578;
A[8][22] =    1.3054;
A[8][23] =   -1.0258;
A[8][24] =    0.0041;
A[8][25] =    0.2598;
A[8][26] =   -2.8744;
A[8][27] =   -0.4679;
A[8][28] =    0.3617;
A[8][29] =   -0.4108;
A[9][0] =   -0.2336;
A[9][1] =   -0.0468;
A[9][2] =    0.0740;
A[9][3] =   -1.1257;
A[9][4] =   -0.0122;
A[9][5] =   -1.0422;
A[9][6] =   -0.7670;
A[9][7] =    1.4233;
A[9][8] =    0.9288;
A[9][9] =   -0.3179;
A[9][10] =    1.9617;
A[9][11] =    1.4867;
A[9][12] =    0.5757;
A[9][13] =   -2.3659;
A[9][14] =    2.2246;
A[9][15] =    2.2151;
A[9][16] =   -1.1573;
A[9][17] =   -2.9986;
A[9][18] =   -1.3664;
A[9][19] =   -0.1686;
A[9][20] =    2.6375;
A[9][21] =   -0.1035;
A[9][22] =   -1.3131;
A[9][23] =    0.1264;
A[9][24] =   -0.5428;
A[9][25] =   -0.2043;
A[9][26] =   -3.7989;
A[9][27] =    0.9231;
A[9][28] =   -0.1346;
A[9][29] =   -0.6774;
A[10][0] =    0.4996;
A[10][1] =   -0.5488;
A[10][2] =   -0.6599;
A[10][3] =   -0.6013;
A[10][4] =    1.0270;
A[10][5] =   -0.5861;
A[10][6] =    0.5221;
A[10][7] =    0.4415;
A[10][8] =   -0.6970;
A[10][9] =    0.3079;
A[10][10] =    0.0624;
A[10][11] =   -1.9678;
A[10][12] =   -0.6733;
A[10][13] =   -0.0545;
A[10][14] =    0.6668;
A[10][15] =   -1.9328;
A[10][16] =   -2.6269;
A[10][17] =    0.1991;
A[10][18] =   -0.9509;
A[10][19] =   -1.1226;
A[10][20] =    3.3425;
A[10][21] =    0.8277;
A[10][22] =    0.6228;
A[10][23] =    1.0548;
A[10][24] =   -0.2947;
A[10][25] =   -0.5046;
A[10][26] =   -3.2934;
A[10][27] =   -0.9509;
A[10][28] =   -0.0294;
A[10][29] =   -0.4871;
A[11][0] =   -0.6839;
A[11][1] =   -0.0729;
A[11][2] =    0.4227;
A[11][3] =   -0.1438;
A[11][4] =    0.4155;
A[11][5] =   -0.3385;
A[11][6] =   -0.5440;
A[11][7] =   -1.4627;
A[11][8] =   -0.7390;
A[11][9] =    1.3522;
A[11][10] =    1.8547;
A[11][11] =   -0.7991;
A[11][12] =   -1.7463;
A[11][13] =   -1.0449;
A[11][14] =    1.0663;
A[11][15] =   -1.0642;
A[11][16] =    2.7158;
A[11][17] =   -1.7282;
A[11][18] =   -2.7069;
A[11][19] =   -0.6998;
A[11][20] =   -0.7891;
A[11][21] =   -0.6526;
A[11][22] =    0.9926;
A[11][23] =    1.2641;
A[11][24] =   -0.1607;
A[11][25] =    1.0057;
A[11][26] =   -3.7823;
A[11][27] =    0.5915;
A[11][28] =    0.4407;
A[11][29] =    0.9412;
A[12][0] =   -0.3406;
A[12][1] =    0.1915;
A[12][2] =   -0.2564;
A[12][3] =    0.8981;
A[12][4] =   -0.1850;
A[12][5] =    0.3537;
A[12][6] =   -0.8219;
A[12][7] =    0.6105;
A[12][8] =    0.0664;
A[12][9] =    0.6913;
A[12][10] =   -1.1313;
A[12][11] =   -0.5082;
A[12][12] =   -1.8050;
A[12][13] =   -2.2166;
A[12][14] =    0.2874;
A[12][15] =    2.5891;
A[12][16] =   -0.4207;
A[12][17] =    0.5199;
A[12][18] =   -1.7764;
A[12][19] =    1.2802;
A[12][20] =   -1.0647;
A[12][21] =    0.8825;
A[12][22] =   -2.3185;
A[12][23] =   -0.1194;
A[12][24] =    0.3764;
A[12][25] =    0.2782;
A[12][26] =    0.7073;
A[12][27] =    0.5684;
A[12][28] =   -0.3653;
A[12][29] =   -0.5918;
A[13][0] =    0.3120;
A[13][1] =   -0.6309;
A[13][2] =   -0.6199;
A[13][3] =    0.9565;
A[13][4] =   -0.4373;
A[13][5] =   -0.9961;
A[13][6] =   -0.4716;
A[13][7] =   -0.4803;
A[13][8] =    0.7909;
A[13][9] =    1.2338;
A[13][10] =    0.2486;
A[13][11] =    1.0094;
A[13][12] =   -2.2536;
A[13][13] =    1.0196;
A[13][14] =   -0.6233;
A[13][15] =    2.6284;
A[13][16] =   -1.6740;
A[13][17] =   -0.2277;
A[13][18] =    0.1424;
A[13][19] =    2.4659;
A[13][20] =    1.4139;
A[13][21] =    0.7580;
A[13][22] =    0.4834;
A[13][23] =   -0.6772;
A[13][24] =   -0.4683;
A[13][25] =    0.5155;
A[13][26] =    0.3396;
A[13][27] =   -0.8056;
A[13][28] =    0.6729;
A[13][29] =    0.8106;
A[14][0] =   -0.5803;
A[14][1] =   -0.6840;
A[14][2] =    0.1722;
A[14][3] =    0.2862;
A[14][4] =    1.1872;
A[14][5] =   -1.1730;
A[14][6] =    1.1557;
A[14][7] =   -1.1837;
A[14][8] =    1.4772;
A[14][9] =   -1.6818;
A[14][10] =   -1.7491;
A[14][11] =    2.0796;
A[14][12] =   -0.4767;
A[14][13] =   -0.9391;
A[14][14] =   -0.2436;
A[14][15] =    1.9932;
A[14][16] =   -1.1076;
A[14][17] =    1.1325;
A[14][18] =   -1.5799;
A[14][19] =    1.5828;
A[14][20] =    0.7801;
A[14][21] =    2.1641;
A[14][22] =    2.0638;
A[14][23] =   -0.9026;
A[14][24] =   -0.1418;
A[14][25] =    0.7696;
A[14][26] =   -2.5760;
A[14][27] =    0.4287;
A[14][28] =    0.6390;
A[14][29] =   -0.6773;
A[15][0] =    0.5105;
A[15][1] =   -0.7882;
A[15][2] =   -0.8464;
A[15][3] =   -0.0713;
A[15][4] =    0.6141;
A[15][5] =   -0.4647;
A[15][6] =    0.4470;
A[15][7] =   -1.5385;
A[15][8] =   -0.6326;
A[15][9] =    0.2817;
A[15][10] =    1.3645;
A[15][11] =    2.0929;
A[15][12] =    2.2985;
A[15][13] =    1.4411;
A[15][14] =    2.1462;
A[15][15] =    2.2005;
A[15][16] =    2.5580;
A[15][17] =   -1.5291;
A[15][18] =    0.5535;
A[15][19] =   -2.2606;
A[15][20] =    2.3774;
A[15][21] =    0.6440;
A[15][22] =   -1.9272;
A[15][23] =    0.5652;
A[15][24] =    0.5663;
A[15][25] =   -0.0746;
A[15][26] =    4.4067;
A[15][27] =   -0.9070;
A[15][28] =   -0.3938;
A[15][29] =   -0.0574;
A[16][0] =   -0.4701;
A[16][1] =   -0.6764;
A[16][2] =   -0.4932;
A[16][3] =    0.7503;
A[16][4] =   -0.4211;
A[16][5] =    1.1209;
A[16][6] =    0.6704;
A[16][7] =    1.0725;
A[16][8] =   -0.9873;
A[16][9] =   -1.5627;
A[16][10] =   -1.3647;
A[16][11] =    0.5630;
A[16][12] =    1.8948;
A[16][13] =   -1.6261;
A[16][14] =    0.9430;
A[16][15] =   -0.9052;
A[16][16] =    2.7838;
A[16][17] =    2.6267;
A[16][18] =    1.8825;
A[16][19] =   -2.3739;
A[16][20] =    3.0094;
A[16][21] =   -1.6102;
A[16][22] =   -1.2809;
A[16][23] =   -1.0749;
A[16][24] =   -0.3321;
A[16][25] =   -0.4569;
A[16][26] =   -5.5495;
A[16][27] =   -0.8756;
A[16][28] =    0.6281;
A[16][29] =    0.8747;
A[17][0] =   -0.0683;
A[17][1] =   -0.1407;
A[17][2] =   -0.9711;
A[17][3] =   -0.3179;
A[17][4] =   -0.2940;
A[17][5] =    1.0002;
A[17][6] =    1.2609;
A[17][7] =    1.3235;
A[17][8] =   -1.3784;
A[17][9] =    0.8545;
A[17][10] =    0.2671;
A[17][11] =    1.5648;
A[17][12] =    0.4856;
A[17][13] =   -2.4226;
A[17][14] =   -2.0166;
A[17][15] =    2.1058;
A[17][16] =   -2.4169;
A[17][17] =    1.1235;
A[17][18] =   -0.6830;
A[17][19] =   -2.5104;
A[17][20] =    3.2882;
A[17][21] =   -0.2513;
A[17][22] =    1.9679;
A[17][23] =   -0.9965;
A[17][24] =    0.3067;
A[17][25] =   -0.0526;
A[17][26] =   -1.3080;
A[17][27] =    0.5784;
A[17][28] =   -0.9476;
A[17][29] =   -0.7285;
A[18][0] =   -0.3049;
A[18][1] =   -0.0950;
A[18][2] =   -0.2537;
A[18][3] =    0.3860;
A[18][4] =    0.1652;
A[18][5] =    0.5636;
A[18][6] =   -0.1923;
A[18][7] =   -1.6669;
A[18][8] =   -0.6314;
A[18][9] =   -0.7232;
A[18][10] =    1.3182;
A[18][11] =   -0.9401;
A[18][12] =   -0.4991;
A[18][13] =    1.4080;
A[18][14] =   -0.6854;
A[18][15] =   -1.2960;
A[18][16] =    2.7412;
A[18][17] =    2.5852;
A[18][18] =    1.7012;
A[18][19] =   -1.4117;
A[18][20] =    2.7346;
A[18][21] =   -0.4790;
A[18][22] =   -1.3966;
A[18][23] =    0.5773;
A[18][24] =   -0.2556;
A[18][25] =   -0.9525;
A[18][26] =    5.2800;
A[18][27] =    0.5511;
A[18][28] =    0.6747;
A[18][29] =    0.5705;
A[19][0] =    0.0673;
A[19][1] =    0.5837;
A[19][2] =    0.8493;
A[19][3] =   -0.3512;
A[19][4] =    0.5609;
A[19][5] =    0.3843;
A[19][6] =   -0.5263;
A[19][7] =   -0.8548;
A[19][8] =   -1.6494;
A[19][9] =    0.7478;
A[19][10] =    1.8880;
A[19][11] =   -0.5456;
A[19][12] =   -2.3375;
A[19][13] =   -0.4435;
A[19][14] =    1.8319;
A[19][15] =    1.6285;
A[19][16] =    0.6623;
A[19][17] =   -0.4928;
A[19][18] =    1.2931;
A[19][19] =   -2.7924;
A[19][20] =   -3.1827;
A[19][21] =   -0.9979;
A[19][22] =   -0.9173;
A[19][23] =   -0.2827;
A[19][24] =   -0.0359;
A[19][25] =   -0.5501;
A[19][26] =    4.9456;
A[19][27] =    0.9748;
A[19][28] =   -0.0214;
A[19][29] =    0.3284;
A[20][0] =    0.6648;
A[20][1] =   -0.4252;
A[20][2] =    0.2561;
A[20][3] =   -0.8709;
A[20][4] =   -0.2174;
A[20][5] =    0.2868;
A[20][6] =    0.9457;
A[20][7] =   -1.5255;
A[20][8] =   -0.6112;
A[20][9] =    1.4931;
A[20][10] =    0.6289;
A[20][11] =   -1.2002;
A[20][12] =    1.3222;
A[20][13] =   -0.0987;
A[20][14] =   -1.1866;
A[20][15] =    0.3324;
A[20][16] =    1.7736;
A[20][17] =    1.8851;
A[20][18] =    3.0055;
A[20][19] =   -2.0767;
A[20][20] =   -2.8063;
A[20][21] =    2.9174;
A[20][22] =    0.8368;
A[20][23] =   -0.7168;
A[20][24] =    0.2096;
A[20][25] =    0.0818;
A[20][26] =   -3.6272;
A[20][27] =    0.8903;
A[20][28] =   -0.7946;
A[20][29] =    0.4623;
A[21][0] =   -0.4953;
A[21][1] =    0.5626;
A[21][2] =   -0.3055;
A[21][3] =   -0.0777;
A[21][4] =    0.2744;
A[21][5] =   -0.7068;
A[21][6] =   -0.2745;
A[21][7] =   -0.5063;
A[21][8] =   -1.4051;
A[21][9] =   -1.9699;
A[21][10] =   -1.2671;
A[21][11] =    2.1535;
A[21][12] =    0.9456;
A[21][13] =   -1.2895;
A[21][14] =   -0.3274;
A[21][15] =   -2.4597;
A[21][16] =    2.5828;
A[21][17] =   -0.3600;
A[21][18] =   -2.8849;
A[21][19] =    2.9118;
A[21][20] =   -0.9486;
A[21][21] =   -1.0850;
A[21][22] =   -1.3295;
A[21][23] =   -0.5878;
A[21][24] =    0.2429;
A[21][25] =   -0.2035;
A[21][26] =   -3.5075;
A[21][27] =    0.4658;
A[21][28] =    0.1578;
A[21][29] =   -0.6485;
A[22][0] =    0.5194;
A[22][1] =   -0.2051;
A[22][2] =    0.3328;
A[22][3] =   -1.0646;
A[22][4] =    1.1394;
A[22][5] =    0.5127;
A[22][6] =    0.6250;
A[22][7] =   -0.4312;
A[22][8] =    0.3053;
A[22][9] =   -0.2798;
A[22][10] =   -1.7555;
A[22][11] =   -2.1930;
A[22][12] =   -2.2336;
A[22][13] =   -0.6478;
A[22][14] =   -0.9588;
A[22][15] =    0.6115;
A[22][16] =   -2.4838;
A[22][17] =    1.5761;
A[22][18] =    0.9572;
A[22][19] =   -2.0002;
A[22][20] =   -2.6515;
A[22][21] =    0.2697;
A[22][22] =   -0.1969;
A[22][23] =    0.0941;
A[22][24] =   -0.0829;
A[22][25] =    0.5177;
A[22][26] =    5.7163;
A[22][27] =    0.9158;
A[22][28] =   -0.6963;
A[22][29] =   -0.2630;
A[23][0] =   -0.0163;
A[23][1] =   -0.7882;
A[23][2] =    0.2684;
A[23][3] =    0.1081;
A[23][4] =    0.8598;
A[23][5] =   -0.7712;
A[23][6] =    0.3763;
A[23][7] =    0.6838;
A[23][8] =    1.3863;
A[23][9] =   -0.6387;
A[23][10] =   -0.5608;
A[23][11] =    0.8610;
A[23][12] =    1.1374;
A[23][13] =    1.7344;
A[23][14] =   -1.9730;
A[23][15] =    1.4546;
A[23][16] =    2.6508;
A[23][17] =   -2.9497;
A[23][18] =    0.1979;
A[23][19] =   -0.2432;
A[23][20] =   -0.6831;
A[23][21] =   -2.0857;
A[23][22] =    0.0794;
A[23][23] =   -0.9616;
A[23][24] =   -0.6302;
A[23][25] =    0.6727;
A[23][26] =    1.9863;
A[23][27] =   -0.7404;
A[23][28] =    0.8391;
A[23][29] =    0.4164;
 double b[m];
b[0] =    0.9091;
b[1] =    3.6916;
b[2] =    4.6042;
b[3] =    6.7599;
b[4] =    6.6500;
b[5] =   -4.1814;
b[6] =    1.8859;
b[7] =   -1.2215;
b[8] =    4.1261;
b[9] =    9.0326;
b[10] =    1.1128;
b[11] =    6.2793;
b[12] =    4.3949;
b[13] =    5.4864;
b[14] =    6.7281;
b[15] =    8.5006;
b[16] =    0.4231;
b[17] =    2.8560;
b[18] =    2.7521;
b[19] =    1.6056;
b[20] =   -5.3950;
b[21] =    3.2086;
b[22] =   -5.5615;
b[23] =   -1.9608;
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
