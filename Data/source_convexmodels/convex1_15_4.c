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
 n = 15;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double a[n][2*n];
a[0][0] =  -72.0621;
a[0][15] =   72.0621;
a[0][1] =  -14.2079;
a[0][16] =   14.2079;
a[0][2] =   17.0363;
a[0][17] =  -17.0363;
a[0][3] =    9.1772;
a[0][18] =   -9.1772;
a[0][4] =  -48.7780;
a[0][19] =   48.7780;
a[0][5] =   51.5677;
a[0][20] =  -51.5677;
a[0][6] =  -32.0486;
a[0][21] =   32.0486;
a[0][7] =   42.8943;
a[0][22] =  -42.8943;
a[0][8] =  -23.4124;
a[0][23] =   23.4124;
a[0][9] =   93.7555;
a[0][24] =  -93.7555;
a[0][10] =  -12.4454;
a[0][25] =   12.4454;
a[0][11] =   79.9552;
a[0][26] =  -79.9552;
a[0][12] =   -8.4492;
a[0][27] =    8.4492;
a[0][13] =   79.7095;
a[0][28] =  -79.7095;
a[0][14] =  -86.3258;
a[0][29] =   86.3258;
a[1][0] =  -21.2200;
a[1][15] =   21.2200;
a[1][1] =  -97.9645;
a[1][16] =   97.9645;
a[1][2] =   33.5364;
a[1][17] =  -33.5364;
a[1][3] =   88.6340;
a[1][18] =  -88.6340;
a[1][4] =   77.3128;
a[1][19] =  -77.3128;
a[1][5] =   91.4224;
a[1][20] =  -91.4224;
a[1][6] =  -42.5301;
a[1][21] =   42.5301;
a[1][7] =   35.8439;
a[1][22] =  -35.8439;
a[1][8] =  -94.0664;
a[1][23] =   94.0664;
a[1][9] =   64.8999;
a[1][24] =  -64.8999;
a[1][10] =  -43.9540;
a[1][25] =   43.9540;
a[1][11] =  -56.4270;
a[1][26] =   56.4270;
a[1][12] =   44.4419;
a[1][27] =  -44.4419;
a[1][13] =   86.9418;
a[1][28] =  -86.9418;
a[1][14] =  -83.0059;
a[1][29] =   83.0059;
a[2][0] =   96.1126;
a[2][15] =  -96.1126;
a[2][1] =   21.7643;
a[2][16] =  -21.7643;
a[2][2] =   29.6054;
a[2][17] =  -29.6054;
a[2][3] =  -35.7054;
a[2][18] =   35.7054;
a[2][4] =  -10.6398;
a[2][19] =   10.6398;
a[2][5] =  -31.4259;
a[2][20] =   31.4259;
a[2][6] =  -65.8194;
a[2][21] =   65.8194;
a[2][7] =   91.8761;
a[2][22] =  -91.8761;
a[2][8] =   -5.5358;
a[2][23] =    5.5358;
a[2][9] =   91.9219;
a[2][24] =  -91.9219;
a[2][10] =   97.0493;
a[2][25] =  -97.0493;
a[2][11] =  -84.6042;
a[2][26] =   84.6042;
a[2][12] =  -32.2002;
a[2][27] =   32.2002;
a[2][13] =   63.5774;
a[2][28] =  -63.5774;
a[2][14] =  -86.3321;
a[2][29] =   86.3321;
a[3][0] =   28.9588;
a[3][15] =  -28.9588;
a[3][1] =   91.5950;
a[3][16] =  -91.5950;
a[3][2] =  -13.3262;
a[3][17] =   13.3262;
a[3][3] =   61.2934;
a[3][18] =  -61.2934;
a[3][4] =   63.1975;
a[3][19] =  -63.1975;
a[3][5] =   27.6487;
a[3][20] =  -27.6487;
a[3][6] =  -20.1473;
a[3][21] =   20.1473;
a[3][7] =   55.0669;
a[3][22] =  -55.0669;
a[3][8] =  -33.3255;
a[3][23] =   33.3255;
a[3][9] =   29.2692;
a[3][24] =  -29.2692;
a[3][10] =   21.7517;
a[3][25] =  -21.7517;
a[3][11] =   -5.1570;
a[3][26] =    5.1570;
a[3][12] =  -19.7560;
a[3][27] =   19.7560;
a[3][13] =   41.7818;
a[3][28] =  -41.7818;
a[3][14] =  -18.0367;
a[3][29] =   18.0367;
a[4][0] =   79.2820;
a[4][15] =  -79.2820;
a[4][1] =  -80.9107;
a[4][16] =   80.9107;
a[4][2] =  -72.0482;
a[4][17] =   72.0482;
a[4][3] =   20.2798;
a[4][18] =  -20.2798;
a[4][4] =  -80.3325;
a[4][19] =   80.3325;
a[4][5] =  -31.3988;
a[4][20] =   31.3988;
a[4][6] =   39.5299;
a[4][21] =  -39.5299;
a[4][7] =   21.5454;
a[4][22] =  -21.5454;
a[4][8] =   95.1690;
a[4][23] =  -95.1690;
a[4][9] =  -24.0853;
a[4][24] =   24.0853;
a[4][10] =  -49.2502;
a[4][25] =   49.2502;
a[4][11] =   67.0055;
a[4][26] =  -67.0055;
a[4][12] =    5.3960;
a[4][27] =   -5.3960;
a[4][13] =   48.6441;
a[4][28] =  -48.6441;
a[4][14] =  -75.3237;
a[4][29] =   75.3237;
a[5][0] =   -3.5539;
a[5][15] =    3.5539;
a[5][1] =  -92.8819;
a[5][16] =   92.8819;
a[5][2] =   50.3859;
a[5][17] =  -50.3859;
a[5][3] =   57.9241;
a[5][18] =  -57.9241;
a[5][4] =   71.9187;
a[5][19] =  -71.9187;
a[5][5] =  -56.7057;
a[5][20] =   56.7057;
a[5][6] =  -59.2647;
a[5][21] =   59.2647;
a[5][7] =   89.6005;
a[5][22] =  -89.6005;
a[5][8] =   11.0887;
a[5][23] =  -11.0887;
a[5][9] =   -4.6851;
a[5][24] =    4.6851;
a[5][10] =  -73.4771;
a[5][25] =   73.4771;
a[5][11] =   -6.1213;
a[5][26] =    6.1213;
a[5][12] =   78.8471;
a[5][27] =  -78.8471;
a[5][13] =   79.9420;
a[5][28] =  -79.9420;
a[5][14] =  -11.3967;
a[5][29] =   11.3967;
a[6][0] =  -97.1814;
a[6][15] =   97.1814;
a[6][1] =   77.2470;
a[6][16] =  -77.2470;
a[6][2] =  -51.6426;
a[6][17] =   51.6426;
a[6][3] =   59.8370;
a[6][18] =  -59.8370;
a[6][4] =  -94.4742;
a[6][19] =   94.4742;
a[6][5] =   57.2401;
a[6][20] =  -57.2401;
a[6][6] =   33.2653;
a[6][21] =  -33.2653;
a[6][7] =  -88.0717;
a[6][22] =   88.0717;
a[6][8] =   69.2607;
a[6][23] =  -69.2607;
a[6][9] =   82.3780;
a[6][24] =  -82.3780;
a[6][10] =    9.0010;
a[6][25] =   -9.0010;
a[6][11] =  -17.2460;
a[6][26] =   17.2460;
a[6][12] =   55.6721;
a[6][27] =  -55.6721;
a[6][13] =  -86.9520;
a[6][28] =   86.9520;
a[6][14] =   79.7879;
a[6][29] =  -79.7879;
a[7][0] =   24.5761;
a[7][15] =  -24.5761;
a[7][1] =  -50.6117;
a[7][16] =   50.6117;
a[7][2] =   30.0919;
a[7][17] =  -30.0919;
a[7][3] =  -90.0870;
a[7][18] =   90.0870;
a[7][4] =   79.8313;
a[7][19] =  -79.8313;
a[7][5] =   44.6180;
a[7][20] =  -44.6180;
a[7][6] =  -11.3868;
a[7][21] =   11.3868;
a[7][7] =  -46.2575;
a[7][22] =   46.2575;
a[7][8] =  -18.3873;
a[7][23] =   18.3873;
a[7][9] =  -97.0292;
a[7][24] =   97.0292;
a[7][10] =   65.5608;
a[7][25] =  -65.5608;
a[7][11] =    0.5493;
a[7][26] =   -0.5493;
a[7][12] =  -86.1265;
a[7][27] =   86.1265;
a[7][13] =  -32.8178;
a[7][28] =   32.8178;
a[7][14] =  -29.2722;
a[7][29] =   29.2722;
a[8][0] =  -53.7809;
a[8][15] =   53.7809;
a[8][1] =  -98.2170;
a[8][16] =   98.2170;
a[8][2] =   71.4748;
a[8][17] =  -71.4748;
a[8][3] =  -43.3603;
a[8][18] =   43.3603;
a[8][4] =   79.9871;
a[8][19] =  -79.9871;
a[8][5] =  -44.2322;
a[8][20] =   44.2322;
a[8][6] =  -13.3409;
a[8][21] =   13.3409;
a[8][7] =   97.3360;
a[8][22] =  -97.3360;
a[8][8] =   -7.5964;
a[8][23] =    7.5964;
a[8][9] =  -68.6612;
a[8][24] =   68.6612;
a[8][10] =   67.4013;
a[8][25] =  -67.4013;
a[8][11] =  -74.9120;
a[8][26] =   74.9120;
a[8][12] =  -44.2431;
a[8][27] =   44.2431;
a[8][13] =  -99.1330;
a[8][28] =   99.1330;
a[8][14] =  -75.9644;
a[8][29] =   75.9644;
a[9][0] =    5.4868;
a[9][15] =   -5.4868;
a[9][1] =   62.9841;
a[9][16] =  -62.9841;
a[9][2] =  -83.1259;
a[9][17] =   83.1259;
a[9][3] =   30.6914;
a[9][18] =  -30.6914;
a[9][4] =    4.8212;
a[9][19] =   -4.8212;
a[9][5] =   16.4863;
a[9][20] =  -16.4863;
a[9][6] =  -64.9521;
a[9][21] =   64.9521;
a[9][7] =   54.4413;
a[9][22] =  -54.4413;
a[9][8] =   65.2613;
a[9][23] =  -65.2613;
a[9][9] =   -5.6864;
a[9][24] =    5.6864;
a[9][10] =   66.6697;
a[9][25] =  -66.6697;
a[9][11] =  -73.5429;
a[9][26] =   73.5429;
a[9][12] =  -24.1260;
a[9][27] =   24.1260;
a[9][13] =   65.6191;
a[9][28] =  -65.6191;
a[9][14] =   13.8222;
a[9][29] =  -13.8222;
a[10][0] =   44.9984;
a[10][15] =  -44.9984;
a[10][1] =  -71.9001;
a[10][16] =   71.9001;
a[10][2] =   94.4178;
a[10][17] =  -94.4178;
a[10][3] =   -2.0689;
a[10][18] =    2.0689;
a[10][4] =  -75.9601;
a[10][19] =   75.9601;
a[10][5] =  -15.7988;
a[10][20] =   15.7988;
a[10][6] =  -61.3595;
a[10][21] =   61.3595;
a[10][7] =   -4.9291;
a[10][22] =    4.9291;
a[10][8] =   98.2406;
a[10][23] =  -98.2406;
a[10][9] =    8.5983;
a[10][24] =   -8.5983;
a[10][10] =  -59.2569;
a[10][25] =   59.2569;
a[10][11] =   74.0951;
a[10][26] =  -74.0951;
a[10][12] =   72.9345;
a[10][27] =  -72.9345;
a[10][13] =    1.4871;
a[10][28] =   -1.4871;
a[10][14] =   75.0065;
a[10][29] =  -75.0065;
a[11][0] =   21.4832;
a[11][15] =  -21.4832;
a[11][1] =   75.9733;
a[11][16] =  -75.9733;
a[11][2] =  -93.7080;
a[11][17] =   93.7080;
a[11][3] =   94.5704;
a[11][18] =  -94.5704;
a[11][4] =  -64.4412;
a[11][19] =   64.4412;
a[11][5] =  -81.5863;
a[11][20] =   81.5863;
a[11][6] =   23.2843;
a[11][21] =  -23.2843;
a[11][7] =   36.1799;
a[11][22] =  -36.1799;
a[11][8] =    4.7895;
a[11][23] =   -4.7895;
a[11][9] =  -88.0618;
a[11][24] =   88.0618;
a[11][10] =    8.8835;
a[11][25] =   -8.8835;
a[11][11] =   20.5900;
a[11][26] =  -20.5900;
a[11][12] =  -16.0079;
a[11][27] =   16.0079;
a[11][13] =  -26.7675;
a[11][28] =   26.7675;
a[11][14] =  -30.2850;
a[11][29] =   30.2850;
a[12][0] =   17.6733;
a[12][15] =  -17.6733;
a[12][1] =  -80.9246;
a[12][16] =   80.9246;
a[12][2] =   67.0809;
a[12][17] =  -67.0809;
a[12][3] =   49.6980;
a[12][18] =  -49.6980;
a[12][4] =   41.2215;
a[12][19] =  -41.2215;
a[12][5] =  -95.1945;
a[12][20] =   95.1945;
a[12][6] =  -46.1979;
a[12][21] =   46.1979;
a[12][7] =  -16.6130;
a[12][22] =   16.6130;
a[12][8] =   85.0874;
a[12][23] =  -85.0874;
a[12][9] =   31.6061;
a[12][24] =  -31.6061;
a[12][10] =   74.9885;
a[12][25] =  -74.9885;
a[12][11] =  -46.9397;
a[12][26] =   46.9397;
a[12][12] =  -52.0245;
a[12][27] =   52.0245;
a[12][13] =  -54.6721;
a[12][28] =   54.6721;
a[12][14] =  -91.6158;
a[12][29] =   91.6158;
a[13][0] =  -13.3130;
a[13][15] =   13.3130;
a[13][1] =  -29.4880;
a[13][16] =   29.4880;
a[13][2] =   67.1427;
a[13][17] =  -67.1427;
a[13][3] =   13.5682;
a[13][18] =  -13.5682;
a[13][4] =   66.2720;
a[13][19] =  -66.2720;
a[13][5] =   -1.7708;
a[13][20] =    1.7708;
a[13][6] =   11.9356;
a[13][21] =  -11.9356;
a[13][7] =  -23.9702;
a[13][22] =   23.9702;
a[13][8] =   47.8044;
a[13][23] =  -47.8044;
a[13][9] =   77.9271;
a[13][24] =  -77.9271;
a[13][10] =  -75.8000;
a[13][25] =   75.8000;
a[13][11] =   72.9605;
a[13][26] =  -72.9605;
a[13][12] =   19.5309;
a[13][27] =  -19.5309;
a[13][13] =    6.9663;
a[13][28] =   -6.9663;
a[13][14] =  -71.5321;
a[13][29] =   71.5321;
a[14][0] =  -51.1654;
a[14][15] =   51.1654;
a[14][1] =   18.6841;
a[14][16] =  -18.6841;
a[14][2] =  -90.0285;
a[14][17] =   90.0285;
a[14][3] =  -40.2072;
a[14][18] =   40.2072;
a[14][4] =  -93.0332;
a[14][19] =   93.0332;
a[14][5] =  -44.3466;
a[14][20] =   44.3466;
a[14][6] =   88.9568;
a[14][21] =  -88.9568;
a[14][7] =  -57.3459;
a[14][22] =   57.3459;
a[14][8] =   13.4860;
a[14][23] =  -13.4860;
a[14][9] =  -78.0735;
a[14][24] =   78.0735;
a[14][10] =   71.2702;
a[14][25] =  -71.2702;
a[14][11] =  -88.3781;
a[14][26] =   88.3781;
a[14][12] =   -4.1191;
a[14][27] =    4.1191;
a[14][13] =  -42.1030;
a[14][28] =   42.1030;
a[14][14] =  -84.6814;
a[14][29] =   84.6814;
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