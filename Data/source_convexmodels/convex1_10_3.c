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
 n = 10;
 double x[n];
 FILE *file_input;
 file_input = fopen("input.in","r");
 for (i = 0; i < n; i++)
 {
 fscanf(file_input, "%lf\n", &x[i]);
 }
 fclose(file_input);
 
 double a[n][2*n];
a[0][0] =   12.9959;
a[0][10] =  -12.9959;
a[0][1] =   14.7420;
a[0][11] =  -14.7420;
a[0][2] =   57.1118;
a[0][12] =  -57.1118;
a[0][3] =   -6.5864;
a[0][13] =    6.5864;
a[0][4] =   32.5616;
a[0][14] =  -32.5616;
a[0][5] =   -7.0320;
a[0][15] =    7.0320;
a[0][6] =  -58.2107;
a[0][16] =   58.2107;
a[0][7] =   19.4423;
a[0][17] =  -19.4423;
a[0][8] =   79.4383;
a[0][18] =  -79.4383;
a[0][9] =  -40.3512;
a[0][19] =   40.3512;
a[1][0] =   28.0624;
a[1][10] =  -28.0624;
a[1][1] =  -89.5844;
a[1][11] =   89.5844;
a[1][2] =    2.6755;
a[1][12] =   -2.6755;
a[1][3] =   29.6397;
a[1][13] =  -29.6397;
a[1][4] =  -33.8342;
a[1][14] =   33.8342;
a[1][5] =   52.7914;
a[1][15] =  -52.7914;
a[1][6] =   81.0307;
a[1][16] =  -81.0307;
a[1][7] =  -40.0126;
a[1][17] =   40.0126;
a[1][8] =  -60.6684;
a[1][18] =   60.6684;
a[1][9] =  -90.7297;
a[1][19] =   90.7297;
a[2][0] =  -16.5942;
a[2][10] =   16.5942;
a[2][1] =   86.2403;
a[2][11] =  -86.2403;
a[2][2] =  -64.4795;
a[2][12] =   64.4795;
a[2][3] =  -94.9544;
a[2][13] =   94.9544;
a[2][4] =   79.6972;
a[2][14] =  -79.6972;
a[2][5] =   63.6408;
a[2][15] =  -63.6408;
a[2][6] =   35.0782;
a[2][16] =  -35.0782;
a[2][7] =  -73.1754;
a[2][17] =   73.1754;
a[2][8] =  -81.3259;
a[2][18] =   81.3259;
a[2][9] =    1.0856;
a[2][19] =   -1.0856;
a[3][0] =  -58.8049;
a[3][10] =   58.8049;
a[3][1] =   45.7323;
a[3][11] =  -45.7323;
a[3][2] =  -20.2821;
a[3][12] =   20.2821;
a[3][3] =   68.4413;
a[3][13] =  -68.4413;
a[3][4] =  -76.3690;
a[3][14] =   76.3690;
a[3][5] =  -79.9557;
a[3][15] =   79.9557;
a[3][6] =   -6.3064;
a[3][16] =    6.3064;
a[3][7] =  -57.4797;
a[3][17] =   57.4797;
a[3][8] =  -38.5266;
a[3][18] =   38.5266;
a[3][9] =   52.2852;
a[3][19] =  -52.2852;
a[4][0] =   89.5866;
a[4][10] =  -89.5866;
a[4][1] =   47.5683;
a[4][11] =  -47.5683;
a[4][2] =  -73.2137;
a[4][12] =   73.2137;
a[4][3] =   11.8065;
a[4][13] =  -11.8065;
a[4][4] =   97.6836;
a[4][14] =  -97.6836;
a[4][5] =  -64.3766;
a[4][15] =   64.3766;
a[4][6] =   82.4265;
a[4][16] =  -82.4265;
a[4][7] =   78.9883;
a[4][17] =  -78.9883;
a[4][8] =   -8.7885;
a[4][18] =    8.7885;
a[4][9] =   26.2140;
a[4][19] =  -26.2140;
a[5][0] =  -83.5858;
a[5][10] =   83.5858;
a[5][1] =  -87.3191;
a[5][11] =   87.3191;
a[5][2] =  -93.8221;
a[5][12] =   93.8221;
a[5][3] =   70.8200;
a[5][13] =  -70.8200;
a[5][4] =    7.9964;
a[5][14] =   -7.9964;
a[5][5] =  -28.0730;
a[5][15] =   28.0730;
a[5][6] =  -79.1977;
a[5][16] =   79.1977;
a[5][7] =  -85.7094;
a[5][17] =   85.7094;
a[5][8] =  -79.6661;
a[5][18] =   79.6661;
a[5][9] =  -82.0217;
a[5][19] =   82.0217;
a[6][0] =  -78.8581;
a[6][10] =   78.8581;
a[6][1] =   72.0881;
a[6][11] =  -72.0881;
a[6][2] =   87.8283;
a[6][12] =  -87.8283;
a[6][3] =  -30.4242;
a[6][13] =   30.4242;
a[6][4] =   41.3835;
a[6][14] =  -41.3835;
a[6][5] =  -88.6591;
a[6][15] =   88.6591;
a[6][6] =   49.1092;
a[6][16] =  -49.1092;
a[6][7] =  -51.5027;
a[6][17] =   51.5027;
a[6][8] =   99.0779;
a[6][18] =  -99.0779;
a[6][9] =  -83.8275;
a[6][19] =   83.8275;
a[7][0] =  -71.5918;
a[7][10] =   71.5918;
a[7][1] =   86.8810;
a[7][11] =  -86.8810;
a[7][2] =  -39.7388;
a[7][12] =   39.7388;
a[7][3] =  -10.7947;
a[7][13] =   10.7947;
a[7][4] =   99.8983;
a[7][14] =  -99.8983;
a[7][5] =    4.3771;
a[7][15] =   -4.3771;
a[7][6] =   47.2535;
a[7][16] =  -47.2535;
a[7][7] =  -89.2491;
a[7][17] =   89.2491;
a[7][8] =  -33.5814;
a[7][18] =   33.5814;
a[7][9] =   55.4481;
a[7][19] =  -55.4481;
a[8][0] =  -66.7079;
a[8][10] =   66.7079;
a[8][1] =   96.8797;
a[8][11] =  -96.8797;
a[8][2] =  -40.8932;
a[8][12] =   40.8932;
a[8][3] =  -89.1521;
a[8][13] =   89.1521;
a[8][4] =  -42.4301;
a[8][14] =   42.4301;
a[8][5] =  -32.8302;
a[8][15] =   32.8302;
a[8][6] =   12.3723;
a[8][16] =  -12.3723;
a[8][7] =  -11.6556;
a[8][17] =   11.6556;
a[8][8] =  -40.5306;
a[8][18] =   40.5306;
a[8][9] =   81.0269;
a[8][19] =  -81.0269;
a[9][0] =   24.1917;
a[9][10] =  -24.1917;
a[9][1] =   71.7878;
a[9][11] =  -71.7878;
a[9][2] =  -33.4127;
a[9][12] =   33.4127;
a[9][3] =  -64.5785;
a[9][13] =   64.5785;
a[9][4] =  -17.0955;
a[9][14] =   17.0955;
a[9][5] =  -64.8662;
a[9][15] =   64.8662;
a[9][6] =  -63.1612;
a[9][16] =   63.1612;
a[9][7] =  -97.3434;
a[9][17] =   97.3434;
a[9][8] =  -87.5910;
a[9][18] =   87.5910;
a[9][9] =    6.7544;
a[9][19] =   -6.7544;
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