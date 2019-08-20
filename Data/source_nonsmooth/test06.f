* SUBROUTINE TIUD06             ALL SYSTEMS                 99/12/01
C PORTABILITY : ALL SYSTEMS
C 90/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
*  INITIATION OF VARIABLES FOR NONLINEAR MINIMAX APPROXIMATION.
*  UNCONSTRAINED DENSE VERSION.
*
* PARAMETERS :
*  IO  N  NUMBER OF VARIABLES.
*  IO  NA  NUMBER OF PARTIAL FUNCTIONS.
*  RO  X(N)  VECTOR OF VARIABLES.
*  RO  FMIN  LOWER BOUND FOR VALUE OF THE OBJECTIVE FUNCTION.
*  RO  XMAX  MAXIMUM STEPSIZE.
*  IO  NEXT  NUMBER OF THE TEST PROBLEM.
*  IO  IEXT  TYPE OF OBJECTIVE FUNCTION. IEXT<0-MAXIMUM OF VALUES.
*         IEXT=0-MAXIMUM OF ABSOLUTE VALUES.
*  IO  IERR  ERROR INDICATOR.
*
      SUBROUTINE TIUD06(N,NA,X,FMIN,XMAX,NEXT,IEXT,IERR)
      INTEGER I,N,NA,NEXT,IEXT,IERR
      REAL*8 X(N),FMIN,XMAX
      REAL*8 Y(123),T
      COMMON /EMPR06/ Y
      FMIN=-1.0D 60
      XMAX=1.0D 3
      IEXT=-1
      IERR=0
      GOTO(10,190,180,80,20,170,120,90,230,210,140,150,200,30,130,100,
     & 40,110,50,60,70,220,160,240,250),NEXT
   10 IF(N.GE.2.AND.NA.GE.3) THEN
C      N=2
      NA=3
      X(1)=2.0D 0
      X(2)=2.0D 0
      ELSE
      IERR=1
      ENDIF
      RETURN
  190 IF(N.GE.2.AND.NA.GE.3) THEN
C     N=2
      NA=3
      X(1)=3.0D 0
      X(2)=1.0D 0
      ELSE
      IERR=1
      ENDIF
      RETURN
  180 IF(N.GE.2.AND.NA.GE.2) THEN
C      N=2
      NA=2
      X(1)= 1.41831D 0
      X(2)=-4.79462D 0
      ELSE
      IERR=1
      ENDIF
      RETURN
   80 IF(N.GE.3.AND.NA.GE.6) THEN
C      N=3
      NA=6
      X(1)=1.0D 0
      X(2)=1.0D 0
      X(3)=1.0D 0
      ELSE
      IERR=1
      ENDIF
      RETURN
   20 IF(N.GE.4.AND.NA.GE.4) THEN
C      N=4
      NA=4
      DO 21 I=1,N
      X(I)=0.0D 0
   21 CONTINUE
      ELSE
      IERR=1
      ENDIF
      RETURN
  170 IF(N.GE.4.AND.NA.GE.4) THEN
C      N=4
      NA=4
      DO 171 I=1,N
      X(I)=0.0D 0
  171 CONTINUE
      ELSE
      IERR=1
      ENDIF
      RETURN
  120 IF(N.GE.3.AND.NA.GE.21) THEN
C      N=3
      NA=21
      DO 121 I=1,N
      X(I)=1.0D 0
  121 CONTINUE
      DO 122 I=1,NA
      T=1.0D 1*DBLE(I-1)/DBLE(NA-1)
      Y(I)=T
      Y(NA+I)=(3.0D 0/2.0D 1)*EXP(-T)+
     & (1.0D 0/5.2D 1)*EXP(-5.0D 0*T)-
     & (1.0D 0/6.5D 1)*EXP(-2.0D 0*T)*(3.0D 0*SIN(2.0D 0*T)+
     & 1.1D 1*COS(2.0D 0*T))
  122 CONTINUE
      IEXT=0
      ELSE
      IERR=1
      ENDIF
      RETURN
   90 IF(N.GE.3.AND.NA.GE.15) THEN
C      N=3
      NA=15
      X(1)=1.0D 0
      X(2)=1.0D 0
      X(3)=1.0D 0
      Y(1)=0.14D 0
      Y(2)=0.18D 0
      Y(3)=0.22D 0
      Y(4)=0.25D 0
      Y(5)=0.29D 0
      Y(6)=0.32D 0
      Y(7)=0.35D 0
      Y(8)=0.39D 0
      Y(9)=0.37D 0
      Y(10)=0.58D 0
      Y(11)=0.73D 0
      Y(12)=0.96D 0
      Y(13)=1.34D 0
      Y(14)=2.10D 0
      Y(15)=4.39D 0
      IEXT=0
      ELSE
      IERR=1
      ENDIF
      RETURN
  230 IF(N.GE.4.AND.NA.GE.11) THEN
C      N=4
      NA=11
      X(1)= 0.25D 0
      X(2)= 0.39D 0
      X(3)= 4.15D-1
      X(4)= 0.39D 0
      Y(1)= 0.1957D 0
      Y(2)= 0.1947D 0
      Y(3)= 0.1735D 0
      Y(4)= 0.1600D 0
      Y(5)= 0.0844D 0
      Y(6)= 0.0627D 0
      Y(7)= 0.0456D 0
      Y(8)= 0.0342D 0
      Y(9)= 0.0323D 0
      Y(10)=0.0235D 0
      Y(11)=0.0246D 0
      Y(12)=4.0000D 0
      Y(13)=2.0000D 0
      Y(14)=1.0000D 0
      Y(15)=0.5000D 0
      Y(16)=0.2500D 0
      Y(17)=0.1670D 0
      Y(18)=0.1250D 0
      Y(19)=0.1000D 0
      Y(20)=0.0833D 0
      Y(21)=0.0714D 0
      Y(22)=0.0625D 0
      IEXT=0
      ELSE
      IERR=1
      ENDIF
      RETURN
  210 IF(N.GE.4.AND.NA.GE.20) THEN
C      N=4
      NA=20
      X(1)= 2.5D 1
      X(2)= 5.0D 0
      X(3)=-5.0D 0
      X(4)=-1.0D 0
      IEXT=0
      ELSE
      IERR=1
      ENDIF
      RETURN
  140 IF(N.GE.4.AND.NA.GE.21) THEN
C      N=4
      NA=21
      DO 141 I=1,N
      X(I)=1.0D 0
  141 CONTINUE
      DO 142 I=1,NA
      Y(I)=0.25D 0+0.75D 0*DBLE(I-1)/DBLE(NA-1)
      Y(NA+I)=SQRT(Y(I))
  142 CONTINUE
      IEXT=0
      ELSE
      IERR=1
      ENDIF
      RETURN
  150 IF(N.GE.4.AND.NA.GE.21) THEN
C      N=4
      NA=21
      X(1)= 1.0D 0
      X(2)= 1.0D 0
      X(3)=-3.0D 0
      X(4)=-1.0D 0
      DO 151 I=1,NA
      Y(I)=-0.5D 0+DBLE(I-1)/DBLE(NA-1)
      Y(NA+I)=1.0D 0/(1.0D 0+Y(I))
  151 CONTINUE
      IEXT=0
      ELSE
      IERR=1
      ENDIF
      RETURN
  200 IF(N.GE.4.AND.NA.GE.61) THEN
C      N=4
      NA=61
      IEXT=0
      DO 201 I=1,N
      X(I)=1.0D 0
  201 CONTINUE
      X(3)=1.0D 1
      Y(1)=1.0D 0
      Y(2)=1.01D 0
      Y(3)=1.02D 0
      Y(4)=1.03D 0
      Y(5)=1.05D 0
      Y(6)=1.075D 0
      Y(7)=1.1D 0
      Y(8)=1.125D 0
      Y(9)=1.15D 0
      Y(10)=1.2D 0
      Y(11)=1.25D 0
      Y(12)=1.3D 0
      Y(13)=1.35D 0
      Y(14)=1.4D 0
      Y(15)=1.5D 0
      Y(16)=1.6D 0
      Y(17)=1.7D 0
      Y(18)=1.8D 0
      Y(19)=1.9D 0
      Y(20)=2.0D 0
      Y(21)=2.1D 0
      Y(22)=2.2D 0
      Y(23)=2.3D 0
      Y(24)=2.5D 0
      Y(25)=2.75D 0
      Y(26)=3.0D 0
      Y(27)=3.25D 0
      Y(28)=3.5D 0
      Y(29)=4.0D 0
      Y(30)=4.5D 0
      Y(31)=5.0D 0
      Y(32)=5.5D 0
      Y(33)=6.0D 0
      Y(34)=6.5D 0
      Y(35)=7.0D 0
      Y(36)=7.5D 0
      Y(37)=8.0D 0
      Y(38)=8.5D 0
      Y(39)=9.0D 0
      Y(40)=10.0D 0
      Y(41)=11.0D 0
      Y(42)=12.0D 0
      Y(43)=13.0D 0
      Y(44)=15.0D 0
      Y(45)=17.5D 0
      Y(46)=20.0D 0
      Y(47)=22.5D 0
      Y(48)=25.0D 0
      Y(49)=30.0D 0
      Y(50)=35.0D 0
      Y(51)=40.0D 0
      Y(52)=50.0D 0
      Y(53)=60.0D 0
      Y(54)=70.0D 0
      Y(55)=80.0D 0
      Y(56)=100.0D 0
      Y(57)=150.0D 0
      Y(58)=200.0D 0
      Y(59)=300.0D 0
      Y(60)=500.0D 0
      Y(61)=1.0D 5
      Y(61+1)= 0.97386702052733792831D 0
      Y(61+2)= 0.97390711665677071911D 0
      Y(61+3)= 0.97394794566286525039D 0
      Y(61+4)= 0.97398947529386626621D 0
      Y(61+5)= 0.97407451325974368215D 0
      Y(61+6)= 0.97418422166965892644D 0
      Y(61+7)= 0.97429732692565188272D 0
      Y(61+8)= 0.97441344289222034304D 0
      Y(61+9)= 0.97453221704823108216D 0
      Y(61+10)=0.97477647977277153145D 0
      Y(61+11)=0.97502785781178233026D 0
      Y(61+12)=0.97528446418205610067D 0
      Y(61+13)=0.97554472005909873148D 0
      Y(61+14)=0.97580730389916439626D 0
      Y(61+15)=0.97633521198091785788D 0
      Y(61+16)=0.97686134356195586299D 0
      Y(61+17)=0.97738094095418268249D 0
      Y(61+18)=0.97789073928751194169D 0
      Y(61+19)=0.97838854811088140808D 0
      Y(61+20)=0.97887295363155439576D 0
      Y(61+21)=0.97934310478576951385D 0
      Y(61+22)=0.97979855827226762515D 0
      Y(61+23)=0.98023916551033862691D 0
      Y(61+24)=0.98107624468416045728D 0
      Y(61+25)=0.98204290774765289406D 0
      Y(61+26)=0.98292719363632655668D 0
      Y(61+27)=0.98373656564197279264D 0
      Y(61+28)=0.98447846610682328991D 0
      Y(61+29)=0.98578713114264981186D 0
      Y(61+30)=0.98690124654380846379D 0
      Y(61+31)=0.98785879054855173380D 0
      Y(61+32)=0.98868928566806726978D 0
      Y(61+33)=0.98941568049711884384D 0
      Y(61+34)=0.99005592865089067038D 0
      Y(61+35)=0.99062420259214811899D 0
      Y(61+36)=0.99113180018738487730D 0
      Y(61+37)=0.99158781685339306121D 0
      Y(61+38)=0.99199964493176098231D 0
      Y(61+39)=0.99237334707422899195D 0
      Y(61+40)=0.99302559755582945576D 0
      Y(61+41)=0.99357562712206729735D 0
      Y(61+42)=0.99404560031581354300D 0
      Y(61+43)=0.99445173790980305195D 0
      Y(61+44)=0.99511816085114882367D 0
      Y(61+45)=0.99575584307408838284D 0
      Y(61+46)=0.99624640327264396775D 0
      Y(61+47)=0.99663543022201287399D 0
      Y(61+48)=0.99695146031888813172D 0
      Y(61+49)=0.99743367936799001685D 0
      Y(61+50)=0.99778424120023198554D 0
      Y(61+51)=0.99805056960591223604D 0
      Y(61+52)=0.99842841443786596919D 0
      Y(61+53)=0.99868358857261655169D 0
      Y(61+54)=0.99886748198687248566D 0
      Y(61+55)=0.99900629944600342584D 0
      Y(61+56)=0.99920194660435455419D 0
      Y(61+57)=0.99946519560889341627D 0
      Y(61+58)=0.99959785208794891934D 0
      Y(61+59)=0.99973120214935885075D 0
      Y(61+60)=0.99983838442420395745D 0
      Y(61+61)=0.999999189398046846077D 0
      ELSE
        IERR=1
      ENDIF
      RETURN
   30 IF(N.GE.5.AND.NA.GE.21) THEN
C      N=5
      NA=21
      DO 31 I=1,N
      X(I)=0.0D 0
   31 CONTINUE
      X(1)=0.5D 0
      IEXT=0
      ELSE
      IERR=1
      ENDIF
      RETURN
  130 IF(N.GE.5.AND.NA.GE.30) THEN
C      N=5
      NA=30
      X(1)= 0.0D 0
      X(2)=-1.0D 0
      X(3)= 1.0D 1
      X(4)= 1.0D 0
      X(5)= 1.0D 1
      DO 132 I=1,NA
      Y(I)=-1.0D 0+2.0D 0*DBLE(I-1)/DBLE(NA-1)
      T=8.0D 0*Y(I)
      Y(NA+I)=SQRT((T-1.0D 0)**2+1.0D 0)*ATAN(T)/T
  132 CONTINUE
      IEXT=0
      ELSE
      IERR=1
      ENDIF
      RETURN
  100 IF(N.GE.6.AND.NA.GE.51) THEN
C      N=6
      NA=51
      X(1)= 2.0D 0
      X(2)= 2.0D 0
      X(3)= 7.0D 0
      X(4)= 0.0D 0
      X(5)=-2.0D 0
      X(6)= 1.0D 0
      DO 101 I=1,NA
      T=0.1D 0*DBLE(I-1)
      Y(I)=0.5D 0*EXP(-T)-EXP(-2.0D 0*T)+0.5D 0*EXP(-3.0D 0*T)+
     & 1.5D 0*EXP(-1.5D 0*T)*SIN(7.0D 0*T)+
     & EXP(-2.5D 0*T)*SIN(5.0D 0*T)
  101 CONTINUE
      IEXT=0
      ELSE
      IERR=1
      ENDIF
      RETURN
   40 IF(N.GE.6.AND.NA.GE.11) THEN
C      N=6
      NA=11
      X(1)=0.8D 0
      X(2)=1.5D 0
      X(3)=1.2D 0
      X(4)=3.0D 0
      X(5)=0.8D 0
      X(6)=6.0D 0
      Y(1)=0.5D 0
      Y(2)=0.6D 0
      Y(3)=0.7D 0
      Y(4)=0.77D 0
      Y(5)=0.9D 0
      Y(6)=1.0D 0
      Y(7)=1.1D 0
      Y(8)=1.23D 0
      Y(9)=1.3D 0
      Y(10)=1.4D 0
      Y(11)=1.5D 0
      ELSE
      IERR=1
      ENDIF
      RETURN
  110 IF(N.GE.9.AND.NA.GE.41) THEN
C      N=9
      NA=41
      X(1)=0D 0
      X(2)=1D 0
      X(3)=0D 0
      X(4)=-1.5D-1
      X(5)=0D 0
      X(6)=-6.8D-1
      X(7)=0D 0
      X(8)=-7.2D-1
      X(9)=3.7D-1
      DO111 I=1,6
  111 Y(I)=1D-2*(I-1)
      DO112 I=7,20
  112 Y(I)=3D-2*(I-7)+7D-2
      Y(21)=5D-1
      DO113 I=22,35
  113 Y(I)=3D-2*(I-22)+54D-2
      DO114 I=36,41
  114 Y(I)=1D-2*(I-36)+95D-2
      DO115 I=1,41
      Y(41+I)=COS(Y(I)*3.14159265358979324D 0)
  115 Y(82+I)=SIN(Y(I)*3.14159265358979324D 0)
      IEXT=0
      ELSE
        IERR=1
      ENDIF
      RETURN
   50 IF(N.GE.7.AND.NA.GE.5) THEN
C      N=7
      NA=5
      X(1)=1.0D 0
      X(2)=2.0D 0
      X(3)=0.0D 0
      X(4)=4.0D 0
      X(5)=0.0D 0
      X(6)=1.0D 0
      X(7)=1.0D 0
      ELSE
      IERR=1
      ENDIF
      RETURN
   60 IF(N.GE.10.AND.NA.GE.9) THEN
C      N=10
      NA=9
      X(1)=2.0D 0
      X(2)=3.0D 0
      X(3)=5.0D 0
      X(4)=5.0D 0
      X(5)=1.0D 0
      X(6)=2.0D 0
      X(7)=7.0D 0
      X(8)=3.0D 0
      X(9)=6.0D 0
      X(10)=1.0D 1
      ELSE
      IERR=1
      ENDIF
      RETURN
   70 IF(N.GE.20.AND.NA.GE.18) THEN
C      N=20
      NA=18
      X(1)=2.0D 0
      X(2)=3.0D 0
      X(3)=5.0D 0
      X(4)=5.0D 0
      X(5)=1.0D 0
      X(6)=2.0D 0
      X(7)=7.0D 0
      X(8)=3.0D 0
      X(9)=6.0D 0
      X(10)=1.0D 1
      X(11)=2.0D 0
      X(12)=2.0D 0
      X(13)=6.0D 0
      X(14)=1.5D 1
      X(15)=1.0D 0
      X(16)=2.0D 0
      X(17)=1.0D 0
      X(18)=2.0D 0
      X(19)=1.0D 0
      X(20)=3.0D 0
      ELSE
      IERR=1
      ENDIF
      RETURN
  220 IF(N.GE.10.AND.NA.GE.2) THEN
C      N=10
      NA=2
      DO 221 I=1,N
      X(I)=0.1D 0
  221 CONTINUE
      X(1)=1.0D 2
      ELSE
      IERR=1
      ENDIF
      RETURN
  160 IF(N.GE.11.AND.NA.GE.10) THEN
C      N=11
      NA=10
      DO 161 I=1,N
      X(I)=1.0D 0
  161 CONTINUE
      ELSE
      IERR=1
      ENDIF
      RETURN
  240 IF(N.GE.20.AND.NA.GE.31) THEN
C      N=20
      NA=31
      DO 241 I=1,N
      X(I)=0.0D 0
  241 CONTINUE
      IEXT=0
      ELSE
      IERR=1
      ENDIF
      RETURN
  250 IF(N.GE.11.AND.NA.GE.65) THEN
C      N=11
      NA=65
      X(1)= 1.3D 0
      X(2)= 6.5D-1
      X(3)= 6.5D-1
      X(4)= 0.7D 0
      X(5)= 0.6D 0
      X(6)= 3.0D 0
      X(7)= 5.0D 0
      X(8)= 7.0D 0
      X(9)= 2.0D 0
      X(10)=4.5D 0
      X(11)=5.5D 0
      Y(1)= 1.366D 0
      Y(2)= 1.191D 0
      Y(3)= 1.112D 0
      Y(4)= 1.013D 0
      Y(5)= 0.991D 0
      Y(6)= 0.885D 0
      Y(7)= 0.831D 0
      Y(8)= 0.847D 0
      Y(9)= 0.786D 0
      Y(10)=0.725D 0
      Y(11)=0.746D 0
      Y(12)=0.679D 0
      Y(13)=0.608D 0
      Y(14)=0.655D 0
      Y(15)=0.616D 0
      Y(16)=0.606D 0
      Y(17)=0.602D 0
      Y(18)=0.626D 0
      Y(19)=0.651D 0
      Y(20)=0.724D 0
      Y(21)=0.649D 0
      Y(22)=0.649D 0
      Y(23)=0.694D 0
      Y(24)=0.644D 0
      Y(25)=0.624D 0
      Y(26)=0.661D 0
      Y(27)=0.612D 0
      Y(28)=0.558D 0
      Y(29)=0.553D 0
      Y(30)=0.495D 0
      Y(31)=0.500D 0
      Y(32)=0.423D 0
      Y(33)=0.395D 0
      Y(34)=0.375D 0
      Y(35)=0.372D 0
      Y(36)=0.391D 0
      Y(37)=0.396D 0
      Y(38)=0.405D 0
      Y(39)=0.428D 0
      Y(40)=0.429D 0
      Y(41)=0.523D 0
      Y(42)=0.562D 0
      Y(43)=0.607D 0
      Y(44)=0.653D 0
      Y(45)=0.672D 0
      Y(46)=0.708D 0
      Y(47)=0.633D 0
      Y(48)=0.668D 0
      Y(49)=0.645D 0
      Y(50)=0.632D 0
      Y(51)=0.591D 0
      Y(52)=0.559D 0
      Y(53)=0.597D 0
      Y(54)=0.625D 0
      Y(55)=0.739D 0
      Y(56)=0.710D 0
      Y(57)=0.729D 0
      Y(58)=0.720D 0
      Y(59)=0.636D 0
      Y(60)=0.581D 0
      Y(61)=0.428D 0
      Y(62)=0.292D 0
      Y(63)=0.162D 0
      Y(64)=0.098D 0
      Y(65)=0.054D 0
      XMAX=1.0D 1
      IEXT=0
      ELSE
      IERR=1
      ENDIF
      RETURN
      END
* SUBROUTINE TAFU06             ALL SYSTEMS                 99/12/01
C PORTABILITY : ALL SYSTEMS
C 91/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
*  VALUES OF PARTIAL FUNCTIONS IN THE MINIMAX CRITERION.
*
* PARAMETERS :
*  II  N  NUMBER OF VARIABLES.
*  II  KA  INDEX OF THE PARTIAL FUNCTION.
*  RI  X(N)  VECTOR OF VARIABLES.
*  RO  FA  VALUE OF THE PARTIAL FUNCTION AT THE
*          SELECTED POINT.
*  II  NEXT  NUMBER OF THE TEST PROBLEM.
*
      SUBROUTINE TAFU06(N,KA,X,FA,NEXT)
      INTEGER N,KA,NEXT
      REAL*8 X(N),FA
      REAL*8 T,X1,X2,X3,X4,X5,X6,X7,X8
      COMPLEX*16 CA(4),CB(4),C1,C2,C3
      REAL*8 XA(3),XB(3),XC(3),BETA
      INTEGER I,J
      REAL*8 Y(123),PI
      COMMON /EMPR06/ Y
      PARAMETER (PI=3.14159265358979323846D 0)
      GOTO(10,190,180,80,20,170,120,90,230,210,140,150,200,30,130,100,
     & 40,110,50,60,70,220,160,240,250),NEXT
   10 X1=X(1)*X(1)
      X2=X(2)*X(2)
      X3=X(1)+X(1)
      X4=X(2)+X(2)
      GO TO (11,12,13),KA
   11 FA=X1+X2*X2
      RETURN
   12 FA=8.0D 0-4.0D 0*(X(1)+X(2))+X1+X2
      RETURN
   13 FA=2.0D 0*EXP(X(2)-X(1))
      RETURN
  190 X1=1.0D 1*X(1)/(X(1)+1.0D-1)
      X2=2.0D 0*X(2)**2
      IF (KA.EQ.1) THEN
      FA=0.5D 0*( X(1)+X1+X2)
      ELSE IF (KA.EQ.2) THEN
      FA=0.5D 0*(-X(1)+X1+X2)
      ELSE IF (KA.EQ.3) THEN
      FA=0.5D 0*( X(1)-X1+X2)
      ENDIF
      RETURN
  180 X1=X(1)**2+X(2)**2
      X2=SQRT(X1)
      IF (KA.EQ.1) THEN
      FA=(X(1)-X2*COS(X2))**2+5.0D-3*X1
      ELSE IF (KA.EQ.2) THEN
      FA=(X(2)-X2*SIN(X2))**2+5.0D-3*X1
      ENDIF
      RETURN
   80 GO TO (81,82,83,84,85,86),KA
   81 FA=X(1)**2+X(2)**2+X(3)**2-1.0D 0
      RETURN
   82 FA=X(1)**2+X(2)**2+(X(3)-2.0D 0)**2
      RETURN
   83 FA=X(1)+X(2)+X(3)-1.0D 0
      RETURN
   84 FA=X(1)+X(2)-X(3)+1.0D 0
      RETURN
   85 FA=2.0D 0*(X(1)**3+3.0D 0*X(2)**2+(5.0D 0*X(3)-X(1)+1.0D 0)**2)
      RETURN
   86 FA=X(1)**2-9.0D 0*X(3)
      RETURN
   20 X1=X(1)*X(1)
      X2=X(2)*X(2)
      X3=X(3)*X(3)
      X4=X(4)*X(4)
      X5=X(1)+X(1)
      X6=X(2)+X(2)
      X7=X(3)+X(3)
      X8=X(4)+X(4)
      FA=X1+X2+X3+X3+X4-5.0D 0*(X(1)+X(2))-2.1D 1*X(3)+7.0D 0*X(4)
   21 GO TO (31,22,23,24),KA
   22 FA=FA+1.0D 1*(X1+X2+X3+X4+X(1)-X(2)+X(3)-X(4)-8.0D 0)
      RETURN
   23 FA=FA+1.0D 1*(X1+X2+X2+X3+X4+X4-X(1)-X(4)-1.0D 1)
      RETURN
   24 FA=FA+1.0D 1*(X1+X2+X3+X5-X(2)-X(4)-5.0D 0)
      RETURN
  170 X1=X(1)-(X(4)+1.0D 0)**4
      X2=X1*X1
      X3=X(2)-X2*X2
      X4=X3*X3
      FA=X2+X4+2.0D 0*X(3)**2+X(4)**2-5.0D 0*(X1+X3)-2.1D 1*X(3)+
     * 7.0D 0*X(4)
      GO TO (171,172,173,174),KA
  171 CONTINUE
      RETURN
  172 FA=FA+1.0D 1*(X2+X4+X(3)**2+X(4)**2+X1-X3+X(3)-X(4)-8.0D 0)
      RETURN
  173 FA=FA+1.0D 1*(X2+2.0D 0*X4+X(3)**2+2.0D 0*X(4)**2-X1-X(4)-
     * 1.0D 1)
      RETURN
  174 FA=FA+1.0D 1*(X2+X4+X(3)**2+2.0D 0*X1-X3-X(4)-5.0D 0)
      RETURN
  120 T=Y(KA)
      FA=(X(3)/X(2))*EXP(-X(1)*T)*SIN(X(2)*T)-Y(KA+21)
      RETURN
   90 FA=Y(KA)-X(1)-DBLE(KA)/(DBLE(16-KA)*X(2)+DBLE(MIN(KA,16-KA))*
     & X(3))
      RETURN
  230 T=Y(KA+11)
      FA=Y(KA)-X(1)*T*(T+X(2))/((T+X(3))*T+X(4))
      RETURN
  210 T=0.2D 0*DBLE(KA)
      FA=(X(1)+X(2)*T-EXP(T))**2+(X(3)+X(4)*SIN(T)-COS(T))**2
      RETURN
  140 T=Y(KA)
      FA=X(4)-((X(1)*T+X(2))*T+X(3))**2-Y(KA+21)
      RETURN
  150 T=Y(KA)
      FA=X(1)*EXP(X(3)*T)+X(2)*EXP(X(4)*T)-Y(KA+21)
      RETURN
  200 T=Y(KA)
      FA=X(1)*ABS((T+X(2)+1.0D 0/(X(3)*T+X(4)))/((T+1.0D 0)*Y(61+KA)))
     & **(T+5.0D-1)-1.0D 0
      RETURN
   30 T=0.1D 0*DBLE(KA-1)-1.0D 0
      X1=X(1)+T*X(2)
      X2=1.0D 0/(1.0D 0+T*(X(3)+T*(X(4)+T*X(5))))
      X3=X1*X2-EXP(T)
      FA=X3
   31 RETURN
  130 T=Y(KA)
      FA=(X(1)+T*(X(2)+T*X(3)))/(1.0D 0+T*(X(4)+T*X(5)))-Y(KA+30)
      RETURN
  100 T=0.1D 0*DBLE(KA-1)
      FA=X(1)*EXP(-X(2)*T)*COS(X(3)*T+X(4))+X(5)*EXP(-X(6)*T)-Y(KA)
      RETURN
   40 BETA=0.5D 0*PI*Y(KA)
      DO 41 I=1,3
      J=I+I
      XA(I)=X(J-1)
      XB(I)=X(J)
   41 CONTINUE
      CA(4)=CMPLX(1.0D 0,0.0D 0)
      CB(4)=1.0D 1*CA(4)
      DO 42 J=1,3
      I=4-J
      XC(I)=BETA*XA(I)
      T=XC(I)
      X1=COS(T)
      X2=SIN(T)
      C1=CMPLX(X1,0.0D 0)
      C2=CMPLX(0.0D 0,(X2*XB(I)))
      C3=CMPLX(0.0D 0,(X2/XB(I)))
      CB(I)=C1*CB(I+1)+C2*CA(I+1)
      CA(I)=C3*CB(I+1)+C1*CA(I+1)
   42 CONTINUE
      C1=-CA(1)
      C2=CB(1)-C1
      C3=1.0D 0+2.0D 0*C1/C2
      FA=CDABS(C3)
      RETURN
  110 T=Y(41+KA)
      BETA=Y(82+KA)
      X1=(X(1)+(1D 0+X(2))*T)**2+((1D 0-X(2))*BETA)**2
      X2=(X(3)+(1D 0+X(4))*T)**2+((1D 0-X(4))*BETA)**2
      X3=(X(5)+(1D 0+X(6))*T)**2+((1D 0-X(6))*BETA)**2
      X4=(X(7)+(1D 0+X(8))*T)**2+((1D 0-X(8))*BETA)**2
      IF(X2.EQ.0D 0)X2=1D-30
      IF(X4.EQ.0D 0)X4=1D-30
      FA=X(9)*SQRT(X1/X2)*SQRT(X3/X4)-ABS(1D 0-2D 0*Y(KA))
      RETURN
   50 FA=(X(1)-1.0D 1)**2+5.0D 0*(X(2)-1.2D 1)**2+X(3)**4+3.0D 0*
     &(X(4)-1.1D 1)**2+1.0D 1*X(5)**6+7.0D 0*X(6)**2+X(7)**4-4.0D 0*
     &X(6)*X(7)-1.0D 1*X(6)-8.0D 0*X(7)
   51 GO TO (31,52,53,54,55),KA
   52 FA=FA+1.0D 1*(2.0D 0*X(1)**2+3.0D 0*X(2)**4+X(3)+
     &4.0D 0*X(4)**2+5.0D 0*X(5)-1.27D 2)
      RETURN
   53 FA=FA+1.0D 1*(7.0D 0*X(1)+3.0D 0*X(2)+1.0D 1*X(3)**2+X(4)-
     &X(5)-2.82D 2)
      RETURN
   54 FA=FA+1.0D 1*(2.3D 1*X(1)+X(2)**2+6.0D 0*X(6)**2-8.0D 0*X(7)-
     &1.96D 2)
      RETURN
   55 FA=FA+1.0D 1*(4.0D 0*X(1)**2+X(2)**2-3.0D 0*X(1)*X(2)+2.0D 0*
     &X(3)**2+5.0D 0*X(6)-1.1D 1*X(7))
      RETURN
   60 FA=X(1)**2+X(2)**2+X(1)*X(2)-1.4D 1*X(1)-1.6D 1*X(2)+
     &(X(3)-1.0D 1)**2+4.0D 0*(X(4)-5.0D 0)**2+(X(5)-3.0D 0)**2+
     &2.0D 0*(X(6)-1.0D 0)**2+5.0D 0*X(7)**2+7.0D 0*(X(8)-
     &1.1D 1)**2+2.0D 0*(X(9)-1.0D 1)**2+(X(10)-7.0D 0)**2+4.5D 1
   61 GO TO (31,62,63,64,65,66,67,68,69),KA
   62 FA=FA+1.0D 1*(3.0D 0*(X(1)-2.0D 0)**2+4.0D 0*(X(2)-
     &3.0D 0)**2+2.0D 0*X(3)**2-7.0D 0*X(4)-1.2D 2)
      RETURN
   63 FA=FA+1.0D 1*(5.0D 0*X(1)**2+8.0D 0*X(2)+(X(3)-6.0D 0)**2-
     &2.0D 0*X(4)-4.0D 1)
      RETURN
   64 FA=FA+1.0D 1*(0.5D 0*(X(1)-8.0D 0)**2+2.0D 0*(X(2)-
     &4.0D 0)**2+3.0D 0*X(5)**2-X(6)-3.0D 1)
      RETURN
   65 FA=FA+1.0D 1*(X(1)**2+2.0D 0*(X(2)-2.0D 0)**2-
     &2.0D 0*X(1)*X(2)+1.4D 1*X(5)-6.0D 0*X(6))
      RETURN
   66 FA=FA+1.0D 1*(4.0D 0*X(1)+5.0D 0*X(2)-3.0D 0*X(7)+
     &9.0D 0*X(8)-1.05D 2)
      RETURN
   67 FA=FA+1.0D 1*(1.0D 1*X(1)-8.0D 0*X(2)-1.7D 1*X(7)+
     &2.0D 0*X(8))
      RETURN
   68 FA=FA+1.0D 1*(6.0D 0*X(2)-3.0D 0*X(1)+1.2D 1*(X(9)-
     &8.0D 0)**2-7.0D 0*X(10))
      RETURN
   69 FA=FA+1.0D 1*(2.0D 0*X(2)-8.0D 0*X(1)+5.0D 0*X(9)-
     &2.0D 0*X(10)-1.2D 1)
      RETURN
   70 FA=X(1)**2+X(2)**2+X(1)*X(2)-1.4D 1*X(1)-1.6D 1*X(2)+(X(3)-
     &1.0D 1)**2+4.0D 0*(X(4)-5.0D 0)**2+(X(5)-3.0D 0)**2+2.0D 0*
     &(X(6)-1.0D 0)**2+5.0D 0*X(7)**2+7.0D 0*(X(8)-1.1D 1)**2+
     &2.0D 0*(X(9)-1.0D 1)**2+(X(10)-7.0D 0)**2+(X(11)-9.0D 0)**2+
     &1.0D 1*(X(12)-1.0D 0)**2+5.0D 0*(X(13)-7.0D 0)**2+4.0D 0*
     &(X(14)-1.4D 1)**2+2.7D 1*(X(15)-1.0D 0)**2+X(16)**4+(X(17)-
     &2.0D 0)**2+1.3D 1*(X(18)-2.0D 0)**2+(X(19)-3.D 0)**2+X(20)**2+
     &9.5D 1
   71 GO TO (31,62,63,64,65,66,67,68,69,72,73,74,75,76,77,78,79,89),KA
   72 FA=FA+1.0D 1*(X(1)+X(2)+4.0D 0*X(11)-2.1D 1*X(12))
      RETURN
   73 FA=FA+1.0D 1*(X(1)**2+1.5D 1*X(11)-8.0D 0*X(12)-2.8D 1)
      RETURN
   74 FA=FA+1.0D 1*(4.0D 0*X(1)+9.0D 0*X(2)+5.0D 0*X(13)**2-9.0D 0*
     &X(14)-8.7D 1)
      RETURN
   75 FA=FA+1.0D 1*(3.0D 0*X(1)+4.0D 0*X(2)+3.0D 0*(X(13)-
     16.0D 0)**2-1.4D 1*X(14)-1.0D 1)
      RETURN
   76 FA=FA+1.0D 1*(1.4D 1*X(1)**2+3.5D 1*X(15)-7.9D 1*X(16)-
     &9.2D 1)
      RETURN
   77 FA=FA+1.0D 1*(1.5D 1*X(2)**2+1.1D 1*X(15)-6.1D 1*X(16)-
     &5.4D 1)
      RETURN
   78 FA=FA+1.0D 1*(5.0D 0*X(1)**2+2.0D 0*X(2)+9.0D 0*X(17)**4-
     &X(18)-6.8D 1)
      RETURN
   79 FA=FA+1.0D 1*(X(1)**2-X(2)+1.9D 1*X(19)-2.0D 1*X(20)+1.9D 1)
      RETURN
   89 FA=FA+1.0D 1*(7.0D 0*X(1)**2+5.0D 0*X(2)**2+X(19)**2-3.0D 1*
     &X(20))
      RETURN
  220 X1=0.0D 0
      DO 221 I=1,N
      X3=1.0D 0
      X4=X(I)
      IF (I.EQ.1) X3=1.0D-8
      IF (I.EQ.4) X3=4.0D 0
      IF (I.EQ.2.AND.KA.EQ.1) X4=X(I)+2.0D 0
      IF (I.EQ.2.AND.KA.EQ.2) X4=X(I)-2.0D 0
      X1=X1+X3*X4**2
  221 CONTINUE
      FA=EXP(X1)
      RETURN
  160 FA=0.0D 0
      DO 161 I=1,N
      X1=1.0D 0*DBLE(I+KA-1)
      X2=X(I)-SIN(DBLE(2*I+KA-3))
      FA=FA+X1*EXP(X2**2)
  161 CONTINUE
      RETURN
  240 IF (KA.EQ.1) THEN
      FA=X(1)
      ELSE IF (KA.EQ.2) THEN
      FA=X(2)-X(1)**2-1.0D 0
      ELSE
      T=DBLE(KA-2)/2.9D 1
      X1=0.0D 0
      X2=X(1)
      DO 241 I=2,N
      X1=X1+DBLE(I-1)*X(I)*T**(I-2)
      X2=X2+X(I)*T**(I-1)
  241 CONTINUE
      FA=X1-X2**2-1.0D 0
      ENDIF
      RETURN
  250 T=1.0D-1*DBLE(KA-1)
      FA=Y(KA)-X(1)*EXP(-X(5)*T)-X(2)*EXP(-X(6)*(T-X(9))**2)-
     & X(3)*EXP(-X(7)*(T-X(10))**2)-X(4)*EXP(-X(8)*(T-X(11))**2)
      RETURN
      END
* SUBROUTINE TAGU06             ALL SYSTEMS                 99/12/01
C PORTABILITY : ALL SYSTEMS
C 90/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
*  GRADIENTS OF PARTIAL FUNCTIONS IN THE MINIMAX CRITERION.
*
* PARAMETERS :
*  II  N  NUMBER OF VARIABLES.
*  II  KA  INDEX OF THE PARTIAL FUNCTION.
*  RI  X(N)  VECTOR OF VARIABLES.
*  RO  GA(N)  GRADIENT OF THE PARTIAL FUNCTION AT THE
*          SELECTED POINT.
*  II  NEXT  NUMBER OF THE TEST PROBLEM.
*
      SUBROUTINE TAGU06(N,KA,X,GA,NEXT)
      INTEGER N,KA,NEXT
      REAL*8 X(N),GA(N)
      REAL*8 FA,T,X1,X2,X3,X4,X5,X6,X7,X8
      COMPLEX*16 CA(4),CB(4),CC(6),C1,C2,C3
      REAL*8 XA(3),XB(3),XC(3),BETA
      INTEGER I,J
      REAL*8 Y(123),PI
      COMMON /EMPR06/ Y
      PARAMETER (PI=3.14159265358979323846D 0)
      GOTO(10,190,180,80,20,170,120,90,230,210,140,150,200,30,130,100,
     & 40,110,50,60,70,220,160,240,250),NEXT
   10 X1=X(1)*X(1)
      X2=X(2)*X(2)
      X3=X(1)+X(1)
      X4=X(2)+X(2)
      GO TO (11,12,13),KA
   11 GA(1)=X3
      GA(2)=(X2+X2)*X4
      RETURN
   12 GA(1)=-4.0D 0+X3
      GA(2)=-4.0D 0+X4
      RETURN
   13 FA=2.0D 0*EXP(X(2)-X(1))
      GA(1)=-FA
      GA(2)=+FA
      RETURN
  190 X1=1.0D 0/(X(1)+1.0D-1)**2
      GA(2)=2.0D 0*X(2)
      IF (KA.EQ.1) THEN
      GA(1)=0.5D 0*( 1.0D 0+X1)
      ELSE IF (KA.EQ.2) THEN
      GA(1)=0.5D 0*(-1.0D 0+X1)
      ELSE IF (KA.EQ.3) THEN
      GA(1)=0.5D 0*( 1.0D 0-X1)
      ENDIF
      RETURN
  180 X1=X(1)**2+X(2)**2
      X2=SQRT(X1)
      X3=COS(X2)
      X4=SIN(X2)
      IF (KA.EQ.1) THEN
      X5=2.0D 0*(X(1)-X2*X3)
      X6=-(X3/X2-X4)
      GA(1)=X5*(X(1)*X6+1.0D 0)+1.0D-2*X(1)
      GA(2)=X5*X(2)*X6+1.0D-2*X(2)
      ELSE IF (KA.EQ.2) THEN
      X5=2.0D 0*(X(2)-X2*X4)
      X6=-(X4/X2+X3)
      GA(1)=X5*X(1)*X6+1.0D-2*X(1)
      GA(2)=X5*(X(2)*X6+1.0D 0)+1.0D-2*X(2)
      ENDIF
      RETURN
   80 GO TO (81,82,83,84,85,86),KA
   81 GA(1)=2.0D 0*X(1)
      GA(2)=2.0D 0*X(2)
      GA(3)=2.0D 0*X(3)
      RETURN
   82 GA(1)=2.0D 0*X(1)
      GA(2)=2.0D 0*X(2)
      GA(3)=2.0D 0*(X(3)-2.0D 0)
      RETURN
   83 GA(1)=1.0D 0
      GA(2)=1.0D 0
      GA(3)=1.0D 0
      RETURN
   84 GA(1)= 1.0D 0
      GA(2)= 1.0D 0
      GA(3)=-1.0D 0
      RETURN
   85 GA(1)=6.0D 0*X(1)**2-4.0D 0*(5.0D 0*X(3)-X(1)+1.0D 0)
      GA(2)=1.2D 1*X(2)
      GA(3)=2.0D 1*(5.0D 0*X(3)-X(1)+1.0D 0)
      RETURN
   86 GA(1)= 2.0D 0*X(1)
      GA(2)= 0.0D 0
      GA(3)=-9.0D 0
      RETURN
   20 X1=X(1)*X(1)
      X2=X(2)*X(2)
      X3=X(3)*X(3)
      X4=X(4)*X(4)
      X5=X(1)+X(1)
      X6=X(2)+X(2)
      X7=X(3)+X(3)
      X8=X(4)+X(4)
      GA(1)=X5-5.0D 0
      GA(2)=X6-5.0D 0
      GA(3)=X7+X7-2.1D 1
      GA(4)=X8+7.0D 0
   21 GO TO (31,22,23,24),KA
   22 GA(1)=GA(1)+1.0D 1*(X5+1.0D 0)
      GA(2)=GA(2)+1.0D 1*(X6-1.0D 0)
      GA(3)=GA(3)+1.0D 1*(X7+1.0D 0)
      GA(4)=GA(4)+1.0D 1*(X8-1.0D 0)
      RETURN
   23 GA(1)=GA(1)+1.0D 1*(X5-1.0D 0)
      GA(2)=GA(2)+1.0D 1*(X6+X6)
      GA(3)=GA(3)+1.0D 1*X7
      GA(4)=GA(4)+1.0D 1*(X8+X8-1.0D 0)
      RETURN
   24 GA(1)=GA(1)+1.0D 1*(X5+2.0D 0)
      GA(2)=GA(2)+1.0D 1*(X6-1.0D 0)
      GA(3)=GA(3)+1.0D 1*X7
      GA(4)=GA(4)-1.0D 1
      RETURN
  170 X1=X(1)-(X(4)+1.0D 0)**4
      X2=X1*X1
      X3=X(2)-X2*X2
      X4=X1*X3
      X5=-4.0D 0*(X(4)+1.0D 0)**3
      GA(1)=2.0D 0*X1-8.0D 0*X4-5.0D 0*(1.0D 0-4.0D 0*X1)
      GA(2)=2.0D 0*X3-5.0D 0
      GA(3)=4.0D 0*X(3)-2.1D 1
      GA(4)=2.0D 0*X1*X5-8.0D 0*X4*X5+2.0D 0*X(4)-
     & 5.0D 0*(X5-4.0D 0*X1*X5)+7.0D 0
      GO TO (171,172,173,174),KA
  171 CONTINUE
      RETURN
  172 GA(1)=GA(1)+1.0D 1*(2.0D 0*X1-8.0D 0*X4+1.0D 0+4.0D 0*X1)
      GA(2)=GA(2)+1.0D 1*(2.0D 0*X3-1.0D 0)
      GA(3)=GA(3)+1.0D 1*(2.0D 0*X(3)+1.0D 0)
      GA(4)=GA(4)+1.0D 1*(2.0D 0*X1*X5-8.0D 0*X4*X5+2.0D 0*X(4)+
     & X5+4.0D 0*X1*X5-1.0D 0)
      RETURN
  173 GA(1)=GA(1)+1.0D 1*(2.0D 0*X1-1.6D 1*X4-1.0D 0)
      GA(2)=GA(2)+1.0D 1*(4.0D 0*X3)
      GA(3)=GA(3)+1.0D 1*(2.0D 0*X(3))
      GA(4)=GA(4)+1.0D 1*(2.0D 0*X1*X5-1.6D 1*X4*X5+4.0D 0*X(4)-X5-
     & 1.0D 0)
      RETURN
  174 GA(1)=GA(1)+1.0D 1*(2.0D 0*X1-8.0D 0*X4+2.0D 0+4.0D 0*X1)
      GA(2)=GA(2)+1.0D 1*(2.0D 0*X3-1.0D 0)
      GA(3)=GA(3)+1.0D 1*(2.0D 0*X(3))
      GA(4)=GA(4)+1.0D 1*(2.0D 0*X1*X5-8.0D 0*X4*X5+
     & 2.0D 0*X5+4.0D 0*X1*X5-1.0D 0)
      RETURN
  120 T=Y(KA)
      X1=EXP(-X(1)*T)/X(2)
      X2=SIN( X(2)*T)
      X3=COS( X(2)*T)
      GA(1)=-T*X(3)*X1*X2
      GA(2)= X(3)*X1*(T*X3-X2/X(2))
      GA(3)= X1*X2
      RETURN
   90 C1=DBLE(16-KA)
      C2=DBLE(MIN(KA,16-KA))
      C3=DBLE(KA)/(C1*X(2)+C2*X(3))**2
      GA(1)=-1.0D 0
      GA(2)=C1*C3
      GA(3)=C2*C3
      RETURN
  230 T=Y(KA+11)
      X1=X(1)*T*(T+X(2))
      X2=((T+X(3))*T+X(4))
      X3=X1/X2**2
      GA(1)=-T*(T+X(2))/X2
      GA(2)=-T*X(1)/X2
      GA(3)=T*X3
      GA(4)=X3
      RETURN
  210 T=0.2D 0*DBLE(KA)
      GA(1)=2.0D 0*(X(1)+X(2)*T-EXP(T))
      GA(2)=2.0D 0*(X(1)+X(2)*T-EXP(T))*T
      GA(3)=2.0D 0*(X(3)+X(4)*SIN(T)-COS(T))
      GA(4)=2.0D 0*(X(3)+X(4)*SIN(T)-COS(T))*SIN(T)
      RETURN
  140 T=Y(KA)
      X1=-2.0D 0*((X(1)*T+X(2))*T+X(3))
      GA(1)=X1*T**2
      GA(2)=X1*T
      GA(3)=X1
      GA(4)=1.0D 0
      RETURN
  150 T=Y(KA)
      X1=EXP(X(3)*T)
      X2=EXP(X(4)*T)
      GA(1)=X1
      GA(2)=X2
      GA(3)=X(1)*T*X1
      GA(4)=X(2)*T*X2
      RETURN
  200 T=Y(KA)
      X1=T+X(2)+1.0D 0/(X(3)*T+X(4))
      IF(X1.EQ.0D 0)X1=1D-30
      X2=X1/((T+1.0D 0)*Y(61+KA))
      GA(1)=ABS(X2)**(T+5.0D-1)
      GA(2)=X(1)*GA(1)*(T+5.0D-1)/X1
      GA(4)=-GA(2)/(X(3)*T+X(4))**2
      GA(3)=GA(4)*T
      RETURN
   30 T=0.1D 0*DBLE(KA-1)-1.0D 0
      X1=X(1)+T*X(2)
      X2=1.0D 0/(1.0D 0+T*(X(3)+T*(X(4)+T*X(5))))
      X3=X1*X2-EXP(T)
      GA(1)=X2
      GA(2)=X2*T
      GA(3)=-X1*X2*X2*T
      GA(4)=GA(3)*T
      GA(5)=GA(4)*T
   31 RETURN
  130 T=Y(KA)
      X1=1.0D 0/(1.0D 0+T*(X(4)+T*X(5)))
      X2=X(1)+T*(X(2)+T*X(3))
      GA(1)= X1
      GA(2)= X1*T
      GA(3)= X1*T*T
      GA(4)=-X2*X1**2*T
      GA(5)=-X2*X1**2*T*T
      RETURN
  100 T=0.1D 0*DBLE(KA-1)
      X1=EXP(-X(2)*T)
      X2=COS(X(3)*T+X(4))
      X3=SIN(X(3)*T+X(4))
      X4=EXP(-X(6)*T)
      GA(1)= X1*X2
      GA(2)=-X1*X2*X(1)*T
      GA(3)=-X1*X3*X(1)*T
      GA(4)=-X1*X3*X(1)
      GA(5)= X4
      GA(6)=-X4*X(5)*T
      RETURN
   40 BETA=0.5D 0*PI*Y(KA)
      DO 41 I=1,3
      J=I+I
      XA(I)=X(J-1)
      XB(I)=X(J)
   41 CONTINUE
      CA(4)=CMPLX(1.0D 0,0.0D 0)
      CB(4)=1.0D 1*CA(4)
      DO 42 J=1,3
      I=4-J
      XC(I)=BETA*XA(I)
      T=XC(I)
      X1=COS(T)
      X2=SIN(T)
      C1=CMPLX(X1,0.0D 0)
      C2=CMPLX(0.0D 0,(X2*XB(I)))
      C3=CMPLX(0.0D 0,(X2/XB(I)))
      CB(I)=C1*CB(I+1)+C2*CA(I+1)
      CA(I)=C3*CB(I+1)+C1*CA(I+1)
   42 CONTINUE
      C1=-CA(1)
      C2=CB(1)-C1
      C3=1.0D 0+2.0D 0*C1/C2
      FA=CDABS(C3)
      C3=CONJG(C3)
      C1=2.0D 0/C2
      DO 43 I=1,3
      T=XC(I)
      J=I+I
      CC(J)=(CB(I)*CA(I)-CB(I+1)*CA(I+1))/(C2*XB(I))
      CC(J-1)=BETA*(CB(I)*CA(I+1)-CB(I+1)*CA(I))/(C2*SIN(T))
   43 CONTINUE
      DO 44 I=1,6
      GA(I)=DBLE(C1*C3*CC(I))/FA
   44 CONTINUE
      RETURN
  110 T=Y(41+KA)
      BETA=Y(82+KA)
      X1=(X(1)+(1D 0+X(2))*T)**2+((1D 0-X(2))*BETA)**2
      X2=(X(3)+(1D 0+X(4))*T)**2+((1D 0-X(4))*BETA)**2
      X3=(X(5)+(1D 0+X(6))*T)**2+((1D 0-X(6))*BETA)**2
      X4=(X(7)+(1D 0+X(8))*T)**2+((1D 0-X(8))*BETA)**2
      IF(X1.EQ.0D 0)X1=1D-30
      IF(X2.EQ.0D 0)X2=1D-30
      IF(X3.EQ.0D 0)X3=1D-30
      IF(X4.EQ.0D 0)X4=1D-30
      FA=SQRT(X1/X2)*SQRT(X3/X4)
      GA(9)=FA
      FA=X(9)*FA
      GA(1)=FA/X1*(X(1)+T*(1D 0+X(2)))
      GA(2)=FA/X1*(X(2)+2D 0*T*T-1D 0+X(1)*T)
      GA(3)=-FA/X2*(X(3)+T*(1D 0+X(4)))
      GA(4)=-FA/X2*(X(4)+2D 0*T*T-1D 0+X(3)*T)
      GA(5)=FA/X3*(X(5)+T*(1D 0+X(6)))
      GA(6)=FA/X3*(X(6)+2D 0*T*T-1D 0+X(5)*T)
      GA(7)=-FA/X4*(X(7)+T*(1D 0+X(8)))
      GA(8)=-FA/X4*(X(8)+2D 0*T*T-1D 0+X(7)*T)
      RETURN
   50 GA(1)=2.0D 0*(X(1)-1.0D 1)
      GA(2)=1.0D 1*(X(2)-1.2D 1)
      GA(3)=4.0D 0*X(3)**3
      GA(4)=6.0D 0*(X(4)-1.1D 1)
      GA(5)=6.0D 1*X(5)**5
      GA(6)=1.4D 1*X(6)-4.0D 0*X(7)-1.0D 1
      GA(7)=4.0D 0*X(7)**3-4.0D 0*X(6)-8.0D 0
   51 GO TO (31,52,53,54,55),KA
   52 GA(1)=GA(1)+4.0D 1*X(1)
      GA(2)=GA(2)+1.2D 2*X(2)**3
      GA(3)=GA(3)+1.0D 1
      GA(4)=GA(4)+8.0D 1*X(4)
      GA(5)=GA(5)+5.0D 1
      RETURN
   53 GA(1)=GA(1)+7.0D 1
      GA(2)=GA(2)+3.0D 1
      GA(3)=GA(3)+2.0D 2*X(3)
      GA(4)=GA(4)+1.0D 1
      GA(5)=GA(5)-1.0D 1
      RETURN
   54 GA(1)=GA(1)+2.3D 2
      GA(2)=GA(2)+2.0D 1*X(2)
      GA(6)=GA(6)+1.2D 2*X(6)
      GA(7)=GA(7)-8.0D 1
      RETURN
   55 GA(1)=GA(1)+8.0D 1*X(1)-3.0D 1*X(2)
      GA(2)=GA(2)+2.0D 1*X(2)-3.0D 1*X(1)
      GA(3)=GA(3)+4.0D 1*X(3)
      GA(6)=GA(6)+5.0D 1
      GA(7)=GA(7)-1.1D 2
      RETURN
   60 GA(1)=2.0D 0*X(1)+X(2)-1.4D 1
      GA(2)=2.0D 0*X(2)+X(1)-1.6D 1
      GA(3)=2.0D 0*(X(3)-1.0D 1)
      GA(4)=8.0D 0*(X(4)-5.0D 0)
      GA(5)=2.0D 0*(X(5)-3.0D 0)
      GA(6)=4.0D 0*(X(6)-1.0D 0)
      GA(7)=1.0D 1*X(7)
      GA(8)=1.4D 1*(X(8)-1.1D 1)
      GA(9)=4.0D 0*(X(9)-1.0D 1)
      GA(10)=2.0D 0*(X(10)-7.0D 0)
   61 GO TO (31,62,63,64,65,66,67,68,69),KA
   62 GA(1)=GA(1)+6.0D 1*(X(1)-2.0D 0)
      GA(2)=GA(2)+8.0D 1*(X(2)-3.0D 0)
      GA(3)=GA(3)+4.0D 1*X(3)
      GA(4)=GA(4)-7.0D 1
      RETURN
   63 GA(1)=GA(1)+1.0D 2*X(1)
      GA(2)=GA(2)+8.0D 1
      GA(3)=GA(3)+2.0D 1*(X(3)-6.0D 0)
      GA(4)=GA(4)-2.0D 1
      RETURN
   64 GA(1)=GA(1)+1.0D 1*(X(1)-8.0D 0)
      GA(2)=GA(2)+4.0D 1*(X(2)-4.0D 0)
      GA(5)=GA(5)+6.0D 1*X(5)
      GA(6)=GA(6)-1.0D 1
      RETURN
   65 GA(1)=GA(1)+2.0D 1*X(1)-2.0D 1*X(2)
      GA(2)=GA(2)+4.0D 1*(X(2)-2.0D 0)-2.0D 1*X(1)
      GA(5)=GA(5)+1.4D 2
      GA(6)=GA(6)-6.0D 1
      RETURN
   66 GA(1)=GA(1)+4.0D 1
      GA(2)=GA(2)+5.0D 1
      GA(7)=GA(7)-3.0D 1
      GA(8)=GA(8)+9.0D 1
      RETURN
   67 GA(1)=GA(1)+1.0D 2
      GA(2)=GA(2)-8.0D 1
      GA(7)=GA(7)-1.7D 2
      GA(8)=GA(8)+2.0D 1
      RETURN
   68 GA(1)=GA(1)-3.0D 1
      GA(2)=GA(2)+6.0D 1
      GA(9)=GA(9)+2.4D 2*(X(9)-8.0D 0)
      GA(10)=GA(10)-7.0D 1
      RETURN
   69 GA(1)=GA(1)-8.0D 1
      GA(2)=GA(2)+2.0D 1
      GA(9)=GA(9)+5.0D 1
      GA(10)=GA(10)-2.0D 1
      RETURN
   70 GA(1)=2.0D 0*X(1)+X(2)-1.4D 1
      GA(2)=2.0D 0*X(2)+X(1)-1.6D 1
      GA(3)=2.0D 0*(X(3)-1.0D 1)
      GA(4)=8.0D 0*(X(4)-5.0D 0)
      GA(5)=2.0D 0*(X(5)-3.0D 0)
      GA(6)=4.0D0*(X(6)-1.0D 0)
      GA(7)=1.0D 1*X(7)
      GA(8)=1.4D 1*(X(8)-1.1D 1)
      GA(9)=4.0D 0*(X(9)-1.0D 1)
      GA(10)=2.0D 0*(X(10)-7.0D0)
      GA(11)=2.0D 0*(X(11)-9.0D0)
      GA(12)=2.0D 1*(X(12)-1.0D 0)
      GA(13)=1.0D 1*(X(13)-7.0D 0)
      GA(14)=8.0D 0*(X(14)-1.4D 1)
      GA(15)=5.4D 1*(X(15)-1.0D 0)
      GA(16)=4.0D 0*X(16)**3
      GA(17)=2.0D 0*(X(17)-2.0D 0)
      GA(18)=2.6D 1*(X(18)-2.0D 0)
      GA(19)=2.0D 0*(X(19)-3.0D 0)
      GA(20)=2.0D 0*X(20)
   71 GO TO (31,62,63,64,65,66,67,68,69,72,73,74,75,76,77,78,79,89),KA
   72 GA(1)=GA(1)+1.0D 1
      GA(2)=GA(2)+1.0D 1
      GA(11)=GA(11)+4.0D 1
      GA(12)=GA(12)-2.1D 2
      RETURN
   73 GA(1)=GA(1)+2.0D 1*X(1)
      GA(11)=GA(11)+1.5D 2
      GA(12)=GA(12)-8.0D 1
      RETURN
   74 GA(1)=GA(1)+4.0D 1
      GA(2)=GA(2)+9.0D 1
      GA(13)=GA(13)+1.0D 2*X(13)
      GA(14)=GA(14)-9.0D 1
      RETURN
   75 GA(1)=GA(1)+3.0D 1
      GA(2)=GA(2)+4.0D 1
      GA(13)=GA(13)+6.0D 1*(X(13)-6.0D 0)
      GA(14)=GA(14)-1.4D 2
      RETURN
   76 GA(1)=GA(1)+2.8D 2*X(1)
      GA(15)=GA(15)+3.5D 2
      GA(16)=GA(16)-7.9D 2
      RETURN
   77 GA(2)=GA(2)+3.0D 2*X(2)
      GA(15)=GA(15)+1.1D 2
      GA(16)=GA(16)-6.1D 2
      RETURN
   78 GA(1)=GA(1)+1.0D 2*X(1)
      GA(2)=GA(2)+2.0D 1
      GA(17)=GA(17)+3.6D 2*X(17)**3
      GA(18)=GA(18)-1.0D 1
      RETURN
   79 GA(1)=GA(1)+2.0D 1*X(1)
      GA(2)=GA(2)-1.0D 1
      GA(19)=GA(19)+1.9D 2
      GA(20)=GA(20)-2.0D 2
      RETURN
   89 GA(1)=GA(1)+1.4D 2*X(1)
      GA(2)=GA(2)+1.0D 2*X(2)
      GA(19)=GA(19)+2.0D 1*X(19)
      GA(20)=GA(20)-3.0D 2
      RETURN
  220 X1=0.0D 0
      DO 221 I=1,N
      X3=1.0D 0
      X4=X(I)
      IF (I.EQ.1) X3=1.0D-8
      IF (I.EQ.4) X3=4.0D 0
      IF (I.EQ.2.AND.KA.EQ.1) X4=X(I)+2.0D 0
      IF (I.EQ.2.AND.KA.EQ.2) X4=X(I)-2.0D 0
      X1=X1+X3*X4**2
  221 CONTINUE
      X2=EXP(X1)
      DO 222 I=1,N
      X3=2.0D 0
      X4=X(I)
      IF (I.EQ.1) X3=2.0D-8
      IF (I.EQ.4) X3=8.0D 0
      IF (I.EQ.2.AND.KA.EQ.1) X4=X(I)+2.0D 0
      IF (I.EQ.2.AND.KA.EQ.2) X4=X(I)-2.0D 0
      GA(I)=X2*X3*X4
  222 CONTINUE
      RETURN
  160 DO 161 I=1,N
      X1=1.0D 0*DBLE(I+KA-1)
      X2=X(I)-SIN(DBLE(2*I+KA-3))
      GA(I)=2.0D 0*X1*X2*EXP(X2**2)
  161 CONTINUE
      RETURN
  240 IF (KA.LE.2) THEN
      DO 241 I=2,N
      GA(I)=0.0D 0
  241 CONTINUE
      IF (KA.EQ.1) THEN
      GA(1)=1.0D 0
      ELSE IF (KA.EQ.2) THEN
      GA(1)=-2.0D 0*X(1)
      GA(2)= 1.0D 0
      ENDIF
      ELSE
      GA(1)=0.0D 0
      T=DBLE(KA-2)/2.9D 1
      X2=X(1)
      DO 242 I=2,N
      X2=X2+X(I)*T**(I-1)
  242 CONTINUE
      DO 243 I=1,N
      IF (I.GT.1) GA(I)=DBLE(I-1)*T**(I-2)
      GA(I)=GA(I)-2.0D 0*X2*T**(I-1)
  243 CONTINUE
      ENDIF
      RETURN
  250 T=1.0D-1*DBLE(KA-1)
      X1=EXP(-X(5)*T)
      X2=EXP(-X(6)*(T-X( 9))**2)
      X3=EXP(-X(7)*(T-X(10))**2)
      X4=EXP(-X(8)*(T-X(11))**2)
      GA(1)= -X1
      GA(2)= -X2
      GA(3)= -X3
      GA(4)= -X4
      GA(5)=  X1*X(1)*T
      GA(6)=  X2*X(2)*(T-X( 9))**2
      GA(7)=  X3*X(3)*(T-X(10))**2
      GA(8)=  X4*X(4)*(T-X(11))**2
      GA(9)= -2.0D 0*X2*X(2)*X(6)*(T-X( 9))
      GA(10)=-2.0D 0*X3*X(3)*X(7)*(T-X(10))
      GA(11)=-2.0D 0*X4*X(4)*X(8)*(T-X(11))
      RETURN
      END
* SUBROUTINE TAHD06             ALL SYSTEMS                 99/12/01
C PORTABILITY : ALL SYSTEMS
C 95/12/01 LU : ORIGINAL VERSION
*
* PURPOSE :
*  HESSIAN MATRICES OF PARTIAL FUNCTIONS IN THE MINIMAX CRITERION.
*  DENSE VERSION.
*
* PARAMETERS :
*  II  N  NUMBER OF VARIABLES.
*  II  KA  INDEX OF THE PARTIAL FUNCTION.
*  RI  X(N)  VECTOR OF VARIABLES.
*  RO  HA(N*(N+1)/2)  HESSIAN MATRIX OF THE PARTIAL FUNCTION
*         AT THE SELECTED POINT.
*  II  NEXT  NUMBER OF THE TEST PROBLEM.
*
      SUBROUTINE TAHD06(N,KA,X,HA,NEXT)
      INTEGER N,KA,NEXT
      REAL*8 X(N),HA(N*(N+1)/2)
      REAL*8 T,X1,X2,X3,X4,X5,X6,X7,X8
      COMPLEX*16 CA(4),CB(4),CC(6),DD(6),C1,C2,C3,CI,S1,S2,S3,S4
      REAL*8 GA(8),CT(3),ST(3),XA(3),XB(3),XC(3),BETA,FA
      INTEGER I,J,L
      REAL*8 Y(123),PI
      COMMON /EMPR06/ Y
      PARAMETER (PI=3.14159265358979323846D 0)
      GOTO(10,190,180,80,20,170,120,90,230,210,140,150,200,30,130,100,
     & 40,110,50,60,70,220,160,240,250),NEXT
   10 HA(1)=2.0D 0
      HA(2)=0.0D 0
      HA(3)=2.0D 0
      IF(KA.EQ.1) HA(3)=12.0D 0*X(2)*X(2)
      IF(KA.EQ.3) THEN
      HA(1)=2.0D 0*EXP(X(2)-X(1))
      HA(2)=-HA(1)
      HA(3)= HA(1)
      ENDIF
      RETURN
  190 CONTINUE
      HA(2)=0.0D 0
      HA(3)=2.0D 0
      X1=1.0D 0/(X(1)+1.0D-1)**3
      IF (KA.EQ.3) THEN
      HA(1)= X1
      ELSE
      HA(1)=-X1
      ENDIF
      RETURN
  180 X1=X(1)**2+X(2)**2
      X2=SQRT(X1)
      X3=COS(X2)
      X4=SIN(X2)
      IF (KA.EQ.1) THEN
      X5=X(1)-X2*X3
      X6=-(X3/X2-X4)
      X7=X2+1.0D 0/X2
      X7=(X7*X3+X4)/X2**2
      X1=1.0D 0+X6*X(1)
      X2=X6*X(2)
      HA(1)=2.0D 0*(X1**2+X5*(X6+X7*X(1)**2))+1.0D-2
      HA(2)=2.0D 0*X(2)*(X6*X1+X5*X7*X(1))
      HA(3)=2.0D 0*(X2**2+X5*(X6+X7*X(2)**2))+1.0D-2
      ELSE IF (KA.EQ.2) THEN
      X5=X(2)-X2*X4
      X6=-(X4/X2+X3)
      X7=X2+1.0D 0/X2
      X7=(X7*X4-X3)/X2**2
      X1=X6*X(1)
      X2=1.0D 0+X6*X(2)
      HA(1)=2.0D 0*(X1**2+X5*(X6+X7*X(1)**2))+1.0D-2
      HA(2)=2.0D 0*X(1)*(X6*X2+X5*X7*X(2))
      HA(3)=2.0D 0*(X2**2+X5*(X6+X7*X(2)**2))+1.0D-2
      ENDIF
      RETURN
   80 DO 81 I=1,N*(N+1)/2
      HA(I)=0.0D 0
   81 CONTINUE
      GO TO (82,82,83,83,85,86),KA
   82 HA(1)= 2.0D 0
      HA(3)= 2.0D 0
      HA(6)= 2.0D 0
      RETURN
   83 RETURN
   85 HA(1)= 1.6D 0
      HA(3)= 1.2D 1
      HA(4)=-2.0D 1
      HA(6)= 1.0D 2
      RETURN
   86 HA(1)= 2.0D 0
      RETURN
   20 DO 21 I=1,N*(N+1)/2
      HA(I)=0.0D 0
   21 CONTINUE
      HA(1)=2.0D 0
      HA(3)=2.0D 0
      HA(6)=4.0D 0
      HA(10)=2.0D 0
      IF (KA.GT.1) THEN
      HA(1)=HA(1)+2.0D 1
      HA(3)=HA(3)+2.0D 1
      HA(6)=HA(6)+2.0D 1
      HA(10)=HA(10)+2.0D 1
      GO TO (24,22,23),KA-1
   22 HA(3)=HA(3)+2.0D 1
      HA(10)=HA(10)+2.0D 1
      GO TO 24
   23 HA(10)=HA(10)-2.0D 1
   24 CONTINUE
      ENDIF
      RETURN
  170 X1=X(1)-(X(4)+1.0D 0)**4
      X2=X1*X1
      X3=X(2)-X2*X2
      X4=X3*X3
      X5=-4.0D 0*(X(4)+1.0D 0)**3
      X6=-1.2D 1*(X(4)+1.0D 0)**2
      X7=X1*X6+X5*X5
      X8=X3-4.0D 0*X1*X1
      HA(1)=2.0D 0-8.0D 0*X8+2.0D 1
      HA(2)=-8.0D 0*X1
      HA(3)=2.0D 0
      HA(4)=0.0D 0
      HA(5)=0.0D 0
      HA(6)=4.0D 0
      HA(7)=2.0D 0*X5-8.0D 0*X5*X8+2.0D 1*X5
      HA(8)=-8.0D 0*X1*X5
      HA(9)=0.0D 0
      HA(10)=2.0D 0*X7-8.0D 0*(X5*X5*X8+X1*X3*X6)-
     & 5.0D 0*(X6-4.0D 0*X7)+2.0D 0
      GO TO (171,172,173,174),KA
  171 CONTINUE
      RETURN
  172 CONTINUE
      HA(1)=HA(1)+1.0D 1*(2.0D 0-8.0D 0*X8+4.0D 0)
      HA(2)=HA(2)-8.0D 1*X1
      HA(3)=HA(3)+2.0D 1
      HA(6)=HA(6)+2.0D 1
      HA(7)=HA(7)+1.0D 1*(2.0D 0*X5-8.0D 0*X5*X8+4.0D 0*X5)
      HA(8)=HA(8)-8.0D 1*X1*X5
      HA(10)=HA(10)+1.0D 1*(2.0D 0*X7-8.0D 0*(X5*X5*X8+X1*X3*X6)+
     & X6+4.0D 0*X7+2.0D 0)
      RETURN
  173 CONTINUE
      HA(1)=HA(1)+1.0D 1*(2.0D 0-1.6D 1*X8)
      HA(2)=HA(2)-1.6D 2*X1
      HA(3)=HA(3)+4.0D 1
      HA(6)=HA(6)+2.0D 1
      HA(7)=HA(7)+1.0D 1*(2.0D 0*X5-1.6D 1*X5*X8)
      HA(8)=HA(8)-1.6D 2*X1*X5
      HA(10)=HA(10)+1.0D 1*(2.0D 0*X7-1.6D 1*(X5*X5*X8+X1*X3*X6)-
     & X6+4.0D 0)
      RETURN
  174 CONTINUE
      HA(1)=HA(1)+1.0D 1*(2.0D 0-8.0D 0*X8+4.0D 0)
      HA(2)=HA(2)-8.0D 1*X1
      HA(3)=HA(3)+2.0D 1
      HA(6)=HA(6)+2.0D 1
      HA(7)=HA(7)+1.0D 1*(2.0D 0*X5-8.0D 0*X5*X8+4.0D 0*X5)
      HA(8)=HA(8)-8.0D 1*X1*X5
      HA(10)=HA(10)+1.0D 1*(2.0D 0*X7-8.0D 0*(X5*X5*X8+X1*X3*X6)+
     & 2.0D 0*X6+4.0D 0*X7)
      RETURN
  120 T=Y(KA)
      X1=EXP(-X(1)*T)/X(2)
      X2=SIN( X(2)*T)
      X3=COS( X(2)*T)
      X4=T*X3-X2/X(2)
      HA(1)= X(3)*X1*X2*T**2
      HA(2)=-X(3)*X1*X4*T
      HA(3)=-X(3)*X1*(T**2*X2+2.0D 0*T*X3/X(2)-2.0D 0*X2/X(2)**2)
      HA(4)=-X1*X2*T
      HA(5)= X1*X4*T
      HA(6)= 0.0D 0
      RETURN
   90 DO 91 I=1,N*(N+1)/2
      HA(I)=0.0D 0
   91 CONTINUE
      C1=DBLE(16-KA)
      C2=DBLE(MIN(KA,16-KA))
      C3=-2.0D 0*DBLE(KA)/(C1*X(2)+C2*X(3))**3
      HA(3)=C1*C1*C3
      HA(5)=C1*C2*C3
      HA(6)=C2*C2*C3
      RETURN
  230 T=Y(KA+11)
      X1= X(1)*T*(T+X(2))
      X2= ((T+X(3))*T+X(4))
      X3= 1.0D 0/X2**2
      X4= T*(T+X(2))*X3
      X5=-2.0D 0*X1*X3/X2
      HA(1)= 0.0D 0
      HA(2)=-T/X2
      HA(3)= 0.0D 0
      HA(7)= X4
      HA(4)= T*X4
      HA(8)= T*X(1)*X3
      HA(5)= T*HA(8)
      HA(10)=X5
      HA(9)= T*X5
      HA(6)= T*HA(9)
      RETURN
  210 T=0.2D 0*DBLE(KA)
      HA(1)= 2.0D 0
      HA(2)= 2.0D 0*T
      HA(3)= 2.0D 0*T*T
      HA(4)= 0.0D 0
      HA(5)= 0.0D 0
      HA(6)= 2.0D 0
      HA(7)= 0.0D 0
      HA(8)= 0.0D 0
      HA(9)= 2.0D 0*SIN(T)
      HA(10)=2.0D 0*SIN(T)**2
      RETURN
  140 T=Y(KA)
      DO 141 I=7,10
      HA(I)=0.0D 0
  141 CONTINUE
      HA(6)=-2.0D 0
      HA(5)= HA(6)*T
      HA(4)= HA(5)*T
      HA(3)= HA(4)
      HA(2)= HA(3)*T
      HA(1)= HA(2)*T
      RETURN
  150 T=Y(KA)
      DO 151 I=1,10
      HA(I)=0.0D 0
  151 CONTINUE
      X1=EXP(X(3)*T)
      X2=EXP(X(4)*T)
      HA(4)= X1*T
      HA(8)= X2*T
      HA(6)= X(1)*X1*T**2
      HA(10)=X(2)*X2*T**2
      RETURN
  200 T=Y(KA)
      X1=T+X(2)+1.0D 0/(X(3)*T+X(4))
      IF (X1.EQ.0D 0) X1=1D-30
      X2=X1/((T+1.0D 0)*Y(61+KA))
      X3=X(3)*T+X(4)
      IF (X3.EQ.0D 0) X3=1D-30
      HA(1)= 0.0D 0
      HA(2)= ABS(X2)**(T+5.0D-1)*(T+5.0D-1)/X1
      BETA=X(1)*HA(2)/X1
      HA(3)= BETA*(T-5.0D-1)
      HA(7)=-HA(2)/(X3*X3)
      HA(4)= HA(7)*T
      HA(8)=-HA(3)/(X3*X3)
      HA(5)= HA(8)*T
      HA(10)=BETA*(T+1.5D 0+(X3+X3)*(T+X(2)))/X3**4
      HA(6)= HA(10)*T*T
      HA(9)= HA(10)*T
      RETURN
   30 T=0.1D 0*DBLE(KA-1)-1.0D 0
      HA(1)=0.0D 0
      HA(2)=0.0D 0
      HA(3)=0.0D 0
      X1=X(1)+T*X(2)
      X2=1.0D 0/(1.0D 0+T*(X(3)+T*(X(4)+T*X(5))))
      X3=-T*X2*X2
      X4=-2.0D 0*T*X1*X2*X3
      HA(4)=X3
      HA(5)=HA(4)*T
      HA(7)=HA(5)
      HA(8)=HA(7)*T
      HA(11)=HA(8)
      HA(12)=HA(11)*T
      HA(6)=X4
      HA(9)=HA(6)*T
      HA(10)=HA(9)*T
      HA(13)=HA(10)
      HA(14)=HA(13)*T
      HA(15)=HA(14)*T
      RETURN
  130 T=Y(KA)
      DO 131 I=1,6
      HA(I)=0.0D 0
  131 CONTINUE
      X1=1.0D 0/(1.0D 0-T*(X(4)-T*X(5)))
      X2=X(1)+T*(X(2)+T*X(3))
      X3=X1*X1
      HA(7)=-T*X3
      HA(8)=HA(7)*T
      HA(9)=HA(8)*T
      HA(10)= 2.0D 0*T*T*X1*X2*X3
      HA(11)= HA(8)
      HA(12)= HA(11)*T
      HA(13)= HA(12)*T
      HA(14)= HA(10)*T
      HA(15)= HA(14)*T
      RETURN
  100 T=0.1D 0*DBLE(KA-1)
      DO 101 I=1,N*(N+1)/2
      HA(I)=0.0D 0
  101 CONTINUE
      X1=EXP(-X(2)*T)
      X2=COS(X(3)*T+X(4))
      X3=SIN(X(3)*T+X(4))
      X4=EXP(-X(6)*T)
      HA(2)= -X1*X2*T
      HA(10)=-X1*X2*X(1)
      HA(9)=  HA(10)*T
      HA(3)= -HA(9)*T
      HA(7)= -X1*X3
      HA(4)=  HA(7)*T
      HA(8)=  X1*X3*X(1)*T
      HA(5)=  HA(8)*T
      HA(6)= -HA(3)
      HA(20)=-X4*T
      HA(21)=-HA(20)*X(5)*T
      RETURN
   40 BETA=0.5D 0*PI*Y(KA)
      DO 41 I=1,3
      J=I+I
      XA(I)=X(J-1)
      XB(I)=X(J)
   41 CONTINUE
      CA(4)=CMPLX(1.0D 0,0.0D 0)
      CB(4)=1.0D 1*CA(4)
      DO 42 J=1,3
      I=4-J
      XC(I)=BETA*XA(I)
      T=XC(I)
      X1=COS(T)
      X2=SIN(T)
      C1=CMPLX(X1,0.0D 0)
      C2=CMPLX(0.0D 0,(X2*XB(I)))
      C3=CMPLX(0.0D 0,(X2/XB(I)))
      CB(I)=C1*CB(I+1)+C2*CA(I+1)
      CA(I)=C3*CB(I+1)+C1*CA(I+1)
   42 CONTINUE
      C1=-CA(1)
      C2=CB(1)-C1
      C3=1.0D 0+2.0D 0*C1/C2
      FA=CDABS(C3)
      C3=CONJG(C3)
      C1=2.0D 0/(C2*C2)
      T=BETA
      DO 43 I=1,3
      ST(I)=SIN(XC(I))
      CT(I)=COS(XC(I))
      CC(I+I)=(CB(I)*CA(I)-CB(I+1)*CA(I+1))/XB(I)
      CC(I+I-1)=(CB(I)*CA(I+1)-CB(I+1)*CA(I))*T/ST(I)
   43 CONTINUE
      DO 44 I=1,6
      GA(I)=DBLE(C3*C1*CC(I))/FA
   44 CONTINUE
      CI=CMPLX(0D 0,1D 0)
      X2=X(2)
      X4=X(4)
      X3=X(6)
      C2=-(C1+C1)/C2
      S1=CMPLX(-ST(1)*X2,CT(1))
      S2=CMPLX(ST(1)/X2,-CT(1))
      S3=S1*CT(2)-CI*S2*ST(2)*X4
      S4=S2*CT(2)-CI*S1*ST(2)/X4
      DD(1)=C2*( T*CI*(CB(1)/X2+CA(1)*X2))
      DD(2)=C2*(-ST(1)*CI*(CB(2)/(X2*X2)-CA(2)))
      DD(3)=C2*( T*(CB(2)*S1/X4-CA(2)*S2*X4))
      DD(4)=C2*(-ST(2)*(CB(3)*S1/(X4*X4)+CA(3)*S2))
      DD(5)=C2*( T*(CB(3)*S3/X3-CA(3)*S4*X3))
      DD(6)=C2*(-ST(3)*(1D 1*S3/(X3*X3)+S4))
      L=0
      DO 45 I=1,6
      L=L+I-1
      DO 45 J=1,I
      HA(L+J)=(DBLE(CC(I)*(C3*DD(J)+C1*CONJG(C1*CC(J))))-GA(I)*GA(J))/
     & FA
   45 CONTINUE
      DO 46 I=1,3
      J=I*(I+I+1)
      HA(J-1)=HA(J-1)+DBLE(C3*C1*CI*T*(CA(I)*CA(I)+(CB(I)/XB(I))**2))/
     & FA
      HA(J)=HA(J)+DBLE(C3*C1/XB(I)*(CI*ST(I)*(CA(I)*CA(I+1)-
     & CB(I)*CB(I+1)/(XB(I)*XB(I)))-CC(I+I)))/FA
   46 CONTINUE
      RETURN
  110 T=Y(41+KA)
      BETA=Y(82+KA)
      X1=(X(1)+(1.0D 0+X(2))*T)**2+((1.0D 0-X(2))*BETA)**2
      X2=(X(3)+(1.0D 0+X(4))*T)**2+((1.0D 0-X(4))*BETA)**2
      X3=(X(5)+(1.0D 0+X(6))*T)**2+((1.0D 0-X(6))*BETA)**2
      X4=(X(7)+(1.0D 0+X(8))*T)**2+((1.0D 0-X(8))*BETA)**2
      IF (X1.EQ.0.0D 0) X1=1.0D-30
      IF (X2.EQ.0.0D 0) X2=1.0D-30
      IF (X3.EQ.0.0D 0) X3=1.0D-30
      IF (X4.EQ.0.0D 0) X4=1.0D-30
      FA=SQRT(X1/X2)*SQRT(X3/X4)
      HA(37)= FA/X1*(X(1)+T*(1.0D 0+X(2)))
      HA(38)= FA/X1*(X(2)+2.0D 0*T*T-1.0D 0+X(1)*T)
      HA(39)=-FA/X2*(X(3)+T*(1.0D 0+X(4)))
      HA(40)=-FA/X2*(X(4)+2.0D 0*T*T-1.0D 0+X(3)*T)
      HA(41)= FA/X3*(X(5)+T*(1.0D 0+X(6)))
      HA(42)= FA/X3*(X(6)+2.0D 0*T*T-1.0D 0+X(5)*T)
      HA(43)=-FA/X4*(X(7)+T*(1.0D 0+X(8)))
      HA(44)=-FA/X4*(X(8)+2.0D 0*T*T-1.0D 0+X(7)*T)
      HA(45)= 0.0D0
      FA=X(9)*FA
      DO111 I=1,8
  111 GA(I)=HA(36+I)*X(9)
      DO112 J=1,8
      DO112 I=1,J
  112 HA((J-1)*J/2+I)=GA(I)*GA(J)/FA
      HA(1)= FA/X1-HA(1)
      HA(2)= FA*T/X1-HA(2)
      HA(3)= FA/X1-HA(3)
      HA(6)= 3.0D 0*HA(6) -FA/X2
      HA(9)= 3.0D 0*HA(9) -FA*T/X2
      HA(10)=3.0D 0*HA(10)-FA/X2
      HA(15)=FA/X3-HA(15)
      HA(20)=FA*T/X3-HA(20)
      HA(21)=FA/X3-HA(21)
      HA(28)=3.0D 0*HA(28)-FA/X4
      HA(35)=3.0D 0*HA(35)-FA*T/X4
      HA(36)=3.0D 0*HA(36)-FA/X4
      RETURN
   50 DO 51 I=1,N*(N+1)/2
      HA(I)=0.0D 0
   51 CONTINUE
      HA(1)=2.0D 0
      HA(3)=1.0D 1
      HA(6)=1.2D 1*X(3)**2
      HA(10)=6.0D 0
      HA(15)=3.0D 2*X(5)**4
      HA(21)=1.4D 1
      HA(27)=-4.0D 0
      HA(28)=1.2D 1*X(7)**2
      GO TO (56,52,53,54,55),KA
   52 HA(1)=HA(1)+4.0D 1
      HA(3)=HA(3)+3.6D 2*X(2)**2
      HA(10)=HA(10)+8.0D 1
      RETURN
   53 HA(6)=HA(6)+2.0D 2
      RETURN
   54 HA(3)=HA(3)+2.0D 1
      HA(21)=HA(21)+1.2D 2
      RETURN
   55 HA(1)=HA(1)+8.0D 1
      HA(2)=HA(2)-3.0D 1
      HA(3)=HA(3)+2.0D 1
      HA(6)=HA(6)+4.0D 1
   56 RETURN
   60 DO 61 I=1,N*(N+1)/2
      HA(I)=0.0D 0
   61 CONTINUE
      HA(1)=2.0D 0
      HA(2)=1.0D 0
      HA(3)=2.0D 0
      HA(6)=2.0D 0
      HA(10)=8.0D 0
      HA(15)=2.0D 0
      HA(21)=4.0D 0
      HA(28)=1.0D 1
      HA(36)=1.4D 1
      HA(45)=4.0D 0
      HA(55)=2.0D 0
      GO TO (69,62,63,64,65,69,69,68,69),KA
   62 HA(1)=HA(1)+6.0D 1
      HA(3)=HA(3)+8.0D 1
      HA(6)=HA(6)+4.0D 1
      RETURN
   63 HA(1)=HA(1)+1.0D 2
      HA(6)=HA(6)+2.0D 1
      RETURN
   64 HA(1)=HA(1)+1.0D 1
      HA(3)=HA(3)+4.0D 1
      HA(15)=HA(15)+6.0D 1
      RETURN
   65 HA(1)=HA(1)+1.0D 1
      HA(2)=HA(2)-2.0D 1
      HA(3)=HA(3)+4.0D 1
      RETURN
   68 HA(45)=HA(45)+2.4D 2
   69 RETURN
   70 DO 71 I=1,N*(N+1)/2
      HA(I)=0.0D 0
   71 CONTINUE
      HA(1)=2.0D 0
      HA(2)=1.0D 0
      HA(3)=2.0D 0
      HA(6)=2.0D 0
      HA(10)=8.0D 0
      HA(15)=2.0D 0
      HA(21)=4.0D 0
      HA(28)=1.0D 1
      HA(36)=1.4D 1
      HA(45)=4.0D 0
      HA(55)=2.0D 0
      HA(66)=2.0D 0
      HA(78)=2.0D 1
      HA(91)=1.0D 1
      HA(105)=8.0D 0
      HA(120)=5.4D 1
      HA(136)=1.2D 1*X(16)**2
      HA(153)=2.0D 0
      HA(171)=2.6D 1
      HA(190)=2.0D 0
      HA(210)=2.0D 0
      GO TO (69,62,63,64,65,69,69,68,69,69,72,73,74,75,76,77,78,79),KA
   72 HA(1)=HA(1)+2.0D 1
      RETURN
   73 HA(91)=HA(91)+1.0D 2
      RETURN
   74 HA(91)=HA(91)+6.0D 1
      RETURN
   75 HA(1)=HA(1)+2.8D 2
      RETURN
   76 HA(3)=HA(3)+3.0D 2
      RETURN
   77 HA(1)=HA(1)+1.0D 2
      HA(153)=HA(153)+10.8D 2*X(17)**2
      RETURN
   78 HA(1)=HA(1)+2.0D 1
      RETURN
   79 HA(1)=HA(1)+1.4D 2
      HA(3)=HA(3)+1.0D 2
      HA(190)=HA(190)+2.0D 1
      RETURN
  220 X1=0.0D 0
      DO 221 I=1,N
      X3=1.0D 0
      X4=X(I)
      IF (I.EQ.1) X3=1.0D-8
      IF (I.EQ.4) X3=4.0D 0
      IF (I.EQ.2.AND.KA.EQ.1) X4=X(I)+2.0D 0
      IF (I.EQ.2.AND.KA.EQ.2) X4=X(I)-2.0D 0
      X1=X1+X3*X4**2
  221 CONTINUE
      X2=EXP(X1)
      L=0
      DO 223 I=1,N
      X3=2.0D 0
      X4=X(I)
      IF (I.EQ.1) X3=2.0D-8
      IF (I.EQ.4) X3=8.0D 0
      IF (I.EQ.2.AND.KA.EQ.1) X4=X(I)+2.0D 0
      IF (I.EQ.2.AND.KA.EQ.2) X4=X(I)-2.0D 0
      DO 222 J=1,I
      L=L+1
      X5=2.0D 0
      X6=X(J)
      IF (J.EQ.1) X5=2.0D-8
      IF (J.EQ.4) X5=8.0D 0
      IF (J.EQ.2.AND.KA.EQ.1) X6=X(J)+2.0D 0
      IF (J.EQ.2.AND.KA.EQ.2) X6=X(J)-2.0D 0
      HA(L)=X2*X3*X4*X5*X6
  222 CONTINUE
      HA(L)=HA(L)+X2*X3
  223 CONTINUE
      RETURN
  160 DO 161 I=1,N*(N+1)/2
      HA(I)=0.0D 0
  161 CONTINUE
      L=0
      DO 162 I=1,N
      L=L+I
      X1=1.0D 0*DBLE(I+KA-1)
      X2=(X(I)-SIN(DBLE(2*I+KA-3)))**2
      HA(L)=2.0D 0*X1*EXP(X2)*(1.0D 0+2.0D 0*X2)
  162 CONTINUE
      RETURN
  240 IF (KA.LE.2) THEN
      DO 241 I=1,N*(N+1)/2
      HA(I)=0.0D 0
  241 CONTINUE
      IF (KA.EQ.1) THEN
      ELSE IF (KA.EQ.2) THEN
      HA(1)=-2.0D 0
      ENDIF
      ELSE
      T=DBLE(KA-2)/2.9D 1
      L=0
      DO 243 I=1,N
      DO 242 J=1,I
      L=L+1
      HA(L)=-2.0D 0*T**(I+J-2)
  242 CONTINUE
  243 CONTINUE
      ENDIF
      RETURN
  250 T=1.0D-1*DBLE(KA-1)
      DO 251 I=1,N*(N+1)/2
      HA(I)=0.0D 0
  251 CONTINUE
      X1=EXP(-X(5)*T)
      X2=EXP(-X(6)*(T-X( 9))**2)
      X3=EXP(-X(7)*(T-X(10))**2)
      X4=EXP(-X(8)*(T-X(11))**2)
      HA(11)= X1*T
      HA(15)=-X1*X(1)*T**2
      HA(17)= X2*(T-X( 9))**2
      HA(21)=-HA(17)*X(2)*(T-X( 9))**2
      HA(24)= X3*(T-X(10))**2
      HA(28)=-HA(24)*X(3)*(T-X(10))**2
      HA(32)= X4*(T-X(11))**2
      HA(36)=-HA(32)*X(4)*(T-X(11))**2
      HA(38)=-2.0D 0*X2*X(6)*(T-X( 9))
      HA(42)=-2.0D 0*X2*X(2)*(T-X( 9))+
     & 2.0D 0*X2*X(2)*X(6)*(T-X( 9))**3
      HA(45)= 2.0D 0*X2*X(2)*X(6)-
     & 4.0D 0*X2*X(2)*(X(6)*(T-X( 9)))**2
      HA(48)=-2.0D 0*X3*X(7)*(T-X(10))
      HA(52)=-2.0D 0*X3*X(3)*(T-X(10))+
     & 2.0D 0*X3*X(3)*X(7)*(T-X(10))**3
      HA(55)= 2.0D 0*X3*X(3)*X(7)-
     & 4.0D 0*X3*X(3)*(X(7)*(T-X(10)))**2
      HA(59)=-2.0D 0*X4*X(8)*(T-X(11))
      HA(63)=-2.0D 0*X4*X(4)*(T-X(11))+
     & 2.0D 0*X4*X(4)*X(8)*(T-X(11))**3
      HA(66)= 2.0D 0*X4*X(4)*X(8)-
     & 4.0D 0*X4*X(4)*(X(8)*(T-X(11)))**2
      RETURN
      END
