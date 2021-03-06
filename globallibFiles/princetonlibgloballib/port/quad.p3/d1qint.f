      SUBROUTINE D1QINT(F,A,B,ERR,Y,I,ANS,ERREST,DNOISE,JFAIL)
C
C ROMBERG QUADRATURE OF F(X) FROM A TO B, WITH ABSOLUTE ACCURACY ERR.
C
C ANSWER AND ESTIMATED ERROR RETURNED IN ANS AND ERREST.
C ARRAY Y CONTAINS STACK OF F-VALUES FOR TRAPEZOIDAL RULES FROM H = B-A
C THROUGH H = (B-A)/(3*2**(I-1)).  I AND Y ARE BOTH INPUT AND OUTPUT.
C (IF I = 0 ON INPUT, ONLY HAVE VALUES FOR H = B-A.)
C JFAIL IS RETURNED AS A FLAG.  0 MEANS SUCCESS, -1 MEANS NEED
C TO DIVIDE INTERVAL AND TRY AGAIN, AND JFAIL.GT.0 MEANS A PROBLEM.
C IF JFAIL = 1, DNOISE MAY BE RETURNED AS AN ESTIMATE OF NOISE IN
C THE FUNCTION F.
C
C DIVIDE B-A INTO 1,2,3,4,6,8,12,16,...,96 INTERVALS
C DO TRAPEZOIDAL RULE AND CAUTIOUS RICHARDSON EXTRAPOLATION
C
C PARAMETERS GOVERNING EXTRAPOLATIONS
C  KMIN = MINIMUM NUMBER BEFORE ANSWER IS BELIEVED  (KMIN.GE.2)
C  KMAX = MAXIMUM NUMBER BEFORE GIVING UP,  (KMAX.LE.12)
C  KDIV = MINIMUM NUMBER BEFORE GIVING UP IF NOT CONVERGING. (KDIV.GE.4)
C  HMAX = LARGEST TRAPEZOIDAL RULE H WHOSE ANSWER WILL BE BELIEVED.
C  HMIN = SMALLEST H THAT WILL BE USED, APPROXIMATELY.
C
C 0 .LE. I .LE. 6 ASSUMED ON INPUT.  I MAY BE INCREASED ON OUTPUT.
C IF IT IS, NEW F-VALUES ARE CALCULATED AND STORED IN Y.
C (THE FOLLOWING IS ASSUMED TRUE ON ENTERING, AND REMAINS TRUE ON UNSUC-
C CESSFUL EXIT, FOR OUTPUT VALUE OF I.  NOT TRUE ON SUCCESSFUL EXIT.)
C Y(J) CONTAINS F(A+(J-1)*(B-A)/(3*2**I)), FOR THOSE JS USED IN TRAP-
C EZOIDAL RULES THROUGH H = (B-A)/(3*2**(I-1)).  CONTENTS OF OTHER JS
C ARE GARBAGE.  J RUNS FROM 1 THROUGH 3*2**I+1.
C
C IF THE INTERVAL NEEDS TO BE SUBDIVIDED, RETURNS ARE MADE ONLY
C AFTER 4, 6, 8, 10, OR 12 EXTRAPOLATIONS.
C SUCCESSFUL RETURNS CAN BE MADE AFTER KMIN OR MORE EXTRAPOLATIONS.
C
      COMMON /D1QCM1/ DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL,
     X   NPRINT,NUM,MAXY,MAXINT,MAXF,KMIN,KMAX,KDIV,NMAX
      COMMON /D1QCM2/ TROUND,HMAX,HMIN,HNOISE,NUMF,INT,JY
      COMMON /D1QCM3/ XBOT,DELX,R(3),RL(3),NEXP,KEXT,KLAST
      COMMON /D1QCM4/ TRAP,T(13),S(13),X(13)
      DOUBLE PRECISION F,A,B,ERR,Y(9),ANS,ERREST,DNOISE,DEL,H,DELTA
      DOUBLE PRECISION DLARGE,DSMALL,DROUND,HSAMPL,HSMALL,ESMALL
      DOUBLE PRECISION TROUND,HMAX,HMIN,HNOISE,XBOT,DELX,TRAP,T,S,X
      DOUBLE PRECISION T0,TCUM(3),D1QFUN,TEMP1,TEMP2
      EXTERNAL F
      LOGICAL NCHECK,INIT,LROUND
      DATA INIT/.FALSE./
C
      IF(INIT) GO TO 20
         X(1)=1.
         X(2)=4.
         X(3)=9.
         DO 10 J=4,13
   10       X(J)=4.*X(J-2)
         INIT=.TRUE.
C
   20 DO 30 J = 1,3
   30    RL(J) = R(J)
      KLAST = KEXT
      ANS = 0.
      ERREST = DLARGE
      DNOISE = 0.
      DEL = B-A
C
C         FIRST TWO EXTRAPOLATIONS
C
      IF (I.GE.1) GO TO 40
         IF(JY.LT.7) GO TO 9940
         IF(NUMF+3.GT.MAXF) GO TO 9950
            I = 1
            Y(3) = D1QFUN(F,(2.*A+B)/3.D0)
            Y(4) = D1QFUN(F,.5*(A+B))
            Y(5) = D1QFUN(F,(A+2.*B)/3.D0)
            Y(7) = Y(2)
            NUMF = NUMF+3
C
   40 N = 3*2**I+1
      T0 = 0.5*(Y(1)+Y(N))
      T(1) = DEL*T0
C
      DO 100 KK = 1,2
         KEXT = KK
         H = DEL/FLOAT(KEXT+1)
         L = (N+KEXT)/(KEXT+1)
         TCUM(KEXT) = Y(L)
         M = N+1-L
         IF (KEXT.EQ.2) TCUM(2) = TCUM(2)+Y(M)
         TRAP = H*(T0+TCUM(KEXT))
         NCHECK = KEXT.LT.KMIN .OR. DABS(H).GT.HMAX
         CALL D1QEXT(KEXT,KDIV,NCHECK,TROUND,ANS,ERREST,R,KASYM,LROUND)
         IF (ERREST.LE.ERR) GO TO 9900
         IF (LROUND) GO TO 9910
  100    CONTINUE
      TCUM(3) = TCUM(1)
C
C         REST OF EXTRAPOLATIONS
C
      NEW = 1
      IMAX = MIN0(6,(KMAX+1)/2)
      DO 200 INEW =2,IMAX
C
         II = MAX0(1,2**(I-INEW))
         IF (INEW.LE.I) GO TO 120
            IF ((2*N-1).GT.JY) GO TO 9940
            IF((NUMF+4*NEW).GT.MAXF) GO TO 9950
C                 HAVE USED ALL INPUT F-VALUES.
C                 SHIFT Y ARRAY TO MAKE ROOM FOR MORE.
               DO 110 J = 2,N
                  L = N+2-J
  110             Y(2*L-1) = Y(L)
               N = 2*N-1
C
  120    DO 140 KK=1,2
            H = DEL/FLOAT(NEW*2*(KK+1))
            DO 130 J = 1,NEW
               L = II*(6*J*KK-(7*KK-4))+1
               M = N+1-L
               IF (I.GE.INEW) GO TO 130
                  DELTA = H*FLOAT((4*KK-2)*(J-1)+1)
                  Y(L) = D1QFUN(F,A+DELTA)
                  Y(M) = D1QFUN(F,B-DELTA)
                  NUMF = NUMF+2
  130          TCUM(KK) = TCUM(KK)+Y(L)+Y(M)
            TRAP = T0+TCUM(KK)
            IF(KK.EQ.2) TRAP = TRAP+TCUM(3)
            TRAP = H*TRAP
            KEXT = KEXT+1
            NCHECK = KEXT.LT.KMIN .OR. DABS(H).GT.HMAX
            CALL D1QEXT(KEXT,KDIV,NCHECK,TROUND,ANS,ERREST,R,KASYM,
     X                  LROUND)
            IF (ERREST.LE.ERR) GO TO 9900
            IF (LROUND) GO TO 9910
  140       CONTINUE
         TCUM(3) = TCUM(1)
         I = MAX0(I,INEW)
C
         IF (DABS(DEL).LE.HMIN) GO TO 9920
         IF (KEXT.GE.KDIV .AND. I.EQ.INEW
     X         .AND. (KASYM.LT.(KEXT-2))) GO TO 210
C
  200    NEW = 2*NEW
C
  210 IF (INT.EQ.MAXINT) GO TO 9930
C
C        CHECK FOR NOISY FUNCTION
C
      IF (DABS(H).GT.HNOISE) GO TO 9890
         NS = 0
         KK = 1+3*2**I
         TEMP1 = Y(5)-2.*Y(3)+Y(1)
         DNOISE = DABS(TEMP1)
         DO 230 J = 7,KK,2
            TEMP2 = Y(J)-2.*Y(J-2)+Y(J-4)
            DNOISE = DNOISE+DABS(TEMP2)
            IF(DSIGN(1.D0,TEMP1).NE.DSIGN(1.D0,TEMP2)) NS = NS+1
  230       TEMP1 = TEMP2
         DNOISE = 2.*DNOISE/FLOAT(KK-3)
         IF(NS.GT.2) GOTO 9910
C
 9890 JFAIL = -1
      RETURN
 9900 JFAIL = 0
      RETURN
 9910 JFAIL = 1
      RETURN
 9920 JFAIL = 2
      RETURN
 9930 JFAIL = 3
      RETURN
 9940 JFAIL = 4
      RETURN
 9950 JFAIL = 5
      RETURN
      END
