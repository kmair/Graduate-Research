      DOUBLE PRECISION FUNCTION DEESFP(K,T,N,A,F,X,NX,XEST,MEST,EEST)
C
C
C PURPOSE -
C
C   TO FIND THE MAXIMUM ABSOLUTE DIFFERENCE BETWEEN F(Y) AND S(Y),
C   WHERE S(Y) IS THE B-SPLINE GIVEN BY (K,T,N,A), ON EACH INTERVA
C   (X(I),X(I+1)) FOR I = 1,...,NX-1.
C
C METHOD -
C
C   THE MAXIMUM IS FOUND (ESTIMATED) BY
C   SEARCHING EACH T INTERVAL AT A PATTERN OF MEST POINTS GIVEN BY
C
C MNEMONIC - DOUBLE PRECISION ESTIMATION OF THE ERROR IN A ,
C            SPLINE FIT TO A FUNCTION, USING A GIVEN PATTERN IN TH
C
C INPUT -
C
C   K      - THE B-SPLINE ORDER USED.
C   T      - THE B-SPLINE MESH.
C   N      - THE NUMBER OF POINTS IN THE MESH T.
C   A      - THE B-SPLINE COEFFICIENTS.
C   F      - THE NAME OF A USER SUPPLIED SUBROUTINE
C
C                    F(X,NX,FX,WX)
C
C            F MUST DECLARED EXTERNAL IN THE USERS CALLING PROGRAM
C
C            INPUT -
C
C              X    - THE POINTS WHERE F IS TO BE EVALUATED.
C              NX   - THE LENGTH OF THE ARRAY X.
C
C            OUTPUT -
C
C              FX - FX(I) = F( X(I) ), I = 1,...,NX.
C              WX - WX IS A DUMMY ARRAY OF LENGTH NX TO MAKE THE C
C                   CONSISTANT WITH THAT TO F IN DL2SFF.
C   X      - THE INTERVALS ON WHICH THE ERROR IS DESIRED.
C            X MUST BE MONOTONE INCREASING.
C   NX     - THE NUMBER OF POINTS IN X.
C   XEST   - THE PATTERN OF POINTS ON (-1,+1) TO BE SCALED AND USE
C            THE SEARCH OF EACH MESH INTERVAL FOR THE MAXIMUM ERRO
C            XEST MUST BE MONOTONE INCREASING.
C   MEST   - THE NUMBER OF POINTS IN XEST.
C
C OUTPUT -
C
C   EEST - EEST(I) IS THE MAXIMUM ERROR FOUND ON THE I-TH X INTERV
C          FOR I = 1,...,NX-1.
C          IF X(I) = X(I+1) THEN EEST(I) IS MEANINGLESS.
C
C          EEST(NX) = MAX(EEST(1),...,EEST(NX-1))
C
C   PROCEDURE VALUE - EEST(NX).
C
C SCRATCH SPACE ALLOCATED - 4*K LONG REAL WORDS.
C
C ERROR STATES -
C
C   1 - K.LT.2.
C   2 - N.LE.K.
C   3 - NX.LT.2.
C   4 - MEST.LT.1.
C   5 - T NOT MONOTONE INCREASING.
C   6 - (X(1),X(NX)) NOT PART OF (T(1),T(N)).
C   7 - X IS NOT MONOTONE INCREASING.
C   8 - XEST(I) NOT IN (-1,+1).
C   9 - XEST IS NOT MONOTONE INCREASING.
C
      INTEGER X2360(1)
      INTEGER ID(1),I,IADIFF,ISTKGT,ILEFT,ILO,JLO,J,L
      DOUBLE PRECISION Y(1),FY(1),WY(1),SY(1)
      LOGICAL LS(1000)
      INTEGER IS(1000)
      REAL RS(1000)
      DOUBLE PRECISION WS(500)
      DOUBLE PRECISION DS(500)
      COMMON /CSTAK/DS
      EXTERNAL F
      INTEGER K,N,NX,MEST
      DOUBLE PRECISION T(N),A(1),X(NX),XEST(MEST),EEST(NX)
C
      EQUIVALENCE (X2360(1),ID(1))
      EQUIVALENCE ( DS(1),WS(1),RS(1),IS(1),LS(1) )
C
      DATA X2360(1)/0/
C
C A(N-K).
C
C
C
C
C CHECK THE INPUT.
C
      IF( K.GE.2 )      GOTO 1000
C/6S
C     CALL SETERR(15HDEESFP - K.LT.2,15,1,2)
C/7S
      CALL SETERR('DEESFP - K.LT.2',15,1,2)
C/
 1000 CONTINUE
      IF( N.GT.K )      GOTO 1002
C/6S
C     CALL SETERR(15HDEESFP - N.LE.K,15,2,2)
C/7S
      CALL SETERR('DEESFP - N.LE.K',15,2,2)
C/
 1002 CONTINUE
      IF( NX.GE.2 )      GOTO 1004
C/6S
C     CALL SETERR(16HDEESFP - NX.LT.2,16,3,2)
C/7S
      CALL SETERR('DEESFP - NX.LT.2',16,3,2)
C/
 1004 CONTINUE
      IF( MEST.GE.1 )      GOTO 1006
C/6S
C     CALL SETERR(18HDEESFP - MEST.LT.1,18,4,2)
C/7S
      CALL SETERR('DEESFP - MEST.LT.1',18,4,2)
C/
 1006 CONTINUE
C
      I21010 = N-1
      DO 1008 I = 1, I21010
      IF( T(I).LE.T(I+1) )      GOTO 1011
C/6S
C     CALL SETERR(34HDEESFP - T NOT MONOTONE INCREASING,34,5,2)
C/7S
      CALL SETERR('DEESFP - T NOT MONOTONE INCREASING',34,5,2)
C/
 1011 CONTINUE
      IF( I+K.LE.N )      GOTO 1013
      GOTO 1008
 1013 CONTINUE
      IX1005 = I+K
      IF( T(IX1005).GT.T(I) )      GOTO 1015
C/6S
C     CALL SETERR(34HDEESFP - T NOT MONOTONE INCREASING,34,5,2)
C/7S
      CALL SETERR('DEESFP - T NOT MONOTONE INCREASING',34,5,2)
C/
 1015 CONTINUE
C
 1008 CONTINUE
 1009 CONTINUE
      IF( X(1).GE.T(1).AND.X(NX).LE.T(N) )      GOTO 1017
C/6S
C           CALL SETERR(45HDEESFP - (X(1),X(NX)) NOT PART OF (T(1),T(N))
C    *,45,6,2)
C/7S
            CALL SETERR('DEESFP - (X(1),X(NX)) NOT PART OF (T(1),T(N))'
     *,45,6,2)
C/
C
 1017 CONTINUE
      I21021 = NX-1
      DO 1019 I = 1, I21021
      IF( X(I).LE.X(I+1) )      GOTO 1022
C/6S
C           CALL SETERR(37HDEESFP - X IS NOT MONOTONE INCREASING,37,7,2)
C/7S
            CALL SETERR('DEESFP - X IS NOT MONOTONE INCREASING',37,7,2)
C/
 1022 CONTINUE
C
 1019 CONTINUE
 1020 CONTINUE
      DO 1024 I = 1, MEST
            IF( XEST(I).GE.(-1.0D0).AND.XEST(I).LE.(+1.0D0) )      GOTO
     *1027
C/6S
C     CALL SETERR(31HDEESFP - XEST(I) NOT IN (-1,+1),31,8,2)
C/7S
      CALL SETERR('DEESFP - XEST(I) NOT IN (-1,+1)',31,8,2)
C/
 1027 CONTINUE
      IF( I.NE.MEST )      GOTO 1029
      GOTO 1024
C
 1029 CONTINUE
      IF( XEST(I).LE.XEST(I+1) )      GOTO 1031
C/6S
C           CALL SETERR(40HDEESFP - XEST IS NOT MONOTONE INCREASING,40,9
C    *,2)
C/7S
            CALL SETERR('DEESFP - XEST IS NOT MONOTONE INCREASING',40,9
     *,2)
C/
 1031 CONTINUE
C
 1024 CONTINUE
 1025 CONTINUE
      CALL ENTER(1)
C
C ALLOCATE SCRATCH SPACE FOR CALLS TO DSPLN2.
C
      IADIFF = ISTKGT(K,4)
C
      CALL SETD(NX,0.0D0,EEST)
C
      ILO = 0
      ILEFT = 1
C
      JLO = 1
C CURRENT LEFT-MOST X INTERVAL.
C
      I21035 = N-1
      DO 1033 I = 1, I21035
      IF( T(I).NE.T(I+1).AND.T(I+1).GE.X(1) )      GOTO 1036
      GOTO 1033
 1036 CONTINUE
      IF( T(I).LE.X(NX) )      GOTO 1038
      GOTO 1034
C
 1038 CONTINUE
      DO 1040 L = 1, MEST
      Y(1) = T(I)+0.5D0*(T(I+1)-T(I))*(1.0D0+XEST(L))
      Y(1) = DMAX1(T(I),DMIN1(T(I+1),Y(1)))
C
      CALL F(Y,1,FY,WY)
      CALL DSPLN2(K,T,N,A,Y,1,ID,1,SY,K,WS(IADIFF),ILO,ILEFT)
C
      J = JLO
 1045 CONTINUE
      IF( X(J).GT.Y(1).OR.J.GE.NX )      GOTO 1044
      IF( X(J+1).GE.Y(1) )      GOTO 1046
      JLO = J
      GOTO 1043
C
 1046 CONTINUE
      EEST(J) = DMAX1(EEST(J),DABS(FY(1)-SY(1)))
      EEST(NX) = DMAX1(EEST(NX),EEST(J))
 1043 CONTINUE
      J = J + (1)
      GOTO 1045
 1044 CONTINUE
 1040 CONTINUE
 1041 CONTINUE
C
 1033 CONTINUE
 1034 CONTINUE
      CALL LEAVE
C
      DEESFP = EEST(NX)
      RETURN
C
      END
