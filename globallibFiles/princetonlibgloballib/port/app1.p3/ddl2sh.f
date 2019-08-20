      SUBROUTINE DDL2SH(X,Y,NXY,MD,W,K,T,NT,A)
C
C
C TO FIT A B-SPLINE TO DISCRETE DATA.
C
C THE MNEMONIC IS -
C
C   DOUBLE PRECISION DISCRETE L2 B-SPLINE FIT, USING HERMITE DATA.
C
C METHOD - LET
C
C   E(F) = SUM(J = 1,...,MD) SUM(I = 1,...,NXY)
C             ( (Y(I,J) - F-SUP-(J-1)) (X(I)) **2 * W(I,J) )
C
C   THIS PROGRAM SOLVES THE PROBLEM
C
C         MINIMIZE (OVER ALL B-SPLINES F) E(F).
C
C INPUT
C
C   X      - THE POINTS WHERE THE FITTING IS TO TAKE PLACE.
C            THE ARRAY X MUST BE MONOTONE INCREASING.
C            ANY MULTIPLICITIES IN X WILL BE IGNORED.
C   Y      - Y(I,J) IS THE VALUE THE (J-1)-ST DERIVATIVE OF THE B-
C            IS TO TAKE AT X(I), FOR I = 1,..,NXY AND J = 1,...,MD
C   NXY    - THE NUMBER OF POINTS IN X.
C   MD     - THE NUMBER OF DERIVATIVES TO BE FITTED.
C   W      - THE WEIGHT FOR THE VALUE OF Y(I,J) IS W(I,J),
C            FOR I = 1,...,NXY AND J = 1,...,MD.
C   K      - THE ORDER OF THE B-SPLINE TO BE USED.
C   T      - THE B-SPLINE MESH TO BE USED.
C   NT     - THE NUMBER OF POINTS IN THE MESH T.
C
C OUTPUT
C
C   A      - THE COEFFICIENTS FOR THE B-SPLINE FIT.
C
C SCRATCH SPACE ALLOCATED -
C
C   (NT-K+MD)*K + MAX( 3*K , K*( MIN((K+1)/2,MD) + 1 ) )
C
C LONG REAL WORDS +
C
C   MD
C
C INTEGER WORDS.
C
C ERROR STATES -
C
C    1 - NXY.LT.1.
C    2 - MD.LT.1.
C    3 - MD.GT.K.
C    4 - K.LT.2.
C    5 - NT.LE.K.
C    6 - X IS NOT MONOTONE INCREASING.
C    7 - NOT ENOUGH FITTING POINTS FOR A UNIQUE L2 FIT.
C    8 - X(1).LT.T(1).
C    9 - X(NXY).GT.T(NT).
C   10 - MESH T IS NOT MONOTONE.
C   11 - MULT(T(I)).GT.K.
C   12 - W(I,J).LT.0.
C   13 - SINGULAR LEAST SQUARES SYSTEM. (RECOVERABLE)
C
      LOGICAL LS(1000)
      INTEGER IS(1000)
      REAL RS(1000)
      DOUBLE PRECISION WS(500)
      DOUBLE PRECISION DS(500)
      COMMON /CSTAK/DS
      INTEGER NDX,I,J,IH,IBX,ISTKGT,NERROR,NERR
      INTEGER NXY,MD,K,NT
      DOUBLE PRECISION X(NXY),Y(NXY,MD),W(NXY,MD),T(NT),A(1)
C
      EQUIVALENCE ( DS(1),WS(1),RS(1),IS(1),LS(1) )
C
C
C A(NT-K)
C
C
C
C CHECK THE INPUT FOR ERRORS.
C
      IF( NXY.GE.1 )      GOTO 1000
C/6S
C     CALL SETERR(17HDDL2SH - NXY.LT.1,17,1,2)
C/7S
      CALL SETERR('DDL2SH - NXY.LT.1',17,1,2)
C/
 1000 CONTINUE
      IF( MD.GE.1 )      GOTO 1002
C/6S
C     CALL SETERR(16HDDL2SH - MD.LT.1,16,2,2)
C/7S
      CALL SETERR('DDL2SH - MD.LT.1',16,2,2)
C/
 1002 CONTINUE
      IF( MD.LE.K )      GOTO 1004
C/6S
C     CALL SETERR(16HDDL2SH - MD.GT.K,16,3,2)
C/7S
      CALL SETERR('DDL2SH - MD.GT.K',16,3,2)
C/
 1004 CONTINUE
      IF( K.GE.2 )      GOTO 1006
C/6S
C     CALL SETERR(15HDDL2SH - K.LT.2,15,4,2)
C/7S
      CALL SETERR('DDL2SH - K.LT.2',15,4,2)
C/
 1006 CONTINUE
      IF( NT.GT.K )      GOTO 1008
C/6S
C     CALL SETERR(16HDDL2SH - NT.LE.K,16,5,2)
C/7S
      CALL SETERR('DDL2SH - NT.LE.K',16,5,2)
C/
 1008 CONTINUE
C
C COUNT THE NUMBER OF DISTINCT X S.
C
      NDX = 1
      I = 2
 1012 CONTINUE
      IF( I.GT.NXY )      GOTO 1011
      IF( X(I).GE.X(I-1) )      GOTO 1013
C/6S
C           CALL SETERR(37HDDL2SH - X IS NOT MONOTONE INCREASING,37,6,2)
C/7S
            CALL SETERR('DDL2SH - X IS NOT MONOTONE INCREASING',37,6,2)
C/
 1013 CONTINUE
C
      IF( X(I).EQ.X(I-1) )      GOTO 1015
      NDX = NDX + (1)
 1015 CONTINUE
 1010 CONTINUE
      I = I + (1)
      GOTO 1012
 1011 CONTINUE
      IF( NDX*MD.GE.NT-K )      GOTO 1017
C/6S
C           CALL SETERR(54HDDL2SH - NOT ENOUGH FITTING POINTS FOR A UNIQ
C    *UE L2 FIT,54,7,2)
C/7S
            CALL SETERR('DDL2SH - NOT ENOUGH FITTING POINTS FOR A UNIQUE
     * L2 FIT',54,7,2)
C/
 1017 CONTINUE
C
      IF( X(1).GE.T(1) )      GOTO 1019
C/6S
C     CALL SETERR(21HDDL2SH - X(1).LT.T(1),21,8,2)
C/7S
      CALL SETERR('DDL2SH - X(1).LT.T(1)',21,8,2)
C/
 1019 CONTINUE
      IF( X(NXY).LE.T(NT) )      GOTO 1021
C/6S
C     CALL SETERR(24HDDL2SH - X(NXY).GT.T(NT),24,9,2)
C/7S
      CALL SETERR('DDL2SH - X(NXY).GT.T(NT)',24,9,2)
C/
 1021 CONTINUE
C
C CHECK THE MESH T.
C
      I21025 = NT-1
      DO 1023 I = 1, I21025
      IF( T(I).LE.T(I+1) )      GOTO 1026
C/6S
C     CALL SETERR(31HDDL2SH - MESH T IS NOT MONOTONE,31,10,2)
C/7S
      CALL SETERR('DDL2SH - MESH T IS NOT MONOTONE',31,10,2)
C/
 1026 CONTINUE
      IF( I+K.LE.NT )      GOTO 1028
      GOTO 1023
 1028 CONTINUE
      IX1005 = I+K
      IF( T(IX1005).NE.T(I) )      GOTO 1030
C/6S
C     CALL SETERR(24HDDL2SH - MULT(T(I)).GT.K,24,11,2)
C/7S
      CALL SETERR('DDL2SH - MULT(T(I)).GT.K',24,11,2)
C/
 1030 CONTINUE
C
 1023 CONTINUE
 1024 CONTINUE
      DO 1032 I = 1, NXY
C     CHECK THE WEIGHTS.
      J = 1
 1037 CONTINUE
      IF( J.GT.MD )      GOTO 1036
      IF( W(I,J).GE.0.0D0 )      GOTO 1038
C/6S
C     CALL SETERR(20HDDL2SH - W(I,J).LT.0,20,12,2)
C/7S
      CALL SETERR('DDL2SH - W(I,J).LT.0',20,12,2)
C/
 1038 CONTINUE
 1035 CONTINUE
      J = J + (1)
      GOTO 1037
 1036 CONTINUE
C
 1032 CONTINUE
 1033 CONTINUE
      CALL ENTER(1)
C
C NOW SOLVE THE LEAST-SQUARES SYSTEM.
C
      IH = ISTKGT((NT-K)*K,4)
      IBX = ISTKGT(MD*K,4)
C
      CALL D6L2S4(X,Y,NXY,MD,W,K,T,NT,A,WS(IH),WS(IBX),NT-K)
C
      CALL DBSPDS(WS(IH),NT-K,K,A,1)
      IF( NERROR(NERR).EQ.0 )      GOTO 1040
      CALL ERROFF
C/6S
C           CALL SETERR(38HDDL2SH - SINGULAR LEAST SQUARES SYSTEM,38,13,
C    *1)
C/7S
            CALL SETERR('DDL2SH - SINGULAR LEAST SQUARES SYSTEM',38,13,
     *1)
C/
C
 1040 CONTINUE
      CALL LEAVE
C
      RETURN
C
      END