      REAL FUNCTION EEBSM(K,T1,N1,A1,T2,N2,A2,X,NX,MEST,EREST1,ERE
     *ST2)
C
C
C PURPOSE - TO ESTIMATE THE ERROR IN A B-SPLINE APPROXIMATION.
C
C MNEMONIC - DOUBLE PRECISION ESTIMATION OF THE ERROR IN A B-SPLIN
C            APPROXIMATION, USING MEST POINTS IN EACH INTERVAL SEA
C
C METHOD -
C
C   LET (K,T1,N1,A1) REPRESENT AN APPROXIMATION Y1 TO SOME FUNCTIO
C   USING B-SPLINES ON THE MESH T1 WITH COEFFICIENTS A1.
C   LET (K,T2,N2,A2) REPRESENT A REFINEMENT Y2 OF THE (K,T1,N1,A1)
C   APPROXIMATION.
C
C   LET T BE THE UNION OF THE MESHES T1 AND T2.
C
C   THE ERROR IN THE (K,T2,N2,A2) APPROXIMATION IS ESTIMATED ON EA
C   INTERVAL (X(I),X(I+1)), BY SEACHING AT A PATTERN OF MEST POINT
C   IN EACH T INTERVAL INTERSECTING IT, TO BE
C
C      MAX(ABS(Y2-Y1))/((H1(I)/H2(I))**K-1)
C
C   WHERE H1(I) IS DEFINED TO BE THE LENGTH OF THE LARGEST MESH IN
C   OF T1 INTERSECTING (X(I),X(I+1)). SIMILARLY FOR H2(I).
C
C   THE PATTERN OF POINTS XEST USED IS COS(THETA) WHERE THE MEST T
C   VALUES ARE UNIFORMLY DISTRIBUTED ON (-PI,0).
C
C   THIS SEARCH IS SUFFICIENT TO GUARANTEE THAT THE ERROR ESTIMATE
C   COMPUTED TO A RELATIVE ACCURACY OF
C
C        0.5*(K-1)*PI/(MEST-1)
C
C INPUT -
C
C   K    - THE ORDER OF THE B-SPLINES USED.
C          2 .LE. K IS ASSUMED.
C   T1   - THE UNREFINED B-SPLINE MESH.
C   N1   - THE NUMBER OF POINTS IN THE T1 MESH.
C   A1   - THE UNREFINED B-SPLINE COEFFICIENTS.
C   T2   - THE REFINED B-SPLINE MESH.
C   N2   - THE NUMBER OF POINTS IN THE T2 MESH.
C   A2   - THE REFINED B-SPLINE COEFFICIENTS.
C   X    - (X(I),X(I+1)) GIVES THE I-TH INTERVAL AN ERROR ESTIMATE
C          DESIRED FOR, I = 1,...,NX-1.
C          X MUST BE MONOTONE INCREASING.
C   NX   - THE NUMBER OF POINTS IN X.
C   MEST - THE NUMBER OF POINTS TO BE USED IN SEARCHING EACH T INT
C
C OUTPUT -
C
C   EREST1 - EREST1(I) IS THE ERROR ESTIMATE FOR THE T1
C            APPROXIMATION ON THE I-TH X INTERVAL, I = 1,...,NX-1.
C            EREST1(I) = R1MACH(2) IS RETURNED IF T2 IS NOT A REFI
C            OF T1 ON (X(I),X(I+1)).
C            IF X(I) = X(I+1) THEN EREST1(I) IS MEANINGLESS.
C
C            EREST1(NX) = MAX(EREST1(1),...,EREST1(NX-1)).
C
C   EREST2 - EREST2(I) IS THE ERROR ESTIMATE FOR THE T2
C            APPROXIMATION ON THE I-TH X INTERVAL, I = 1,...,NX-1.
C            EREST2(I) = R1MACH(2) IS RETURNED IF T2 IS NOT A REFI
C            OF T1 ON (X(I),X(I+1)).
C            IF X(I) = X(I+1) THEN EREST2(I) IS MEANINGLESS.
C
C            EREST2(NX) = MAX(EREST2(1),...,EREST2(NX-1)).
C
C   PROCEDURE VALUE - EREST1(NX).
C
C SCRATCH SPACE ALLOCATED - 5*K + MEST + NX REAL WORDS.
C
C ERROR STATES -
C
C    1 - K.LT.2.
C    2 - N1.LE.K.
C    3 - N2.LE.K.
C    4 - NX.LT.2.
C    5 - MEST.LT.1.
C    6 - (T1(1),T1(N1)) IS NOT (T2(1),T2(N2)).
C    7 - (X(1),X(NX)) NOT PART OF (T1(1),T1(N1)).
C    8 - T1 NOT MONOTONE INCREASING.
C    9 - T2 NOT MONOTONE INCREASING.
C   10 - X IS NOT MONOTONE INCREASING.
C
      INTEGER I,IXEST,ISTKGT
      REAL VALUE,EEBSP
      LOGICAL LS(1000)
      INTEGER IS(1000)
      REAL RS(1000)
      REAL WS(500)
      DOUBLE PRECISION DS(500)
      COMMON /CSTAK/DS
      INTEGER K,N1,N2,NX,MEST
      REAL T1(N1),A1(1),T2(N2),A2(1),X(NX),EREST1(NX),EREST2(NX)
C
      EQUIVALENCE ( DS(1),WS(1),RS(1),IS(1),LS(1) )
C
C
C REAL A1(N1-K),A2(N2-K)
C
C
C
C CHECK THE INPUT.
C
      IF( K.GE.2 )      GOTO 1000
C/6S
C     CALL SETERR(15H EEBSM - K.LT.2,15,1,2)
C/7S
      CALL SETERR(' EEBSM - K.LT.2',15,1,2)
C/
 1000 CONTINUE
      IF( N1.GT.K )      GOTO 1002
C/6S
C     CALL SETERR(16H EEBSM - N1.LE.K,16,2,2)
C/7S
      CALL SETERR(' EEBSM - N1.LE.K',16,2,2)
C/
 1002 CONTINUE
      IF( N2.GT.K )      GOTO 1004
C/6S
C     CALL SETERR(16H EEBSM - N2.LE.K,16,3,2)
C/7S
      CALL SETERR(' EEBSM - N2.LE.K',16,3,2)
C/
 1004 CONTINUE
      IF( NX.GE.2 )      GOTO 1006
C/6S
C     CALL SETERR(16H EEBSM - NX.LT.2,16,4,2)
C/7S
      CALL SETERR(' EEBSM - NX.LT.2',16,4,2)
C/
 1006 CONTINUE
      IF( MEST.GE.1 )      GOTO 1008
C/6S
C     CALL SETERR(18H EEBSM - MEST.LT.1,18,5,2)
C/7S
      CALL SETERR(' EEBSM - MEST.LT.1',18,5,2)
C/
C
 1008 CONTINUE
      IF( T1(1).EQ.T2(1).AND.T1(N1).EQ.T2(N2) )      GOTO 1010
C/6S
C           CALL SETERR(45H EEBSM - (T1(1),T1(N1)) IS NOT (T2(1),T2(N2))
C    *,45,6,2)
C/7S
            CALL SETERR(' EEBSM - (T1(1),T1(N1)) IS NOT (T2(1),T2(N2))'
     *,45,6,2)
C/
C
 1010 CONTINUE
      IF( X(1).GE.T1(1).AND.X(NX).LE.T1(N1) )      GOTO 1012
C/6S
C           CALL SETERR(48H EEBSM - (X(1),X(NX)) NOT PART OF (T1(1),T1(N
C    *1)),48,7,2)
C/7S
            CALL SETERR(' EEBSM - (X(1),X(NX)) NOT PART OF (T1(1),T1(N1)
     *)',48,7,2)
C/
C
 1012 CONTINUE
      I21016 = N1-1
      DO 1014 I = 1, I21016
      IF( T1(I).LE.T1(I+1) )      GOTO 1017
C/6S
C     CALL SETERR(35H EEBSM - T1 NOT MONOTONE INCREASING,35,8,2)
C/7S
      CALL SETERR(' EEBSM - T1 NOT MONOTONE INCREASING',35,8,2)
C/
 1017 CONTINUE
      IF( I+K.LE.N1 )      GOTO 1019
      GOTO 1014
 1019 CONTINUE
      IX1005 = I+K
      IF( T1(IX1005).GT.T1(I) )      GOTO 1021
C/6S
C     CALL SETERR(35H EEBSM - T1 NOT MONOTONE INCREASING,35,8,2)
C/7S
      CALL SETERR(' EEBSM - T1 NOT MONOTONE INCREASING',35,8,2)
C/
 1021 CONTINUE
 1014 CONTINUE
 1015 CONTINUE
      I21025 = N2-1
      DO 1023 I = 1, I21025
      IF( T2(I).LE.T2(I+1) )      GOTO 1026
C/6S
C     CALL SETERR(35H EEBSM - T2 NOT MONOTONE INCREASING,35,9,2)
C/7S
      CALL SETERR(' EEBSM - T2 NOT MONOTONE INCREASING',35,9,2)
C/
 1026 CONTINUE
      IF( I+K.LE.N2 )      GOTO 1028
      GOTO 1023
 1028 CONTINUE
      IX1005 = I+K
      IF( T2(IX1005).GT.T2(I) )      GOTO 1030
C/6S
C     CALL SETERR(35H EEBSM - T2 NOT MONOTONE INCREASING,35,9,2)
C/7S
      CALL SETERR(' EEBSM - T2 NOT MONOTONE INCREASING',35,9,2)
C/
 1030 CONTINUE
C
 1023 CONTINUE
 1024 CONTINUE
      I21034 = NX-1
      DO 1032 I = 1, I21034
      IF( X(I).LE.X(I+1) )      GOTO 1035
C/6S
C           CALL SETERR(37H EEBSM - X IS NOT MONOTONE INCREASING,37,10,2
C    *)
C/7S
            CALL SETERR(' EEBSM - X IS NOT MONOTONE INCREASING',37,10,2
     *)
C/
 1035 CONTINUE
C
 1032 CONTINUE
 1033 CONTINUE
      CALL ENTER(1)
C
      IXEST = ISTKGT(MEST,3)
C
      IF( MEST.LE.1 )      GOTO 1037
      CALL UMD(-4.0E0*ATAN(1.0E0),0.0E0,MEST,WS(IXEST))
      GOTO 1038
 1037 CONTINUE
      WS(IXEST) = -2.0E0*ATAN(1.0E0)
C
 1038 CONTINUE
      IF( MEST.LE.1 )      GOTO 1039
      WS(IXEST) = 0.5E0*(WS(IXEST)+WS(IXEST+1))
 1039 CONTINUE
      IF( MEST.LE.2 )      GOTO 1041
      IX1003 = IXEST+MEST-2
      IX1004 = IXEST+MEST-1
      IX1001 = IXEST+MEST-1
      WS(IX1001) = 0.5E0*(WS(IX1003)+WS(IX1004))
C
C
 1041 CONTINUE
      DO 1043 I = 1, MEST
      IX1005 = IXEST+I-1
      IX1001 = IXEST+I-1
      WS(IX1001) = COS(WS(IX1005))
C
 1043 CONTINUE
 1044 CONTINUE
            VALUE = EEBSP(K,T1,N1,A1,T2,N2,A2,X,NX,WS(IXEST),MEST,EREST1
     *,EREST2)
C
      CALL LEAVE
C
      EEBSM = VALUE
      RETURN
C
      END
