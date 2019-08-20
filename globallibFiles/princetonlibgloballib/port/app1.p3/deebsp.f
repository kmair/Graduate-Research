      DOUBLE PRECISION FUNCTION DEEBSP(K,T1,N1,A1,T2,N2,A2,X,NX,XE
     *ST,MEST,EREST1,EREST2)
C
C
C PURPOSE - TO ESTIMATE THE ERROR IN A B-SPLINE APPROXIMATION.
C
C MNEMONIC - DOUBLE PRECISION ESTIMATION OF THE ERROR IN A B-SPLIN
C            APPROXIMATION, USING A GIVEN PATTERN IN THE SEARCH.
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
C   INTERVAL (X(I),X(I+1)), BY SEACHING AT THE PATTERN OF MEST POI
C   IN EACH T INTERVAL INTERSECTING IT, TO BE
C
C      MAX(ABS(Y2-Y1))/((H1(I)/H2(I))**K-1)
C
C   WHERE H1(I) IS DEFINED TO BE THE LENGTH OF THE LARGEST MESH IN
C   OF T1 INTERSECTING (X(I),X(I+1)). SIMILARLY FOR H2(I).
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
C   XEST - THE PATTERN ON (-1,+1) WHICH IS TO BE SCALED AND USED I
C          SEARCH FOR THE MAXIMUM DEVIATION IN EACH T MESH INTERVA
C          XEST MUST BE MONOTONE INCREASING.
C   MEST - THE LENGTH OF THE ARRAY XEST.
C
C OUTPUT -
C
C   EREST1 - EREST1(I) IS THE ERROR ESTIMATE FOR THE T1
C            APPROXIMATION ON THE I-TH X INTERVAL, I = 1,...,NX-1.
C            EREST1(I) = D1MACH(2) IS RETURNED IF T2 IS NOT A REFI
C            OF T1 ON (X(I),X(I+1)).
C            IF X(I) = X(I+1) THEN EREST1(I) IS MEANINGLESS.
C
C            EREST1(NX) = MAX(EREST1(1),...,EREST1(NX-1)).
C
C   EREST2 - EREST2(I) IS THE ERROR ESTIMATE FOR THE T2
C            APPROXIMATION ON THE I-TH X INTERVAL, I = 1,...,NX-1.
C            EREST2(I) = D1MACH(2) IS RETURNED IF T2 IS NOT A REFI
C            OF T1 ON (X(I),X(I+1)).
C            IF X(I) = X(I+1) THEN EREST2(I) IS MEANINGLESS.
C
C            EREST2(NX) = MAX(EREST2(1),...,EREST2(NX-1)).
C
C   PROCEDURE VALUE - EREST1(NX).
C
C SCRATCH SPACE ALLOCATED - 5*K + NX LONG REAL WORDS.
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
C   11 - XEST(I) NOT IN (-1,+1).
C   12 - XEST IS NOT MONOTONE INCREASING.
C
      INTEGER X2540(1)
            INTEGER ID(1),I,IADIF1,IADIF2,ISTKGT,IERR,ILO1,ILO2,ILEFT1,I
     *LEFT2,I1,I2,IXX,IX
            DOUBLE PRECISION Y1(1),Y2(1),XA(1),D1MACH,BIG,POWER,XLEFT,XR
     *IGHT
      LOGICAL LS(1000)
      INTEGER IS(1000)
      REAL RS(1000)
      DOUBLE PRECISION WS(500)
      DOUBLE PRECISION DS(500)
      COMMON /CSTAK/DS
      INTEGER K,N1,N2,NX,MEST
            DOUBLE PRECISION T1(N1),A1(1),T2(N2),A2(1),X(NX),XEST(MEST),
     *EREST1(NX),EREST2(NX)
C
      EQUIVALENCE (X2540(1),ID(1))
      EQUIVALENCE ( DS(1),WS(1),RS(1),IS(1),LS(1) )
C
      DATA X2540(1)/0/
C
C LONG REAL A1(N1-K),A2(N2-K)
C
C
C
C
C DEFINE H1 = EREST1
C DEFINE H2 = EREST2
C
      BIG = D1MACH(2)
C
C CHECK THE INPUT.
C
      IF( K.GE.2 )      GOTO 1000
C/6S
C     CALL SETERR(15HDEEBSP - K.LT.2,15,1,2)
C/7S
      CALL SETERR('DEEBSP - K.LT.2',15,1,2)
C/
 1000 CONTINUE
      IF( N1.GT.K )      GOTO 1002
C/6S
C     CALL SETERR(16HDEEBSP - N1.LE.K,16,2,2)
C/7S
      CALL SETERR('DEEBSP - N1.LE.K',16,2,2)
C/
 1002 CONTINUE
      IF( N2.GT.K )      GOTO 1004
C/6S
C     CALL SETERR(16HDEEBSP - N2.LE.K,16,3,2)
C/7S
      CALL SETERR('DEEBSP - N2.LE.K',16,3,2)
C/
 1004 CONTINUE
      IF( NX.GE.2 )      GOTO 1006
C/6S
C     CALL SETERR(16HDEEBSP - NX.LT.2,16,4,2)
C/7S
      CALL SETERR('DEEBSP - NX.LT.2',16,4,2)
C/
 1006 CONTINUE
      IF( MEST.GE.1 )      GOTO 1008
C/6S
C     CALL SETERR(18HDEEBSP - MEST.LT.1,18,5,2)
C/7S
      CALL SETERR('DEEBSP - MEST.LT.1',18,5,2)
C/
C
 1008 CONTINUE
      IF( T1(1).EQ.T2(1).AND.T1(N1).EQ.T2(N2) )      GOTO 1010
C/6S
C           CALL SETERR(45HDEEBSP - (T1(1),T1(N1)) IS NOT (T2(1),T2(N2))
C    *,45,6,2)
C/7S
            CALL SETERR('DEEBSP - (T1(1),T1(N1)) IS NOT (T2(1),T2(N2))'
     *,45,6,2)
C/
C
 1010 CONTINUE
      IF( X(1).GE.T1(1).AND.X(NX).LE.T1(N1) )      GOTO 1012
C/6S
C           CALL SETERR(48HDEEBSP - (X(1),X(NX)) NOT PART OF (T1(1),T1(N
C    *1)),48,7,2)
C/7S
            CALL SETERR('DEEBSP - (X(1),X(NX)) NOT PART OF (T1(1),T1(N1)
     *)',48,7,2)
C/
C
 1012 CONTINUE
      I21016 = N1-1
      DO 1014 I = 1, I21016
      IF( T1(I).LE.T1(I+1) )      GOTO 1017
C/6S
C     CALL SETERR(35HDEEBSP - T1 NOT MONOTONE INCREASING,35,8,2)
C/7S
      CALL SETERR('DEEBSP - T1 NOT MONOTONE INCREASING',35,8,2)
C/
 1017 CONTINUE
      IF( I+K.LE.N1 )      GOTO 1019
      GOTO 1014
 1019 CONTINUE
      IX1005 = I+K
      IF( T1(IX1005).GT.T1(I) )      GOTO 1021
C/6S
C     CALL SETERR(35HDEEBSP - T1 NOT MONOTONE INCREASING,35,8,2)
C/7S
      CALL SETERR('DEEBSP - T1 NOT MONOTONE INCREASING',35,8,2)
C/
 1021 CONTINUE
 1014 CONTINUE
 1015 CONTINUE
      I21025 = N2-1
      DO 1023 I = 1, I21025
      IF( T2(I).LE.T2(I+1) )      GOTO 1026
C/6S
C     CALL SETERR(35HDEEBSP - T2 NOT MONOTONE INCREASING,35,9,2)
C/7S
      CALL SETERR('DEEBSP - T2 NOT MONOTONE INCREASING',35,9,2)
C/
 1026 CONTINUE
      IF( I+K.LE.N2 )      GOTO 1028
      GOTO 1023
 1028 CONTINUE
      IX1005 = I+K
      IF( T2(IX1005).GT.T2(I) )      GOTO 1030
C/6S
C     CALL SETERR(35HDEEBSP - T2 NOT MONOTONE INCREASING,35,9,2)
C/7S
      CALL SETERR('DEEBSP - T2 NOT MONOTONE INCREASING',35,9,2)
C/
 1030 CONTINUE
C
 1023 CONTINUE
 1024 CONTINUE
      I21034 = NX-1
      DO 1032 I = 1, I21034
      IF( X(I).LE.X(I+1) )      GOTO 1035
C/6S
C           CALL SETERR(37HDEEBSP - X IS NOT MONOTONE INCREASING,37,10,2
C    *)
C/7S
            CALL SETERR('DEEBSP - X IS NOT MONOTONE INCREASING',37,10,2
     *)
C/
 1035 CONTINUE
C
 1032 CONTINUE
 1033 CONTINUE
      DO 1037 I = 1, MEST
            IF( XEST(I).GE.(-1.0D0).AND.XEST(I).LE.(+1.0D0) )      GOTO
     *1040
C/6S
C     CALL SETERR(31HDEEBSP - XEST(I) NOT IN (-1,+1),31,11,2)
C/7S
      CALL SETERR('DEEBSP - XEST(I) NOT IN (-1,+1)',31,11,2)
C/
 1040 CONTINUE
      IF( I.NE.MEST )      GOTO 1042
      GOTO 1037
C
 1042 CONTINUE
      IF( XEST(I).LE.XEST(I+1) )      GOTO 1044
C/6S
C           CALL SETERR(40HDEEBSP - XEST IS NOT MONOTONE INCREASING,40,1
C    *2,2)
C/7S
            CALL SETERR('DEEBSP - XEST IS NOT MONOTONE INCREASING',40,1
     *2,2)
C/
 1044 CONTINUE
C
 1037 CONTINUE
 1038 CONTINUE
      CALL ENTER(1)
C
C ALLOCATE SCRATCH SPACE FOR CALLS TO DSPLN2.
C
      IADIF1 = ISTKGT(2*K,4)
      IADIF2 = IADIF1+K
C
      IERR = ISTKGT(NX,4)
C THE DEVIATION ON EACH T INTERVAL.
C
      CALL SETD(NX,0.0D0,EREST1)
C H1 == 0.
      CALL SETD(NX,0.0D0,EREST2)
C H2 == 0.
      CALL SETD(NX,0.0D0,WS(IERR))
C
C INITIALIZE PARAMETERS FOR DSPLN2.
C
      ILO1 = 0
      ILO2 = 0
      ILEFT1 = 1
      ILEFT2 = 1
C
      I1 = 1
C CURRENT RIGHT-MOST T1 AND
      I2 = 1
C T2 MESH POINTS.
C
      IX = 1
C CURRENT LEFT-MOST X INTERVAL.
C
C GET H1, H2 AND THE DEVIATION OVER EACH X INTERVAL.
C
      XLEFT = X(1)
 1048 CONTINUE
      IF( XLEFT.GE.X(NX) )      GOTO 1047
 1049 CONTINUE
      IF( T1(I1).GT.XLEFT.OR.I1.GE.N1 )      GOTO 1050
      I1 = I1 + (1)
      GOTO 1049
 1050 CONTINUE
 1051 CONTINUE
      IF( T2(I2).GT.XLEFT.OR.I2.GE.N2 )      GOTO 1052
      I2 = I2 + (1)
C
      GOTO 1051
 1052 CONTINUE
      XRIGHT = DMIN1(T1(I1),T2(I2),X(NX))
C
      DO 1053 I = 1, MEST
C     SEARCH (XLEFT,XRIGHT).
      XA(1) = XLEFT+0.5D0*(XRIGHT-XLEFT)*(1.0D0+XEST(I))
      XA(1) = DMAX1(XLEFT,DMIN1(XA(1),XRIGHT))
C
            CALL DSPLN2(K,T1,N1,A1,XA,1,ID,1,Y1,K,WS(IADIF1),ILO1,ILEFT1
     *)
            CALL DSPLN2(K,T2,N2,A2,XA,1,ID,1,Y2,K,WS(IADIF2),ILO2,ILEFT2
     *)
C
C     UPDATE ALL X INTERVALS CONTAINING XA(1).
C
      IXX = IX
 1058 CONTINUE
      IF( X(IXX).GT.XA(1).OR.IXX.GE.NX )      GOTO 1057
      IF( X(IXX+1).GE.XA(1) )      GOTO 1059
      IX = IXX
      GOTO 1056
C
 1059 CONTINUE
      EREST1(IXX) = DMAX1(EREST1(IXX),T1(ILEFT1+1)-T1(ILEFT1))
      EREST2(IXX) = DMAX1(EREST2(IXX),T2(ILEFT2+1)-T2(ILEFT2))
C
      IX1005 = IERR+IXX-1
      IX1001 = IERR+IXX-1
      WS(IX1001) = DMAX1(WS(IX1005),DABS(Y1(1)-Y2(1)))
 1056 CONTINUE
      IXX = IXX + (1)
      GOTO 1058
 1057 CONTINUE
 1053 CONTINUE
 1054 CONTINUE
C
C NOW GET THE ERROR ESTIMATES.
C
 1046 CONTINUE
      XLEFT = XRIGHT
      GOTO 1048
 1047 CONTINUE
      I21063 = NX-1
      DO 1061 I = 1, I21063
            IF( EREST1(I).LE.EREST2(I).OR.EREST2(I).LE.0.0D0 )      GOTO
     * 1064
      POWER = (EREST1(I)/EREST2(I))**(K)
C
      IX1002 = IERR+I-1
      EREST2(I) = WS(IX1002)/(POWER-1.0D0)
      EREST1(I) = EREST2(I)*POWER
C
      EREST1(NX) = DMAX1(EREST1(NX),EREST1(I))
      EREST2(NX) = DMAX1(EREST2(NX),EREST2(I))
      GOTO 1065
 1064 CONTINUE
      EREST1(I) = BIG
      EREST2(I) = BIG
      EREST1(NX) = BIG
      EREST2(NX) = BIG
 1065 CONTINUE
C
 1061 CONTINUE
 1062 CONTINUE
      CALL LEAVE
C
      DEEBSP = EREST1(NX)
      RETURN
C
      END