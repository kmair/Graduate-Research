      INTEGER FUNCTION IMNP1(X,NX,NL,NI,NR,K,NT)
C
C
C TO CREATE A B-SPLINE MESH FROM AN ARRAY OF FITTING POINTS.
C
C METHOD - USE AT AT LEAST NL FITTING POINTS X IN THE FIRST MESH
C          INTERVAL, NI FITTING POINTS IN THE INTERIOR INTERVALS,
C          AND AT LEAST NR FITTING POINTS X IN THE LAST
C          MESH INTERVAL.
C
C MNEMONIC - INTEGER POINTER TO THE DOUBLE PRECISION MESH
C            USING N POINTS PER INTERVAL, FOR A B-SPLINE, LEVEL 1.
C
C INPUT -
C
C    X  - THE MONOTONE INCREASING ARRAY OF FITTING POINTS.
C         ANY MULTIPLICITIES IN X WILL BE IGNORED.
C    NX - THE NUMBER OF POINTS IN THE ARRAY X.
C    NL - THE NUMBER OF FITTING POINTS TO BE USED IN THE FIRST
C         MESH INTERVAL. MUST HAVE NL .GE. 2.
C         NL = MAX(2*NI,NI*(K+1-NDX)+2*NDX-K)/2 IS A GOOD CHOICE.
C    NI - THE NUMBER OF FITTING POINTS TO BE USED PER INTERIOR
C         MESH INTERVAL. MUST HAVE NI .GE. 2.
C         NI = K IS A GOOD CHOICE, ALTHOUGH NI = 2 IS OK.
C    NR - THE NUMBER OF FITTING POINTS TO BE USED IN THE LAST
C         MESH INTERVAL. MUST HAVE NR .GE. 2.
C         NR = MAX(2*NI,NI*(K+1-NDX)+2*NDX-K)/2 IS A GOOD CHOICE.
C    K  - THE ORDER FOR THE B-SPLINE MESH,
C         THE MULTIPLICITY FOR THE FIRST AND LAST MESH POINTS.
C
C OUTPUT -
C
C    NT     - THE NUMBER OF POINTS IN THE MESH.
C
C             IF ( NDX .GE. NL+NR-1 )
C
C               NT = 2*K + 1 +(NDX-NL-NR+1)/(NI-1)
C
C             OTHERWISE
C
C               NT = 2*K
C
C             WHERE NDX IS THE NUMBER OF DISTINCT POINTS IN THE AR
C    IMNP1 - THE POINTER TO THE REAL MESH IN THE STACK.
C             THE MESH POINTS ARE TAKEN FROM THE ARRAY X.
C
C ERROR STATES -
C
C   1 - NX .LT. 2.
C   2 - NL .LT. 2.
C   3 - NI .LT. 2.
C   4 - NR .LT. 2.
C   5 - K .LT. 1.
C   6 - THE ARRAY X IS NOT MONOTONE INCREASING.
C   7 - THE NUMBER OF DISTINCT POINTS IN THE ARRAY X IS ONE.
C
C STORAGE ALLOCATED, AND LEFT ON THE STACK,
C
C          NT
C
C REAL LOCATIONS.
C
      INTEGER NDX,I,IMNP2
      LOGICAL LS(1000)
      INTEGER IS(1000)
      REAL RS(1000)
      REAL WS(500)
      DOUBLE PRECISION DS(500)
      COMMON /CSTAK/DS
      INTEGER NX,NL,NI,NR,K,NT
      REAL X(NX)
C
      EQUIVALENCE ( DS(1),WS(1),RS(1),IS(1),LS(1) )
C
C
C
C
C
C CHECK THE INPUT FOR ERRORS.
C
      IF( NX.GE.2 )      GOTO 1000
C/6S
C     CALL SETERR(16H IMNP1 - NX.LT.2,16,1,2)
C/7S
      CALL SETERR(' IMNP1 - NX.LT.2',16,1,2)
C/
 1000 CONTINUE
      IF( NL.GE.2 )      GOTO 1002
C/6S
C     CALL SETERR(16H IMNP1 - NL.LT.2,16,2,2)
C/7S
      CALL SETERR(' IMNP1 - NL.LT.2',16,2,2)
C/
 1002 CONTINUE
      IF( NI.GE.2 )      GOTO 1004
C/6S
C     CALL SETERR(16H IMNP1 - NI.LT.2,16,3,2)
C/7S
      CALL SETERR(' IMNP1 - NI.LT.2',16,3,2)
C/
 1004 CONTINUE
      IF( NR.GE.2 )      GOTO 1006
C/6S
C     CALL SETERR(16H IMNP1 - NR.LT.2,16,4,2)
C/7S
      CALL SETERR(' IMNP1 - NR.LT.2',16,4,2)
C/
 1006 CONTINUE
      IF( K.GE.1 )      GOTO 1008
C/6S
C     CALL SETERR(15H IMNP1 - K.LT.1,15,5,2)
C/7S
      CALL SETERR(' IMNP1 - K.LT.1',15,5,2)
C/
 1008 CONTINUE
C
      NDX = 1
C GET THE NUMBER OF DISTINCT POINTS IN X.
      DO 1010 I = 2, NX
      IF( X(I).GE.X(I-1) )      GOTO 1013
C/6S
C           CALL SETERR(37H IMNP1 - X IS NOT MONOTONE INCREASING,37,6,2)
C/7S
            CALL SETERR(' IMNP1 - X IS NOT MONOTONE INCREASING',37,6,2)
C/
 1013 CONTINUE
      IF( X(I).LE.X(I-1) )      GOTO 1015
      NDX = NDX+1
 1015 CONTINUE
 1010 CONTINUE
 1011 CONTINUE
      IF( NDX.NE.1 )      GOTO 1017
C/6S
C           CALL SETERR(38H IMNP1 - ONLY ONE POINT IN THE ARRAY X,38,7,2
C    *)
C/7S
            CALL SETERR(' IMNP1 - ONLY ONE POINT IN THE ARRAY X',38,7,2
     *)
C/
 1017 CONTINUE
C
      IMNP1 = IMNP2(X,NX,NL,NI,NR,K,1,K,NT)
      RETURN
C
      END
