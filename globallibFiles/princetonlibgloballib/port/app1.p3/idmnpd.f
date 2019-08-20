      INTEGER FUNCTION IDMNPD(X,NX,N,NT)
C
C
C TO CREATE A MESH FROM AN ARRAY OF FITTING POINTS.
C
C METHOD - USE AT AT LEAST N FITTING POINTS X IN ALL MESH INTERVAL
C
C MNEMONIC - INTEGER POINTER TO THE DOUBLE PRECISION MESH
C            USING N POINTS PER INTERVAL, DISTINCT POINTS.
C
C INPUT -
C
C    X  - THE MONOTONE INCREASING ARRAY OF FITTING POINTS.
C         ANY MULTIPLICITIES IN X WILL BE IGNORED.
C    NX - THE NUMBER OF POINTS IN THE ARRAY X.
C    N  - THE NUMBER OF FITTING POINTS TO USE PER MESH INTERVAL.
C
C OUTPUT -
C
C    NT     - THE NUMBER OF POINTS IN THE MESH.
C
C             IF ( NDX .GE. 2*N-1 )
C
C               NT = 3 + (NDX-2*N+1)/(N-1)
C
C             OTHERWISE
C
C               NT = 2
C
C             WHERE NDX IS THE NUMBER OF DISTINCT POINTS IN THE AR
C    IDMNPD - THE POINTER TO THE LONG REAL MESH IN THE STACK.
C             THE MESH POINTS ARE TAKEN FROM THE ARRAY X.
C
C ERROR STATES -
C
C   1 - NX .LT. 2.
C   2 - N .LT. 2
C   3 - THE ARRAY X IS NOT MONOTONE INCREASING.
C   4 - THE NUMBER OF DISTINCT POINTS IN THE ARRAY X IS ONE.
C
C STORAGE ALLOCATED, AND LEFT ON THE STACK,
C
C          NT
C
C LONG REAL LOCATIONS.
C
      INTEGER NDX,I,IDMNPB
      LOGICAL LS(1000)
      INTEGER IS(1000)
      REAL RS(1000)
      DOUBLE PRECISION WS(500)
      DOUBLE PRECISION DS(500)
      COMMON /CSTAK/DS
      INTEGER NX,N,NT
      DOUBLE PRECISION X(NX)
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
C     CALL SETERR(16HIDMNPD - NX.LT.2,16,1,2)
C/7S
      CALL SETERR('IDMNPD - NX.LT.2',16,1,2)
C/
 1000 CONTINUE
      IF( N.GE.2 )      GOTO 1002
C/6S
C     CALL SETERR(15HIDMNPD - N.LT.2,15,2,2)
C/7S
      CALL SETERR('IDMNPD - N.LT.2',15,2,2)
C/
 1002 CONTINUE
C
      NDX = 1
C GET THE NUMBER OF DISTINCT POINTS IN X.
      DO 1004 I = 2, NX
      IF( X(I).GE.X(I-1) )      GOTO 1007
C/6S
C           CALL SETERR(37HIDMNPD - X IS NOT MONOTONE INCREASING,37,3,2)
C/7S
            CALL SETERR('IDMNPD - X IS NOT MONOTONE INCREASING',37,3,2)
C/
 1007 CONTINUE
      IF( X(I).LE.X(I-1) )      GOTO 1009
      NDX = NDX+1
 1009 CONTINUE
 1004 CONTINUE
 1005 CONTINUE
      IF( NDX.NE.1 )      GOTO 1011
C/6S
C           CALL SETERR(38HIDMNPD - ONLY ONE POINT IN THE ARRAY X,38,4,2
C    *)
C/7S
            CALL SETERR('IDMNPD - ONLY ONE POINT IN THE ARRAY X',38,4,2
     *)
C/
 1011 CONTINUE
C
      IDMNPD = IDMNPB(X,NX,N,1,NT)
      RETURN
C
      END