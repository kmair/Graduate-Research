      INTEGER FUNCTION IDLUMB(XB,NXB,N,K,NX)
C
C  TO ALLOCATE AND SET UP THE GRID X WHICH INCLUDES THE POINTS
C
C            XB(I)+(J-1)*((XB(I+1)-XB(I))/(N-1))
C
C  FOR I=1,...,NXB-1 AND J=1,...,N.
C
C  INPUT -
C
C    XB  - THE BASIC POINTS FOR DEFINING THE GRID X.
C          XB MUST BE EITHER MONOTONE INCREASING OR MONOTONE DECREASING.
C          ANY MULTIPLICITIES IN XB ARE IGNORED.
C    NXB - THE NUMBER OF POINTS IN XB.
C    N   - THE NUMBER OF POINTS TO BE USED IN THE GRID
C          X FROM XB(I) TO XB(I+1), FOR I=1,...,NXB-1.
C    K   - THE FIRST AND LAST POINTS OF THE MESH X WILL HAVE
C          MULTIPLICITY K. THE INTERIOR POINTS WILL ALL HAVE
C          MULTIPLICITY 1.
C
C  OUTPUT -
C
C    X  - X(I)=WS(I+IDLUMB-1) FOR I=1,...,NX.
C    NX - THE NUMBER OF MESH POINTS IN X.
C
C           NX = 2*K-1 + (SUM(I=1,...,NXB-1,XB(I).NE.XB(I+1))(N-1).
C
C  SCRATCH SPACE ALLOCATED - NX DOUBLE PRECISION WORDS ARE LEFT
C                            ON THE STACK.
C
C  ERROR STATES -
C
C    1 - NXB.LT.2.
C    2 - K.LT.1.
C    3 - XB IS NOT MONOTONE.
C    4 - N.LT.2.
C
      DOUBLE PRECISION XB(NXB)
      INTEGER N
C
      DOUBLE PRECISION DIR
C
C/6S
C     IF (NXB.LT.2) CALL SETERR(17HIDLUMB - NXB.LT.2,17,1,2)
C     IF (K.LT.1) CALL SETERR(15HIDLUMB - K.LT.1,15,2,2)
C/7S
      IF (NXB.LT.2) CALL SETERR('IDLUMB - NXB.LT.2',17,1,2)
      IF (K.LT.1) CALL SETERR('IDLUMB - K.LT.1',15,2,2)
C/
C
      DIR=XB(NXB)-XB(1)
C/6S
C     IF (DIR.EQ.0.0D0) CALL SETERR
C    1  (27HIDLUMB - XB IS NOT MONOTONE,27,3,2)
C/7S
      IF (DIR.EQ.0.0D0) CALL SETERR
     1  ('IDLUMB - XB IS NOT MONOTONE',27,3,2)
C/
C
      NXBM1=NXB-1
      DO 10 I=1,NXBM1
        IF (XB(I).EQ.XB(I+1)) GO TO 10
C/6S
C       IF ((DIR/DABS(DIR))*(XB(I+1)-XB(I)).LT.0.0D0) CALL SETERR
C    1    (27HIDLUMB - XB IS NOT MONOTONE,27,3,2)
C       IF (N.LT.2) CALL SETERR(15HIDLUMB - N.LT.2,15,4,2)
C/7S
        IF ((DIR/DABS(DIR))*(XB(I+1)-XB(I)).LT.0.0D0) CALL SETERR
     1    ('IDLUMB - XB IS NOT MONOTONE',27,3,2)
        IF (N.LT.2) CALL SETERR('IDLUMB - N.LT.2',15,4,2)
C/
 10     CONTINUE
C
C ... MAKE THE MESH X.
C
      IDLUMB=IDLUM1(XB,NXB,N,K,1,K,NX)
C
      RETURN
C
      END
