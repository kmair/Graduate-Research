      INTEGER FUNCTION IDUMB(A,B,NAB,K,NX)
C
C  TO SET UP THE MESH X(1),...,X(NX) ON (A,B) WHICH INCLUDES THE
C  POINTS A + (I-1)*((B-A)/(NAB-1)), I=1,...,NAB WITH MULTIPLICITIES
C  K FOR I=1, 1 FOR 1.LT.I.LT.NAB AND K FOR I=NAB.
C
C  INPUT -
C
C    A   - LEFT HAND END POINT.
C    B   - RIGHT HAND END POINT.
C    NAB - NUMBER OF POINTS TO BE USED ON (A,B).
C    K   - THE B-SPLINE ORDER FOR THE MESH.
C
C  OUTPUT -
C
C    NX    - THE NUMBER OF MESH POINTS IN X,
C
C               NX = 2*(K-1) + NAB.
C
C    IDUMB - THE POINTER TO THE MESH OF LENGTH NX IN THE STACK.
C
C  SCRATCH SPACE ALLOCATED - NX DOUBLE PRECISION LOCATIONS
C                            ARE LEFT ON THE STACK.
C
C  ERROR STATES -
C
C    1 - A=B.
C    2 - NAB.LT.2.
C    3 - K.LT.1.
C
      DOUBLE PRECISION A,B
C
C ... CHECK THE INPUT.
C
C/6S
C     IF (A.EQ.B) CALL SETERR(11HIDUMB - A=B,11,1,2)
C     IF (NAB.LT.2) CALL SETERR(16HIDUMB - NAB.LT.2,16,2,2)
C     IF (K.LT.1) CALL SETERR(14HIDUMB - K.LT.1,14,3,2)
C/7S
      IF (A.EQ.B) CALL SETERR('IDUMB - A=B',11,1,2)
      IF (NAB.LT.2) CALL SETERR('IDUMB - NAB.LT.2',16,2,2)
      IF (K.LT.1) CALL SETERR('IDUMB - K.LT.1',14,3,2)
C/
C
      IDUMB=IDUM1(A,B,NAB,K,1,K,NX)
C
      RETURN
C
      END
