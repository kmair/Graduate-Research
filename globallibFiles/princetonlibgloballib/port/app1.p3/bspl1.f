      SUBROUTINE BSPL1(K,T,N,
     1                  X,NX,ILEFT,
     2                  ID,NID,
     3                  BX)
C
C  TO OBTAIN ALL NON-ZERO BASIS SPLINES AND THEIR DERIVATIVES,
C
C           BX(IX,I,J) = B(ILEFT+I-K) SUP (ID(J)) (X(IX))
C
C  FOR IX=1,...,NX, I=1,...,K AND J=1,...,NID.
C
C  INPUT
C
C    K     - THE ORDER OF THE B-SPLINES TO BE USED.
C            2.LE.K IS ASSUMED.
C    T     - THE B-SPLINE MESH.
C    N     - THE NUMBER OF POINTS IN THE MESH T.
C    X     - POINTS OF EVALUATION FOR THE B-SPLINE.
C    NX    - THE NUMBER OF POINTS IN X.
C    ILEFT - ALL POINTS X(IX) ARE ASSUMED TO LIE IN THE SAME INTERVAL
C            (T(ILEFT),T(ILEFT+1)).
C    ID    - DERIVATIVES OF THE B-SPLINE DESIRED.
C            0.LE.ID(1).LT.ID(2).LT. ... .LT.ID(NID).LT.K IS ASSUMED.
C    NID   - THE NUMBER OF DERIVATIVES DESIRED, THE LENGTH OF ID.
C
C  OUTPUT
C
C    BX - THE BASIS SPLINES AND THEIR DERIVATIVES.
C
C  SCRATCH SPACE ALLOCATED -
C
C        MAX(3*K,K*(MIN((K+1)/2,ID(NID)+1)+1))
C
C  REAL WORDS.
C
C  ERROR STATES -
C
C     1 - K.LT.2.
C     2 - N.LE.K.
C     3 - NX.LT.1.
C     4 - ILEFT.LT.1.
C     5 - ILEFT.GE.N.
C     6 - T(ILEFT)=T(ILEFT+1).
C     7 - NID.LT.1.
C     8 - ID(1).LT.0.
C     9 - ID IS NOT MONOTONE INCREASING.
C    10 - ID(NID).GE.K.
C    11 - ALL X ARE NOT IN THE SAME ILEFT INTERVAL.
C    12 - T IS NOT MONOTONE INCREASING.
C
      REAL T(N),X(NX),BX(NX,K,NID)
      INTEGER ID(NID)
C
      LOGICAL A3PLN8
C
      COMMON /CSTAK/DS
      DOUBLE PRECISION DS(500)
      REAL WS(1)
      EQUIVALENCE (DS(1),WS(1))
C
C ... CHECK THE INPUT.
C
C/6S
C     IF (K.LT.2) CALL SETERR(15H BSPL1 - K.LT.2,15,1,2)
C     IF (N.LE.K) CALL SETERR(15H BSPL1 - N.LE.K,15,2,2)
C     IF (NX.LT.1) CALL SETERR(16H BSPL1 - NX.LT.1,16,3,2)
C     IF (ILEFT.LT.1) CALL SETERR(19H BSPL1 - ILEFT.LT.1,19,4,2)
C     IF (ILEFT.GE.N) CALL SETERR(19H BSPL1 - ILEFT.GE.N,19,5,2)
C     IF (T(ILEFT).EQ.T(ILEFT+1)) CALL SETERR
C    1  (28H BSPL1 - T(ILEFT)=T(ILEFT+1),28,6,2)
C     IF (NID.LT.1) CALL SETERR(17H BSPL1 - NID.LT.1,17,7,2)
C     IF (ID(1).LT.0) CALL SETERR(19H BSPL1 - ID(1).LT.0,19,8,2)
C/7S
      IF (K.LT.2) CALL SETERR(' BSPL1 - K.LT.2',15,1,2)
      IF (N.LE.K) CALL SETERR(' BSPL1 - N.LE.K',15,2,2)
      IF (NX.LT.1) CALL SETERR(' BSPL1 - NX.LT.1',16,3,2)
      IF (ILEFT.LT.1) CALL SETERR(' BSPL1 - ILEFT.LT.1',19,4,2)
      IF (ILEFT.GE.N) CALL SETERR(' BSPL1 - ILEFT.GE.N',19,5,2)
      IF (T(ILEFT).EQ.T(ILEFT+1)) CALL SETERR
     1  (' BSPL1 - T(ILEFT)=T(ILEFT+1)',28,6,2)
      IF (NID.LT.1) CALL SETERR(' BSPL1 - NID.LT.1',17,7,2)
      IF (ID(1).LT.0) CALL SETERR(' BSPL1 - ID(1).LT.0',19,8,2)
C/
      IF (NID.EQ.1) GO TO 20
      DO 10 I=2,NID
C/6S
C        IF (ID(I-1).GE.ID(I)) CALL SETERR
C    1       (38H BSPL1 - ID IS NOT MONOTONE INCREASING,38,9,2)
C/7S
         IF (ID(I-1).GE.ID(I)) CALL SETERR
     1       (' BSPL1 - ID IS NOT MONOTONE INCREASING',38,9,2)
C/
 10      CONTINUE
C/6S
C20   IF (ID(NID).GE.K) CALL SETERR(21H BSPL1 - ID(NID).GE.K,21,10,2)
C/7S
 20   IF (ID(NID).GE.K) CALL SETERR(' BSPL1 - ID(NID).GE.K',21,10,2)
C/
C
      DO 30 IX=1,NX
C/6S
C       IF (T(ILEFT).GT.X(IX).OR.X(IX).GT.T(ILEFT+1)) CALL SETERR
C    1    (49H BSPL1 - ALL X ARE NOT IN THE SAME ILEFT INTERVAL,49,11,2)
C       IF (X(IX).EQ.T(ILEFT+1).AND.T(ILEFT+1).LT.T(N)) CALL SETERR
C    1    (49H BSPL1 - ALL X ARE NOT IN THE SAME ILEFT INTERVAL,49,11,2)
C/7S
        IF (T(ILEFT).GT.X(IX).OR.X(IX).GT.T(ILEFT+1)) CALL SETERR
     1    (' BSPL1 - ALL X ARE NOT IN THE SAME ILEFT INTERVAL',49,11,2)
        IF (X(IX).EQ.T(ILEFT+1).AND.T(ILEFT+1).LT.T(N)) CALL SETERR
     1    (' BSPL1 - ALL X ARE NOT IN THE SAME ILEFT INTERVAL',49,11,2)
C/
 30     CONTINUE
C
C ... ALLOCATE SCRATCH SPACE.
C
      IFA=ISTKGT(MAX0(3*K,K*(MIN0((K+1)/2,ID(NID)+1)+1)),3)
C
C/6S
C     IF (.NOT.A3PLN8(K,T,N,X,NX,ILEFT,ID,NID,BX,IFA))
C    1  CALL SETERR(37H BSPL1 - T IS NOT MONOTONE INCREASING,37,12,2)
C/7S
      IF (.NOT.A3PLN8(K,T,N,X,NX,ILEFT,ID,NID,BX,IFA))
     1  CALL SETERR(' BSPL1 - T IS NOT MONOTONE INCREASING',37,12,2)
C/
C
      CALL ISTKRL(1)
C
      RETURN
C
      END