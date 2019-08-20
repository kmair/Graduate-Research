      SUBROUTINE LPLMI(NV, V, X, NX, XI)
      INTEGER NV, NX
      REAL V(NV), X(NX), XI(NX)
      INTEGER I, IX
      REAL FLOAT
      LOGICAL TEMP
C GET XI(X) FROM X(XI) = X.
C/6S
C     IF (NV .LT. 2) CALL SETERR(18H LPLMI - NV .LT. 2, 18, 1, 2)
C     IF (NX .LT. 1) CALL SETERR(18H LPLMI - NX .LT. 1, 18, 2, 2)
C/7S
      IF (NV .LT. 2) CALL SETERR(' LPLMI - NV .LT. 2', 18, 1, 2)
      IF (NX .LT. 1) CALL SETERR(' LPLMI - NX .LT. 1', 18, 2, 2)
C/
      DO  4 IX = 1, NX
         I = 1
            GOTO  2
   1        I = I+1
   2        IF (I .GE. NV) GOTO  3
C FIND X(IX) IN RANGE OF X.
            TEMP = X(IX) .GE. V(I)
            IF (TEMP) TEMP = X(IX) .LE. V(I+1)
            IF (TEMP) GOTO  3
            GOTO  1
   3     IF (I .GE. NV) I = NV-1
C X NOT IN RANGE, USE LAST INTERVAL.
         XI(IX) = (X(IX)-V(I))/(V(I+1)-V(I))+FLOAT(I)-1.
   4     CONTINUE
      RETURN
      END
