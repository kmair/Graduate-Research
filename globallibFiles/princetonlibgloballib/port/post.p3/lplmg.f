      SUBROUTINE LPLMG(NV, X, V)
      INTEGER NV
      REAL X(NV), V(NV)
      INTEGER I
C FORCE X(I-1,V) = X(I), I = 1,..., NV, FOR THE LPLM MAPS.
C/6S
C     IF (NV .LT. 2) CALL SETERR(18H LPLMG - NV .LT. 2, 18, 1, 2)
C/7S
      IF (NV .LT. 2) CALL SETERR(' LPLMG - NV .LT. 2', 18, 1, 2)
C/
      DO  1 I = 2, NV
C/6S
C        IF (X(I) .LE. X(I-1)) CALL SETERR(
C    1      46H LPLMG - X IS NOT STRICTLY MONOTONE INCREASING, 46, 2, 2)
C/7S
         IF (X(I) .LE. X(I-1)) CALL SETERR(
     1      ' LPLMG - X IS NOT STRICTLY MONOTONE INCREASING', 46, 2, 2)
C/
   1     CONTINUE
C GET V.
      DO  2 I = 1, NV
         V(I) = X(I)
   2     CONTINUE
      RETURN
      END
