      SUBROUTINE DASYM(N, X)
      INTEGER N, MID, JJ, J
      DOUBLE PRECISION X(1)
C
C MAKE X ARRAY ANTISYMMETRIC FRONT-TO-BACK
C
C/6S
C     IF (N .LT. 0) CALL SETERR(16HDASYM - N .LT. 0, 16, 1, 2)
C/7S
      IF (N .LT. 0) CALL SETERR('DASYM - N .LT. 0', 16, 1, 2)
C/
      IF (N .EQ. 1 ) X(1) = 0.D0
      IF (N .LE. 1) RETURN
C
      MID = N/2
      JJ = N
      DO 10 J = 1, MID
         X(JJ) = X(JJ) - 0.5D0*(X(J)+X(JJ))
         X(J) = -X(JJ)
         JJ = JJ-1
   10 CONTINUE
      IF (JJ .GT. MID) X(JJ) = 0.D0
      RETURN
      END
