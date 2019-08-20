      SUBROUTINE  SYM(N, X)
      INTEGER N, MID, JJ, J
      REAL X(1)
C
C MAKE X ARRAY SYMMETRIC FRONT-TO-BACK
C
C/6S
C     IF (N .LT. 0) CALL SETERR(16H  SYM - N .LT. 0, 16, 1, 2)
C/7S
      IF (N .LT. 0) CALL SETERR('  SYM - N .LT. 0', 16, 1, 2)
C/
      IF (N .LE. 1) RETURN
C
      MID = N/2
      JJ = N
      DO 10 J = 1, MID
         X(JJ) = X(JJ) + 0.5E0*(X(J)-X(JJ))
         X(J) = X(JJ)
         JJ = JJ-1
   10 CONTINUE
      RETURN
      END
