      SUBROUTINE DZ1SOL(N, QT, R, B, AUX, X)
      INTEGER N
      DOUBLE PRECISION QT(N, N), R(N, N), B(N), AUX(N), X(N)
      INTEGER J
      DOUBLE PRECISION BIG, D1MACH, TEMP, DDOT, DABS
C SOLVE A*X=B FOR X
C N BY N MATRIX A = Q*R
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT
C R IS UPPER TRIANGULAR
C B IS GIVEN VECTOR
C SCRATCH SPACE AUX
C/6S
C     IF (N .LT. 1) CALL SETERR(15HDZ1SOL - N.LT.1, 15, 1, 2)
C/7S
      IF (N .LT. 1) CALL SETERR('DZ1SOL - N.LT.1', 15, 1, 2)
C/
      DO  1 J = 1, N
         AUX(J) = DDOT(N, QT(J, 1), N, B(1), 1)
   1     CONTINUE
      BIG = 0.5D0*D1MACH(2)
      J = N
         GOTO  3
   2     J = J-1
   3     IF (J .LE. 0)GOTO  6
         TEMP = AUX(J)
         IF (J .LT. N) TEMP = TEMP - DDOT(N-J, X(J+1), 1, R(J, J+1), N)
         IF (DABS(R(J, J)) .GE. 1.0D0) GOTO 5
            IF (DABS(TEMP) .LE. BIG*DABS(R(J, J))) GOTO 4
C/6S
C              CALL SETERR(24HDZ1SOL - SINGULAR MATRIX, 24, 2, 1)
C/7S
               CALL SETERR('DZ1SOL - SINGULAR MATRIX', 24, 2, 1)
C/
               RETURN
   4     CONTINUE
   5     X(J) = TEMP/R(J, J)
         GOTO  2
   6  RETURN
      END
