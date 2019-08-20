      SUBROUTINE DZ1VM(N, X, QT, R, AUX, Y)
      INTEGER N
      DOUBLE PRECISION X(N), QT(N, N), R(N, N), AUX(N), Y(N)
      INTEGER J
      DOUBLE PRECISION DDOT
C MULTIPLY VECTOR X-TRANSPOSE TIMES MATRIX A
C A = Q*R
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT
C R IS UPPER TRIANGULAR
C SCRATCH SPACE AUX
C/6S
C     IF (N .LT. 1) CALL SETERR(14HDZ1VM - N.LT.1, 15, 1, 2)
C/7S
      IF (N .LT. 1) CALL SETERR('DZ1VM - N.LT.1', 15, 1, 2)
C/
      DO  1 J = 1, N
         AUX(J) = DDOT(N, QT(J, 1), N, X(1), 1)
   1     CONTINUE
      DO  2 J = 1, N
         Y(J) = DDOT(J, AUX(1), 1, R(1, J), 1)
   2     CONTINUE
      RETURN
      END
