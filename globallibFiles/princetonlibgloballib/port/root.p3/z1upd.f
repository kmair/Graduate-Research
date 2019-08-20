      SUBROUTINE Z1UPD(N, QT, R, U, V, AUX)
      INTEGER N
      REAL QT(N, N), R(N, N), U(N), V(N), AUX(N)
      INTEGER J, K
      REAL SC, SS, SDOT
C RANK-ONE UPDATE OF QR FACTORIZATION
C INPUT MATRIX A=Q*R
C TO BE UPDATED BY ADDING U*V-TRANSPOSE
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT
C R IS UPPER TRIANGULAR
C SCRATCH SPACE AUX
C/6S
C     IF (N .LT. 1) CALL SETERR(15H Z1UPD - N.LT.1, 15, 1, 2)
C/7S
      IF (N .LT. 1) CALL SETERR(' Z1UPD - N.LT.1', 15, 1, 2)
C/
      DO  1 J = 1, N
         AUX(J) = SDOT(N, QT(J, 1), N, U, 1)
   1     CONTINUE
      J = N
         GOTO  3
   2     J = J-1
   3     IF (J .LE. 2)GOTO  4
         CALL SROTG(AUX(J-1), AUX(J), SC, SS)
         CALL SROT(N+2-J, R(J-1, J-1), N, R(J, J-1), N, SC, SS)
         CALL SROT(N, QT(J-1, 1), N, QT(J, 1), N, SC, SS)
         GOTO  2
   4  DO  5 K = 1, N
         R(1, K) = R(1, K)+AUX(1)*V(K)
   5     CONTINUE
      IF (N .LE. 1) GOTO 10
         DO  6 K = 1, N
            R(2, K) = R(2, K)+AUX(2)*V(K)
   6        CONTINUE
         J = 1
            GOTO  8
   7        J = J+1
   8        IF (J .GE. N)GOTO  9
            CALL SROTG(R(J, J), R(J+1, J), SC, SS)
            CALL SROT(N-J, R(J, J+1), N, R(J+1, J+1), N, SC, SS)
            CALL SROT(N, QT(J, 1), N, QT(J+1, 1), N, SC, SS)
            GOTO  7
   9     CONTINUE
  10  RETURN
      END
