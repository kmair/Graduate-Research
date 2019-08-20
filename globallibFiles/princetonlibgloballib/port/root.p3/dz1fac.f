      SUBROUTINE DZ1FAC(N, A, QT)
      INTEGER N
      DOUBLE PRECISION A(N, N), QT(N, N)
      INTEGER J, K
      DOUBLE PRECISION SC, SS
C DECOMPOSE N BY N MATRIX A INTO Q*R
C Q IS N BY N ORTHOGONAL MATRIX
C R IS N BY N UPPER-TRIANGULAR MATRIX
C A IS OVERWRITTEN BY R.
C TRANSPOSE OF Q IS RETURNED IN QT.
C/6S
C     IF (N .LT. 1) CALL SETERR(15HDZ1FAC - N.LT.1, 15, 1, 2)
C/7S
      IF (N .LT. 1) CALL SETERR('DZ1FAC - N.LT.1', 15, 1, 2)
C/
      DO  2 J = 1, N
         DO  1 K = 1, N
            QT(J, K) = 0.0D0
   1        CONTINUE
         QT(J, J) = 1.
   2     CONTINUE
      K = 1
         GOTO  4
   3     K = K+1
   4     IF (K .GE. N)GOTO  8
         J = N
            GOTO  6
   5        J = J-1
   6        IF (J .LE. K)GOTO  7
            CALL DROTG(A(J-1, K), A(J, K), SC, SS)
            CALL DROT(N-K, A(J-1, K+1), N, A(J, K+1), N, SC, SS)
            CALL DROT(N, QT(J-1, 1), N, QT(J, 1), N, SC, SS)
            GOTO  5
   7     GOTO  3
   8  RETURN
      END
