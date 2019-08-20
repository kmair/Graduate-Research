      SUBROUTINE DM1THB(M, N, U, IU, Z, BETA, D)
      INTEGER N, IU
      INTEGER M
      DOUBLE PRECISION U(IU, N), Z(N, N), BETA(N), D(N)
      INTEGER I, J, K, IP1, NP1
      DOUBLE PRECISION BETAI, S
C CALCULATE U
      I = N
      NP1 = N+1
   1     IF (I .LE. 0) GOTO  14
         BETAI = BETA(I)
         IF (BETAI .EQ. 0.D0) GOTO 20
         IP1 = I+1
         IF (I .GE. N) GOTO 7
            IF (N .LT. M) GOTO 3
               DO  2 J = IP1, N
                  BETA(J) = 0.D0
   2              CONTINUE
               GOTO  6
   3           DO  5 J = IP1, N
                  S = 0.D0
                  DO  4 K = NP1, M
                     S = S+U(K, I)*U(K, J)
   4                 CONTINUE
                  BETA(J) = S
   5              CONTINUE
   6     CONTINUE
   7     DO  12 J = 1, N
            S = 0.D0
            DO  8 K = I, N
               S = S+Z(K, I)*U(K, J)
   8           CONTINUE
            IF (I .GE. N) GOTO 10
               DO  9 K = IP1, N
                  S = S+BETA(K)*Z(J, K)
   9              CONTINUE
  10        S = S/BETAI
            DO  11 K = I, N
               U(K, J) = U(K, J)+S*Z(K, I)
  11           CONTINUE
            D(J) = S
  12        CONTINUE
         DO  13 J = 1, N
            Z(J, I) = D(J)
  13        CONTINUE
         GO TO 22
  20     DO 21 J=1,N
            Z(J,I) = 0.D0
  21     CONTINUE
  22     I = I-1
         GOTO  1
  14  IF (N .GE. M) GOTO 19
         DO  18 I = NP1, M
            DO  16 J = 1, N
               S = 0.D0
               DO  15 K = 1, N
                  S = S+U(I, K)*Z(J, K)
  15              CONTINUE
               D(J) = S
  16           CONTINUE
            DO  17 J = 1, N
               U(I, J) = D(J)
  17           CONTINUE
  18        CONTINUE
  19  RETURN
      END
