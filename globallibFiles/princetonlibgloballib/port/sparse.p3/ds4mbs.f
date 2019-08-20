      SUBROUTINE DS4MBS(N, IA, JA, A, C, B, IB, NB, TMP)
      INTEGER N, IB, NB
      INTEGER IA(1), JA(1), C(N)
      DOUBLE PRECISION A(1), B(IB, NB), TMP(N)
      INTEGER IIB, IIC, JUJ, I, J, K
      INTEGER JMIN, JMAX, NP1
      DOUBLE PRECISION SUM
C SPARSE BACK SOLVE
      NP1 = N+1
      DO  5 K = 1, NB
         DO  3 IIB = 1, N
            I = NP1-IIB
            JMIN = IA(I)
            JMAX = IA(I+1)-1
            SUM = B(I, K)
            IF (JMIN .GT. JMAX) GOTO 2
               DO  1 J = JMIN, JMAX
                  JUJ = JA(J)
                  SUM = SUM-A(J)*TMP(JUJ)
   1              CONTINUE
   2        IIC = C(I)
            TMP(IIC) = SUM
   3        CONTINUE
         DO  4 I = 1, N
            B(I, K) = TMP(I)
   4        CONTINUE
   5     CONTINUE
      RETURN
      END
