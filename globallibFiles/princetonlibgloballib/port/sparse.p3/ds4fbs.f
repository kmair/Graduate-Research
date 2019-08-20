      SUBROUTINE DS4FBS(N, B, IB, NB, JA, A, IA, IU, IEX, C, R,
     1   ORDER, TMP)
      INTEGER N, IB, NB
      INTEGER JA(1), IA(1), IU(N), IEX(N), C(N), R(N)
      DOUBLE PRECISION B(IB, NB), A(1), TMP(N)
      LOGICAL ORDER
      INTEGER IIB, IIC, JUJ, MIN0, I, J
      INTEGER K, JMIN, JMAX, RI, NP1
      DOUBLE PRECISION SUM
      NP1 = N+1
      DO  8 K = 1, NB
C SPARSE BACK SOLVE
         DO  6 IIB = 1, N
            I = NP1-IIB
            RI = I
            IF (ORDER) RI = R(I)
            JMIN = IA(RI)
            JMAX = MIN0(IA(RI+1)-1, IU(I))
            SUM = B(I, K)
            IF (JMIN .GT. JMAX) GOTO 5
   1              DO  2 J = JMIN, JMAX
                     JUJ = JA(J)
                     SUM = SUM-A(J)*TMP(JUJ)
   2                 CONTINUE
                  IF (JMAX .EQ. IU(I)) GOTO  4
                  JMIN = IEX(I)
                  JMAX = IU(I)
   3              IF (JMIN .LE. JMAX) GOTO  1
   4        CONTINUE
   5        IIC = C(I)
            TMP(IIC) = SUM
   6        CONTINUE
         DO  7 I = 1, N
            B(I, K) = TMP(I)
   7        CONTINUE
   8     CONTINUE
      RETURN
      END
