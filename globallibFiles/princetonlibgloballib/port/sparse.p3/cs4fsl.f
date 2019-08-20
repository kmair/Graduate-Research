      SUBROUTINE CS4FSL(N, R, C, IA, JA, A, IU, B, IB, NB, TMP)
      INTEGER N, IB, NB
      INTEGER R(N), C(N), IA(1), JA(1), IU(N)
      COMPLEX A(1), B(IB, NB), TMP(N)
      INTEGER IIB, IIC, IUI, JUJ, I, J
      INTEGER K, JMIN, JMAX, JJ, IR, NP1
      COMPLEX SUM, DK
C THIS IS LOWER LEVEL SUBROUTINE FORCSPFSL
C SPARSE FORWARD SOLVE
      NP1 = N+1
      DO  7 K = 1, NB
         DO  3 I = 1, N
            IR = R(I)
            JMIN = IA(I)
            IUI = IU(I)-1
            JMAX = IUI-1
            DK = A(IUI)
            SUM = B(IR, K)
            IF (JMIN .GT. JMAX) GOTO 2
               DO  1 J = JMIN, JMAX
                  JJ = JA(J)
                  SUM = SUM+A(J)*TMP(JJ)
   1              CONTINUE
   2        TMP(I) = SUM*DK
   3        CONTINUE
C SPARSE BACKWARD SOLVE
         DO  6 IIB = 1, N
            I = NP1-IIB
            JMIN = IU(I)
            JMAX = IA(I+1)-1
            SUM = TMP(I)
            IF (JMIN .GT. JMAX) GOTO 5
               DO  4 J = JMIN, JMAX
                  JUJ = JA(J)
                  SUM = SUM-A(J)*B(JUJ, K)
   4              CONTINUE
   5        IIC = C(I)
            B(IIC, K) = SUM
   6        CONTINUE
   7     CONTINUE
      RETURN
      END
