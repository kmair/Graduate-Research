      SUBROUTINE CS4SOL(N, R,C, IA, JA, A, IU,  B, IB, NB, TMP)
      INTEGER IB, NB, N
      INTEGER R(N), IA(1), JA(1), IU(N), C(N)
      COMPLEX A(1), B(IB, NB), TMP(N)
      INTEGER JJ, IR, JMIN, JMAX, NP1, I
      INTEGER J, K, IUI
      COMPLEX DK, SUM
C SPARSE FORWARD SOLVE
      NP1 = N+1
      DO  10 K = 1, NB
         DO  5 I = 1, N
            IR = R(I)
            JMIN = IA(I)
            IUI = IU(I)-1
            JMAX=IUI-1
            DK = A(IUI)
            SUM = B(IR, K)
               IF (JMIN .GT. JMAX) GOTO 4
                  DO  2 J = JMIN, JMAX
                     JJ = JA(J)
                     SUM = SUM+A(J)*TMP(JJ)
   2                 CONTINUE
   4        TMP(I) = SUM*DK
   5        CONTINUE
         DO  9 IIB = 1, N
            I = NP1-IIB
            JMIN = IU(I)
            JMAX = IA(I+1)-1
            SUM = TMP(I)
            IF (JMIN .GT. JMAX) GOTO 8
                  DO  7 J = JMIN, JMAX
                     JUJ = JA(J)
                     SUM = SUM-A(J)*TMP(JUJ)
   7                 CONTINUE
   8        IIC = C(I)
            TMP(I) = SUM
            B(IIC,K) = SUM
   9        CONTINUE
   10     CONTINUE
      RETURN
      END
