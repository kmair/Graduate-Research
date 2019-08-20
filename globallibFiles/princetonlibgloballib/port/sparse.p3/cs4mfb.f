      SUBROUTINE CS4MFB(N, R, C, IA, JA, A, IL, B, Z, YNORM)
      INTEGER N
      INTEGER R(N), C(N), IA(1), JA(1), IL(N)
      COMPLEX A(1), B(N), Z(N)
      REAL YNORM
      INTEGER IIB, IIC, IDI, IEX, JUJ, IDI1
      INTEGER MIN0, I, J, JMIN, JMAX, JJ
      INTEGER IR, RI, LASTA, NP1
      REAL   S,  AMAX1
      COMPLEX DK,SUM,CSIGN1,Z1, Z2
      REAL SC, SCASUM
      LOGICAL TEMP
      CSIGN1(Z1,Z2)=CABS1(Z1)*(Z1/CABS1(Z2))
C SPARSE MATRIX SOLUTION
C INPUT
C N ORDER OF PROBLEM
C R ROW PERMUTATION
C C COLUMN PERMUTATION
C IA INTEGER VECTOR, LENGTH N+1 POINTING TO BEGINNING OF ROW IN JA AND A
C JA COLUMN INDICES CORRESPONDING TO NONZERO ELEMENTS IN A
C A  COMPLEX VECTOR OF NONZERO ELEMENTS IN LU DECOMPOSTION
C IL INTEGER VECTOR LENGTH N+1 POINTING TO BEGINNING OF EACH L ROW
C    IN A AND JA. COMPUTED BY CSPMLU
C B RIGHT-HAND SIDE
C OUTPUT
C Z   SOLUTION TO PROBLEM
      LASTA = IA(N+1)
      NP1 = N+1
C SPARSE FORWARD SOLVE
      DO  5 I = 1, N
         IR = R(I)
         IDI = IL(I)
         IDI1 = IL(I+1)
         JMIN = IDI+1
         IEX = JA(IDI1)-1
         IF (JMIN .EQ. IA(IR+1)) JMIN = JA(IDI)
         JMAX = IA(IR+1)-1
         IF (JMAX .LT. JMIN) JMAX = IEX
C DK HAS RECIPROCAL OF THE DIAGONAL
         DK = A(IDI)
         SUM = B(I)
   1     IF (JMIN .GT. JMAX) GOTO  3
            DO  2 J = JMIN, JMAX
               JJ = JA(J)
               SUM = SUM+A(J)*B(JJ)
   2           CONTINUE
            IF (JMAX .EQ. IEX) GOTO  3
            JMIN = JA(IDI)
            JMAX = IEX
            GOTO  1
C
C SCALE THINGS TO AVOID OVERFLOW
C
   3     SC = AMAX1(CABS1(SUM), CABS1(DK))
         IF (CABS1(SUM) .LE. 1.0) SC = CABS1(SUM*DK)
         S = 1.0E0
         IF (SC .LE. 1.0E0) GOTO 4
            S = 1.0E0/SC
            CALL CSSCAL(N, S, B, 1)
            SUM = SUM*CMPLX(S,0.0E0)
            IF (S .EQ. 0.0E0) SUM = (1.0,0.0)
            YNORM = YNORM*S
   4     IF (S .NE. 0.0E0) SUM = SUM*DK
         B(I) = SUM
   5     CONTINUE
      S = 1.0/SCASUM(N, B, 1)
      IF (S .GT. 1.0) GOTO 6
         CALL CSSCAL(N, S, B, 1)
         YNORM = S*YNORM
C SPARSE BACK SOLVE
   6  DO  13 IIB = 1, N
         I = NP1-IIB
         RI = R(I)
         IDI = IL(I)
         JMIN = IA(RI)
         JMAX = MIN0(IA(RI+1), IDI)-1
         SUM = B(I)
         IF (JMIN .GT. JMAX) GOTO 11
   7           DO  8 J = JMIN, JMAX
                  JUJ = JA(J)
                  SUM = SUM-A(J)*Z(JUJ)
   8              CONTINUE
               TEMP = IDI .LT. LASTA
               IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA
               IF (TEMP) GOTO  10
               JMIN = JA(IDI)
               JMAX = IDI-1
   9           IF (JMIN .LE. JMAX) GOTO  7
  10     CONTINUE
  11     IIC = C(I)
         IF (CABS1(SUM) .LE. 1.0E0) GOTO 12
            S = 1.0/CABS1(SUM)
            CALL CSSCAL(N, S, Z, 1)
            CALL CSSCAL(I, S, B, 1)
            YNORM = YNORM*S
            Z(IIC) = CSIGN1(CMPLX(1.0,0.0), SUM)
            SUM = SUM*S
  12     Z(IIC) = SUM
  13     CONTINUE
      DO  14 I = 1, N
         B(I) = Z(I)
  14     CONTINUE
      RETURN
      END
