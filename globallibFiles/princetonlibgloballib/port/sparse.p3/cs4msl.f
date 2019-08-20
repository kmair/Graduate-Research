      SUBROUTINE CS4MSL(N, R, C, IA, JA, A, IL, B, IB, NB, TMP)
      INTEGER N, IB, NB
      INTEGER R(N), C(N), IA(1), JA(1), IL(N)
      COMPLEX A(1), B(IB, NB), TMP(N)
      INTEGER IIB, IIC, IDI, IEX, JUJ, IDI1
      INTEGER MIN0, I, J, K, JMIN, JMAX
      INTEGER JJ, IR, RI, LASTA, NP1
      COMPLEX SUM, DK
      LOGICAL TEMP
C SPARSE MATRIX SOLUTION
C INPUT
C N ORDER OF PROBLEM
C R ROW PERMUTATION
C C COLUMN PERMUTATION
C IA INTEGER VECTOR, LENGTH N+1 POINTING TO BEGINNING OF ROW IN JA AND A
C JA COLUMN INDICES CORRESPONDING TO NONZERO ELEMENTS IN A
C A  COMPLEX VECTOR OF NONZERO ELEMENTS IN LU DECOMPOSTION
C IL INTEGER VECTOR LENGTH N+1 POINTING TO BEGINNING OF EACH L ROW
C    IN A AND JA. COMPUTED BY SPMLU
C B RIGHT-HAND SIDE
C IB ROW DIMENSION OF B
C NB NUMBER OF RIGHT HAND SIDES
C SCRATCH VECTOR -TMP COMPLEX VECTOR LENGTH N
C OUTPUT
C B   SOLUTION TO PROBLEM
C THIS SUBROUTINE ASSUME EACH ROW CAN BE AT MOST TWO SEGMENTS
C IL(I) POINTS TO DIAGONAL REALLY AND JA(IL(I)) INDICATES
C WHERE EXTRA SPACE IS NEEDED FOR THAT ROW. EACH ROW
C LOOKS LIKE U,DIAGONAL,L
      LASTA = IA(N+1)
      NP1 = N+1
      DO  11 K = 1, NB
C SPARSE FORWARD SOLVE
         DO  4 I = 1, N
            IR = R(I)
            IDI = IL(I)
            IDI1 = IL(I+1)
            JMIN = IDI+1
C DETERMINE WHERE FIRST PART OF L IS - IN FIRST OR SECOND
C SEGMENT
            IEX = JA(IDI1)-1
            IF (JMIN .EQ. IA(IR+1)) JMIN = JA(IDI)
            JMAX = IA(IR+1)-1
            IF (JMAX .LT. JMIN) JMAX = IEX
            DK = A(IDI)
            SUM = B(IR, K)
   1        IF (JMIN .GT. JMAX) GOTO  3
C IS THERE ANY PART OF L IN EXTRA SEGMENT
               DO  2 J = JMIN, JMAX
                  JJ = JA(J)
                  SUM = SUM+A(J)*TMP(JJ)
   2              CONTINUE
               IF (JMAX .EQ. IEX) GOTO  3
               JMIN = JA(IDI)
               JMAX = IEX
               GOTO  1
   3        TMP(I) = SUM*DK
   4        CONTINUE
C SPARSE BACK SOLVE
         DO  10 IIB = 1, N
            I = NP1-IIB
            RI = R(I)
            IDI = IL(I)
            JMIN = IA(RI)
C GO UNTIL EITHER END OF U IS FOUND OR REACH END
C OF FIRST SEGMENT
            JMAX = MIN0(IA(RI+1), IDI)-1
            SUM = TMP(I)
            IF (JMIN .GT. JMAX) GOTO 9
   5              DO  6 J = JMIN, JMAX
                     JUJ = JA(J)
                     SUM = SUM-A(J)*B(JUJ, K)
   6                 CONTINUE
C CHECK IF THERE IS ANY OF U IN SECOND SEGMENT
                  TEMP = IDI .LT. LASTA
                  IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA
                  IF (TEMP) GOTO  8
                  JMIN = JA(IDI)
                  JMAX = IDI-1
   7              IF (JMIN .LE. JMAX) GOTO  5
   8        CONTINUE
   9        IIC = C(I)
            B(IIC, K) = SUM
  10        CONTINUE
  11     CONTINUE
      RETURN
      END