      SUBROUTINE S4MCE(N, R, C, A, IA, JA, IL, ANORM, COND, Z,
     1   TMP)
      INTEGER N
      INTEGER R(N), C(N), IA(1), JA(1), IL(1)
      REAL A(N), ANORM, COND, Z(N), TMP(N)
      INTEGER ICK, IDI, IEX, JUJ, IDI1, MIN0
      INTEGER I, J, K, JMIN, JMAX, JMIN1
      INTEGER JMAX1, IB, JJ, IR, KR, LASTA
      INTEGER ISIZE, NP1
      REAL BIG, ABS, WKM, SUM, S, T
      REAL SIGN, AMAX1, EK, SM, WK, GREAT
      REAL FLOAT, SASUM, YNORM, R1MACH
      LOGICAL TEMP
C THIS IS LOWER LEVEL CONDITION ESTIMATOR FOR SPMCE
C PARAMETERS ARE THE SAME AS IN SPMCE EXCEPT THAT
C ANORM -NORM OF MATRIX
C TMP - N VECTOR REAL TEMPORARY
C A,JA - ALREADY CONTAIN LU DECOMPOSITION COMPUTED BY S4MLU.
      IF (N .NE. 1) GOTO 1
         COND = 1.0
         Z(1) = 1.0
         GOTO  24
   1     EK = 1.0
         ISIZE = IA(N+1)-1
         DO  2 J = 1, N
            Z(J) = 0.0
   2        CONTINUE
         BIG = R1MACH(2)/FLOAT(N)
C
C SOLVE TRANS(U) W =E AND PUT ANSWER IN Z
         LASTA = IA(N+1)
         DO  14 K = 1, N
            ICK = C(K)
            IF (ABS(Z(ICK)) .NE. 0.E0) EK = SIGN(EK, -Z(ICK))
            IF (ABS(EK-Z(ICK)) .LE. 1.E0) GOTO 3
               S = 1.E0/ABS(EK-Z(ICK))
               CALL SSCAL(N, S, Z, 1)
               EK = S*EK
   3        WK = EK-Z(ICK)
            WKM = (-EK)-Z(ICK)
            S = ABS(WK)
            SM = ABS(WKM)
            KR = R(K)
C IN LU DECOMPOSITION, THE U PORTION OF THE ROW
C COMES BEFOR THE L  PORTION BUT IT COULD
C BE SPLIT IN 2.
            JMIN = IA(KR)
            IDI = IL(K)
            JMAX = MIN0(IA(KR+1), IDI)-1
            IF (JMIN .GT. JMAX) GOTO 13
               JMIN1 = JMIN
               JMAX1 = JMAX
   4              DO  5 J = JMIN, JMAX
                     JUJ = JA(J)
                     SM = SM+ABS(Z(JUJ)+WKM*A(J))
                     Z(JUJ) = Z(JUJ)+WK*A(J)
                     S = S+ABS(Z(JUJ))
   5                 CONTINUE
                  TEMP = IDI .LT. LASTA
                  IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA
                  IF (TEMP) GOTO  7
                  JMIN = JA(IDI)
                  JMAX = IDI-1
   6              IF (JMIN .LE. JMAX) GOTO  4
   7           IF (S .GE. SM) GOTO 12
                  T = WKM-WK
   8                 DO  9 J = JMIN1, JMAX1
                        JUJ = JA(J)
                        Z(JUJ) = Z(JUJ)+T*A(J)
   9                    CONTINUE
                     TEMP = IDI .LT. LASTA
                     IF (.NOT. TEMP) TEMP = JMIN1 .GE. LASTA
                     IF (TEMP) GOTO  11
                     JMIN1 = JA(IDI)
                     JMAX1 = IDI-1
  10                 IF (JMIN1 .LE. JMAX1) GOTO  8
  11              CONTINUE
  12           CONTINUE
  13        IF (S .LT. SM) WK = WKM
            Z(ICK) = WK
  14        CONTINUE
         S = 1.0/SASUM(N, Z, 1)
         CALL SSCAL(N, S, Z, 1)
C
C FORM Y=L(TRANSPOSE)*W
C AND PUT RESULT BACK INTO Z
C
         NP1 = N+1
         DO  21 IB = 1, N
            I = NP1-IB
            IR = R(I)
            IDI = IL(I)
C A(IDI) HAS RECIPROCAL OF DIAGONAL
            IDI1 = IL(I+1)
            JMIN = IDI+1
            IEX = JA(IDI1)-1
            IF (JMIN .EQ. IA(IR+1)) JMIN = JA(IDI)
            JMAX = IA(IR+1)-1
            IF (JMAX .LT. JMIN) JMAX = IEX
C TO AVOID OVERFLOW SCALE
            SUM = AMAX1(ABS(A(IDI)), ABS(Z(I)))
            IF (ABS(Z(I)) .LE. 1.0E0) SUM = ABS(A(IDI)*Z(I))
            S = 1.0E0
            IF (SUM .LE. 1.E0) GOTO 15
               S = 1.0/SUM
               CALL SSCAL(N, S, Z, 1)
               IF (S .EQ. 0.0E0) Z(I) = 1.0E0
  15        IF (S .NE. 0.0E0) Z(I) = Z(I)*A(IDI)
            IF (JMIN .GT. JMAX) GOTO 20
               SUM = Z(I)
  16              DO  17 J = JMIN, JMAX
                     JJ = JA(J)
                     Z(JJ) = Z(JJ)+SUM*A(J)
  17                 CONTINUE
                  IF (JMAX .EQ. IEX) GOTO  19
                  JMIN = JA(IDI)
                  JMAX = IEX
  18              IF (JMIN .LE. JMAX) GOTO  16
  19           CONTINUE
  20        CONTINUE
  21        CONTINUE
C
C PUT NORM OF Z TO 1.0
C
         S = 1.E0/SASUM(N, Z, 1)
         YNORM = 1.0
         CALL SSCAL(N, S, Z, 1)
C DO FORWARD AND BACK SOLVE
         CALL S4MFB(N, R, C, IA, JA, A, IL, Z, TMP, YNORM)
         S = 1.0/SASUM(N, Z, 1)
         YNORM = YNORM*S
         GREAT = R1MACH(2)
         IF (YNORM .GE. 1.0) GOTO 23
            TEMP = ANORM .EQ. 0.0E0
            IF (.NOT. TEMP) TEMP = ANORM .GT. YNORM*GREAT
            IF (.NOT. TEMP) GOTO 22
               COND = GREAT
               RETURN
  22     CONTINUE
  23     COND = ANORM/YNORM
  24  RETURN
      END
