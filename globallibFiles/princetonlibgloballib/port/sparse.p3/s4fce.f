      SUBROUTINE S4FCE(N, IA, JA, UL, IAMAX, IU, Z, ANORM
     1   , COND, TMP, MCP)
      INTEGER N, IAMAX
      INTEGER IA(1), JA(1), IU(N), MCP(N)
      REAL UL(IAMAX), Z(N), ANORM, COND, TMP(N)
      INTEGER IIC, ICT, IUI, JUJ, I, J
      INTEGER K, JMIN, JMAX, KB, KC, ID
      INTEGER JJ, KP1, NP1
      REAL BIG, ABS, WKM, SUM, S, T
      REAL SIGN, SQRT, AMAX1, EK, DK, SC
      REAL SM, WK, GREAT, FLOAT, SASUM, YNORM
      REAL R1MACH
      LOGICAL TEMP
C THIS SUBROUTINE DETERMINES A LOWER BOUND ON THE CONDITION NUMBER
C OF THE DECOMPOSED MATRIX A VIA THE ALGORITHM USED IN LINPACK
C
C
C SOLVE A(TRANSPOSE)W = E
C WHERE E IS CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH
C IN THE COMPONENTS OF W
      NP1 = N+1
      ICT = IA(N+1)-1
      IF (N .NE. 1) GOTO 1
         COND = 1.0
         Z(1) = 1.0
         GOTO  26
   1     EK = 1.0
C
C SOLVE U(TRANS) Y =E
C
         BIG = SQRT(R1MACH(2))/FLOAT(N)
         DO  2 J = 1, N
            Z(J) = 0.0
   2        CONTINUE
         DO  9 K = 1, N
            KC = MCP(K)
C        DIAG=UL(IU(K)-1)
            IF (ABS(Z(KC)) .NE. 0.0) EK = SIGN(EK, -Z(KC))
            IF (ABS(EK-Z(KC)) .LE. 1.0) GOTO 3
               S = 1.0/ABS(EK-Z(KC))
               CALL SSCAL(N, S, Z, 1)
               EK = S*EK
   3        WK = EK-Z(KC)
            WKM = (-EK)-Z(KC)
            S = ABS(WK)
            SM = ABS(WKM)
            KP1 = K+1
            IF (KP1 .GT. N) GOTO 8
               JMIN = IU(K)
               JMAX = IA(K+1)-1
               IF (JMAX .LT. JMIN) GOTO 7
                  DO  4 J = JMIN, JMAX
                     JUJ = JA(J)
                     SM = SM+ABS(Z(JUJ)+WKM*UL(J))
                     Z(JUJ) = Z(JUJ)+UL(J)*WK
                     S = S+ABS(Z(JUJ))
   4                 CONTINUE
                  IF (S .GE. SM) GOTO 6
                     T = WKM-WK
                     WK = WKM
                     DO  5 J = JMIN, JMAX
                        JUJ = JA(J)
                        Z(JUJ) = Z(JUJ)+T*UL(J)
   5                    CONTINUE
   6              CONTINUE
   7           CONTINUE
   8        Z(KC) = WK
   9        CONTINUE
         S = 1.0/SASUM(N, Z, 1)
         CALL SSCAL(N, S, Z, 1)
C
C SOLVE Y = L(TRANSPOSE) * W
C
         DO  13 KB = 1, N
            K = N+1-KB
            ID = IU(K)-1
C UL(ID) CONTAINS THE RECIPROCAL OF THE DIAGONAL OF U
C TRY TO AVOID OVERFLOW
            SUM = AMAX1(ABS(Z(K)), ABS(UL(ID)))
            IF (ABS(Z(K)) .LE. 1.0) SUM = ABS(Z(K)*UL(ID))
            S = 1.0E0
            JMIN = IA(K)
            JMAX = ID-1
            IF (SUM .LT. 1.0) GOTO 10
               S = 1./SUM
               CALL SSCAL(N, S, Z, 1)
  10        SUM = Z(K)*UL(ID)
            IF (S .EQ. 0.0E0) Z(K) = 1.0
            IF (JMAX .LT. JMIN) GOTO 12
               DO  11 J = JMIN, JMAX
                  JUJ = JA(J)
                  Z(JUJ) = Z(JUJ)+UL(J)*SUM
  11              CONTINUE
  12        Z(K) = SUM
  13        CONTINUE
         S = 1.0/SASUM(N, Z, 1)
         CALL SSCAL(N, S, Z, 1)
         YNORM = 1.0
C
C SOLVE L X =W
C
         DO  17 I = 1, N
            JMIN = IA(I)
            IUI = IU(I)-1
            JMAX = IUI-1
            DK = UL(IUI)
            SUM = Z(I)
            IF (JMIN .GT. JMAX) GOTO 15
               DO  14 J = JMIN, JMAX
                  JJ = JA(J)
                  SUM = SUM+UL(J)*Z(JJ)
  14              CONTINUE
  15        SC = AMAX1(ABS(SUM), ABS(DK))
            IF (ABS(SUM) .LE. 1.0) SC = ABS(SUM*DK)
            S = 1.0E0
            IF (SC .LE. 1.0) GOTO 16
               S = 1./SC
               CALL SSCAL(N, S, Z, 1)
               YNORM = YNORM*S
               SUM = SUM*S
  16        SUM = SUM*DK
            IF (S .EQ. 0.0E0) SUM = 1.0
            Z(I) = SUM
  17        CONTINUE
         S = 1.0/SASUM(N, Z, 1)
         IF (S .GT. 1.0) GOTO 18
            CALL SSCAL(N, S, Z, 1)
            YNORM = YNORM*S
C
C   SOLVE U * Z = X
  18     DO  22 KB = 1, N
            K = N+1-KB
            JMIN = IU(K)
            JMAX = IA(K+1)-1
            SUM = Z(K)
            IF (JMIN .GT. JMAX) GOTO 20
               DO  19 J = JMIN, JMAX
                  JUJ = JA(J)
                  SUM = SUM-UL(J)*TMP(JUJ)
  19              CONTINUE
  20        IIC = MCP(K)
            IF (ABS(SUM) .LE. 1.) GOTO 21
               S = 1.0/ABS(SUM)
               CALL SSCAL(N, S, TMP, 1)
               CALL SSCAL(K, S, Z, 1)
               YNORM = YNORM*S
               SUM = SUM*S
  21        TMP(IIC) = SUM
  22        CONTINUE
C    MAKE ZNORM = 1.0
         S = 1.0/SASUM(N, TMP, 1)
         CALL SSCAL(N, S, TMP, 1)
         DO  23 I = 1, N
            Z(I) = TMP(I)
  23        CONTINUE
         YNORM = YNORM*S
C
C   SET COND = ESTIMATE OF THE CONDITION NUMBER OF A
C
         GREAT = R1MACH(2)
         IF (YNORM .GT. 1.0) GOTO 25
            TEMP = ANORM .GT. YNORM*GREAT
            IF (.NOT. TEMP) TEMP = ANORM .EQ. 0.0E0
            IF (.NOT. TEMP) GOTO 24
               COND = GREAT
               RETURN
  24     CONTINUE
  25     COND = ANORM/YNORM
  26  RETURN
      END
