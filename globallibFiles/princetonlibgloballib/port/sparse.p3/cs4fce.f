      SUBROUTINE CS4FCE(N, IA, JA, UL, IAMAX, IU, Z, ANORM
     1   , COND, TMP, MCP)
      INTEGER N, IAMAX
      INTEGER IA(1), JA(1), IU(N), MCP(N)
      COMPLEX UL(IAMAX), Z(N), TMP(N)
      REAL ANORM, COND
      INTEGER IIC, ICT, IUI, JUJ, I, J
      INTEGER K, JMIN, JMAX, KB, KC, ID
      INTEGER JJ, KP1, NP1
      REAL BIG, CABS1,  SUM, S
      REAL  SQRT, AMAX1,   SC
      REAL SM,  GREAT, FLOAT, SCASUM, YNORM
      COMPLEX WK,WKM, T,CSIGN1, EK, SCUM, DK, Z1, Z2
      REAL R1MACH
      LOGICAL TEMP
      CSIGN1(Z1,Z2)=CABS1(Z1)*(Z1/CABS1(Z2))
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
         Z(1) = (1.0,0.0)
         GOTO  26
   1     EK = (1.0E0,0.0E0)
C
C SOLVE U(TRANS) Y =E
C
         BIG = SQRT(R1MACH(2))/FLOAT(N)
         DO  2 J = 1, N
            Z(J) = (0.0,0.0)
   2        CONTINUE
         DO  9 K = 1, N
            KC = MCP(K)
C        DIAG=UL(IU(K)-1)
            IF (CABS1(Z(KC)) .NE. 0.0) EK = CSIGN1(EK, -Z(KC))
            IF (CABS1(EK-Z(KC)) .LE. 1.0) GOTO 3
               S = 1.0/CABS1(EK-Z(KC))
               CALL CSSCAL(N, S, Z, 1)
               EK = CMPLX(S,0.0E0)*EK
   3        WK = EK-Z(KC)
            WKM = (-EK)-Z(KC)
            S = CABS1(WK)
            SM = CABS1(WKM)
            KP1 = K+1
            IF (KP1 .GT. N) GOTO 8
               JMIN = IU(K)
               JMAX = IA(K+1)-1
               IF (JMAX .LT. JMIN) GOTO 7
                  DO  4 J = JMIN, JMAX
                     JUJ = JA(J)
                     SM = SM+CABS1(Z(JUJ)+WKM*CONJG(UL(J)))
                     Z(JUJ) = Z(JUJ)+CONJG(UL(J))*WK
                     S = S+CABS1(Z(JUJ))
   4                 CONTINUE
                  IF (S .GE. SM) GOTO 6
                     T = WKM-WK
                     WK = WKM
                     DO  5 J = JMIN, JMAX
                        JUJ = JA(J)
                        Z(JUJ) = Z(JUJ)+T*CONJG(UL(J))
   5                    CONTINUE
   6              CONTINUE
   7           CONTINUE
   8        Z(KC) = WK
   9        CONTINUE
         S = 1.0/SCASUM(N, Z, 1)
         CALL CSSCAL(N, S, Z, 1)
C
C SOLVE Y = L(TRANSPOSE) * W
C
         DO  13 KB = 1, N
            K = N+1-KB
            ID = IU(K)-1
C UL(ID) CONTAINS THE RECIPROCAL OF THE DIAGONAL OF U
C TRY TO AVOID OVERFLOW
            SUM = AMAX1(CABS1(Z(K)), CABS1(UL(ID)))
            IF (CABS1(Z(K)) .LE. 1.0) SUM = CABS1(Z(K)*CONJG(UL(ID)))
            S = 1.0E0
            JMIN = IA(K)
            JMAX = ID-1
            IF (SUM .LT. 1.0) GOTO 10
               S = 1./SUM
               CALL CSSCAL(N, S, Z, 1)
  10        SCUM = Z(K)*CONJG(UL(ID))
            IF (S .EQ. 0.0E0) Z(K) = (1.0,0.0)
            IF (JMAX .LT. JMIN) GOTO 12
               DO  11 J = JMIN, JMAX
                  JUJ = JA(J)
                  Z(JUJ) = Z(JUJ)+CONJG(UL(J))*SCUM
  11              CONTINUE
  12        Z(K) = SCUM
  13        CONTINUE
         S = 1.0/SCASUM(N, Z, 1)
         CALL CSSCAL(N, S, Z, 1)
         YNORM = 1.0
C
C SOLVE L X =W
C
         DO  17 I = 1, N
            JMIN = IA(I)
            IUI = IU(I)-1
            JMAX = IUI-1
            DK = UL(IUI)
            SCUM = Z(I)
            IF (JMIN .GT. JMAX) GOTO 15
               DO  14 J = JMIN, JMAX
                  JJ = JA(J)
                  SCUM = SCUM+UL(J)*Z(JJ)
  14              CONTINUE
  15        SC = AMAX1(CABS1(SCUM), CABS1(DK))
            IF (CABS1(SCUM) .LE. 1.0) SC = CABS1(SCUM*DK)
            S = 1.0E0
            IF (SC .LE. 1.0) GOTO 16
               S = 1./SC
               CALL CSSCAL(N, S, Z, 1)
               YNORM = YNORM*S
               SCUM = SCUM*CMPLX(S,0.0E0)
  16        SCUM = SCUM*DK
            IF (S .EQ. 0.0E0) SCUM = (1.0,0.0)
            Z(I) = SCUM
  17        CONTINUE
         S = 1.0/SCASUM(N, Z, 1)
         IF (S .GT. 1.0) GOTO 18
            CALL CSSCAL(N, S, Z, 1)
            YNORM = YNORM*S
C
C   SOLVE U * Z = X
  18     DO  22 KB = 1, N
            K = N+1-KB
            JMIN = IU(K)
            JMAX = IA(K+1)-1
            SCUM = Z(K)
            IF (JMIN .GT. JMAX) GOTO 20
               DO  19 J = JMIN, JMAX
                  JUJ = JA(J)
                  SCUM = SCUM-UL(J)*TMP(JUJ)
  19              CONTINUE
  20        IIC = MCP(K)
            IF (CABS1(SCUM) .LE. 1.) GOTO 21
               S = 1.0/CABS1(SCUM)
               CALL CSSCAL(N, S, TMP, 1)
               CALL CSSCAL(K, S, Z, 1)
               YNORM = YNORM*S
               SCUM = SCUM*CMPLX(S,0.0)
  21        TMP(IIC) = SCUM
  22        CONTINUE
C    MAKE ZNORM = 1.0
         S = 1.0/SCASUM(N, TMP, 1)
         CALL CSSCAL(N, S, TMP, 1)
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
