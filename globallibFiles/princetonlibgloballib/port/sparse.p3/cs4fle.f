      SUBROUTINE CS4FLE(N, IA, JA, A, IAMAX, IU, IROW, Z, IEX, B,
     1   IB, NB, IC, R, C, ORDER, IERR, THRESH, LAST, EPS)
      INTEGER N, IB, NB
      INTEGER IA(1), JA(1), IAMAX, IU(N), IROW(N), IEX(N)
      INTEGER IC(N), R(N), C(N), IERR, LAST
      REAL THRESH, EPS
      COMPLEX A(1), Z(N), B(IB, NB)
      LOGICAL ORDER
      INTEGER ICC, JUJ, JLU, NUU, MIN0, I
      INTEGER J, K, M, CMAX, JMIN, IMAX
      INTEGER JMAX, CI, CK, NC, II, JJ
      INTEGER IJ, RI, RK, IR, MM, JAMIN
      INTEGER JAMAX, NU, ISPAC, IUEND, LASTA, NP1
      INTEGER JAMAX1
      REAL CABS1,PVT, PMAX
      COMPLEX AKI,DK
      LOGICAL TEMP
C THIS IS LOWER LEVEL OF SFLE
C PARAMETERS NOT DEFINED IN SFLE
C IAMAX- SPACE LEFT IN STACK TO WORK IN
C IA - POINTS TO BEGINNING OF EACH ROW IN A AND JA
C JA COLUMN INDICES NONZERO ELEMENTS
C A NUMERICAL NONZERO ELEMENTS
C IU END OF U PORTION OF THE ROW- INTEGER VECTOR OF LENGTH N
C IROW  INTEGER SCRATCH VECTOR LENGTH N
C Z COMPLEX SCRATCH VECTOR LENGTH N
C IEX - INTEGER SCRATCH VECTOR LENGTH N POINTING TO EXTRA SPACE
C IC INTGER VECTOR LENGTH N OF INVERSE COLUMN ORDER
C C INTEGER VECTOR LENGTH N OF COLUMN ORDER
C R IF ORDERING USED AN INTEGER VECTOR OF LENGTH N GIVING ROW ORDER
C IERR -ERROR PARAMETER
C THRESH-THRESHHOLD PARAMETER
C LAST- HOW MUCHCSPACE ACTUALLY USED
C EPS CRITERIA FOR SINGULARITY
C INITIALIZATION
C REARRANGE RIGHT HAND SIDES
      IF (.NOT. ORDER) GOTO 4
         DO  3 J = 1, NB
            DO  1 I = 1, N
               IR = R(I)
               Z(I) = B(IR, J)
   1           CONTINUE
            DO  2 I = 1, N
               B(I, J) = Z(I)
   2           CONTINUE
   3        CONTINUE
   4  LAST = IA(N+1)
      LASTA = LAST
      NP1 = N+1
      IERR = 0
      DO  5 I = 1, N
         IROW(I) = 0
   5     CONTINUE
C DETERMINE NEXT ROW OF L AND U
      DO  32 K = 1, N
         NC = C(K)
         Z(NC)= CMPLX(0.0E0,0.0E0)
         IEX(K) = LAST
         RK = K
         IF (ORDER) RK = R(K)
         M = NP1
         JAMIN = IA(RK)
         IROW(NP1) = NP1
         IUEND = NP1
         JAMAX1 = IA(RK+1)
         JAMAX = JAMAX1-1
         DO  10 J = JAMIN, JAMAX
            JJ = JA(J)
            Z(JJ) = A(J)
            TEMP = JJ .LT. 1
            IF (.NOT. TEMP) TEMP = JJ .GT. N
C/6S
C           IF (TEMP) CALL SETERR(29HCSPFLE-INCORRECT COLUMN INDEX, 29
C    1         , K+10+N, 2)
C/7S
            IF (TEMP) CALL SETERR('CSPFLE-INCORRECT COLUMN INDEX', 29
     1         , K+10+N, 2)
C/
            ICC = IC(JJ)
            IF (ICC .LT. K) GOTO 6
               IROW(ICC) = IUEND
               IUEND = ICC
               GOTO  9
   6           IF (M .GT. ICC) M = NP1
C NEW ELEMENT IS IN L PART OF THE ROW
   7              II = IROW(M)
                  IF (II .GE. ICC) GOTO  8
                  M = II
                  GOTO  7
   8           IROW(M) = ICC
               IROW(ICC) = II
   9        CONTINUE
  10        CONTINUE
C
C ELIMINATE ROW
         I = NP1
  11        J = I
            I = IROW(I)
            IROW(J) = 0
            IF (I .EQ. NP1) GOTO  23
            RI = I
            IF (ORDER) RI = R(I)
            JMIN = IA(RI)
            JMAX = MIN0(IA(RI+1)-1, IU(I))
            CI = C(I)
            AKI = -Z(CI)
C FORWARD SOLVE
            DO  12 MM = 1, NB
               B(K, MM) = B(K, MM)+AKI*B(I, MM)
  12           CONTINUE
            IF (JMAX .LT. JMIN) GOTO 22
  13              DO  19 J = JMIN, JMAX
C ELIMINATE ITH ELEMENT
                     JUJ = JA(J)
                     ICC = IC(JUJ)
                     IF (IROW(ICC) .NE. 0) GOTO 18
                        IF (ICC .LT. K) GOTO 14
                           IROW(ICC) = IUEND
C FILL IN-SEE IF IT IS IN U OR L
                           IUEND = ICC
                           GOTO  17
  14                       TEMP = M .GT. ICC
                           IF (.NOT. TEMP) TEMP = M .LT. I
                           IF (TEMP) M = I
  15                          IJ = IROW(M)
                              IF (IJ .GE. ICC) GOTO  16
                              M = IJ
                              GOTO  15
  16                       IROW(M) = ICC
                           IROW(ICC) = IJ
  17                    Z(JUJ) = (0.E0,0.E0)
  18                 Z(JUJ) = Z(JUJ)+AKI*A(J)
  19                 CONTINUE
                  IF (JMAX .EQ. IU(I)) GOTO  21
                  JMIN = IEX(I)
                  JMAX = IU(I)
  20              IF (JMIN .LE. JMAX) GOTO  13
  21        CONTINUE
  22        CONTINUE
            GOTO  11
  23     I = IUEND
         PMAX = 0.E0
         NU = -1
  24     IF (I .EQ. NP1) GOTO  26
            CI = C(I)
            PVT = CABS1(Z(CI))
            NU = NU+1
            IF (PVT .LE. PMAX) GOTO 25
               IMAX = I
               PMAX = PVT
  25        I = IROW(I)
            GOTO  24
C DO THRESHHOLD PIVOTING
  26     IF (CABS1(Z(NC)) .GE. THRESH*PMAX) IMAX = K
         CMAX = C(IMAX)
C CHECK FOR SINGULARITY
         IF (CABS1(Z(CMAX)) .LE. EPS) GOTO  33
         ISPAC = JAMAX-JAMIN+1
C SEE IF SUFFICIENT SPACE AVAILABLE
         IF (NU-ISPAC+LAST .GE. IAMAX) GOTO  34
         NUU = JAMIN+NU-1
         IF (NUU .GT. JAMAX) NUU = LAST+NU-ISPAC-1
         IU(K) = NUU
         JLU = NUU+1
         DK = (1.E0,0.0)/Z(CMAX)
C FIX UP RIGHT HAND SIDE
         DO  27 MM = 1, NB
            B(K, MM) = B(K, MM)*DK
  27        CONTINUE
         I = IUEND
  28     IF (I .EQ. NP1) GOTO  30
            IF (I .EQ. IMAX) GOTO 29
               CI = C(I)
C STORE ELEMENT OF U
               A(NUU) = Z(CI)*DK
               JA(NUU) = CI
               IF (NUU .EQ. LAST) NUU = JAMAX1
               NUU = NUU-1
  29        II = I
            I = IROW(I)
            IROW(II) = 0
            GOTO  28
  30     IF (JLU .GT. LAST) LAST = JLU
C INTERCHANGE THE COLUMN INDICES
         IF (K .EQ. IMAX) GOTO 31
            JJ = C(K)
            C(K) = CMAX
            C(IMAX) = JJ
            CK = C(K)
            IC(CK) = K
            IC(JJ) = IMAX
  31     CONTINUE
  32     CONTINUE
      JA(LAST) = LAST
      RETURN
  33  IERR = 3*N+9+K
      RETURN
  34  IERR = 2*N+K+10
      RETURN
      END
