      SUBROUTINE S4MLE(N, B, IB, NB, JA, A, IAMAX, IU, IROW, Z,
     1   IC, R, C, IERR, THRESH, JLU, IA, JJA, AA, ORDER, EPS)
      INTEGER N, IB, NB
      INTEGER JA(1), IAMAX, IU(N), IROW(N), IC(N), R(N)
      INTEGER C(N), IERR, JLU, IA(N), JJA(N)
      REAL B(IB, NB), A(1), Z(1), THRESH, AA(N), EPS
      LOGICAL ORDER
      INTEGER ICC, JUJ, NUM, I, J, K
      INTEGER JAMA, M, JMIN, IMAX, JMAX, CK
      INTEGER II, JJ, IJ, IR, MM, RK
      INTEGER JAMIN, NU, IUEND, NP1, NP2
      REAL ABS, AKI, PVT, PMAX, DK
      LOGICAL TEMP
C THIS SUBROUTINE IS LOWER LEVEL SUBROUTINE FOR SPMLE
C PARAMETERS NOT DEFINED IN SPMLE ARE AS FOLLOWS
C JA -SPACE FOR STORING COLUMN INDICES OF U IN LU DECOMPOSITION
C A - SPACE FOR STORING ELEMENTS OF U IN LU DECOMPOSITION
C IAMAX - SIZE OF JA AND A
C IU - POINTER INTO JA AND A GIVING BEGINNING OF EACH ROW
C Z  TEMP VECTOR OF LENGTH N
C IC- INTEGER VECTOR GIVING INVERTED COLUMN PERMUTATION
C R - IF ORDERING INTEGER VECTOR OF LENGTH N GIVING ROW PERMUTATIONS
C C - COLUMN PERMUTATIONS, INTEGER VECTOR OF LENGTH N
C IERR - ERROR FLAG. OK IF SET OT 0 ON OUTPUT
C THRESH - THRESHHOLD PARAMETER FOR PIVOTING
C JLU - NUMBER OF ELEMENTS OF JA AND A ACTUALLY USED
C ORDER - LOGICAL VARIABLE. IF .TRUE. THEN PIVOTING FOR SPARSITY
C WAS DONE AND R SHOULD BE LOOKED AT.
C EPS - CRITERIA FOR SINGULARITY
C IAMAX - SIZE OF SPACE AVAILABLE FOR SORING LU DECOMPOSITION
C INITIALIZATION
      IU(1) = 1
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
   4  NP1 = N+1
      IERR = 0
      NP2 = N+2
      JLU = 1
      DO  5 I = 1, N
         IROW(I) = 0
   5     CONTINUE
C DETERMINE NEXT ROW OF L AND U
      DO  29 K = 1, N
         M = NP1
         IROW(NP1) = NP1
         IUEND = NP2
         RK = K
         IF (ORDER) RK = R(K)
         JAMIN = IA(RK)
         JAMA = IA(RK+1)-1
         NUM = JAMA-JAMIN
         Z(K) = 0.0
         NUM = NUM+1
C CHECK FOR NULL ROW
         IF (JAMA .LT. JAMIN) GOTO  30
         DO  10 J = JAMIN, JAMA
            JJ = JJA(J)
C CHECK FOR VALID COLUMN INDEX
            TEMP = JJ .GT. N
            IF (.NOT. TEMP) TEMP = JJ .LT. 1
C/6S
C           IF (TEMP) CALL SETERR(29H SPMLE-INCORRECT COLUMN INDEX, 29
C    1         , K+10+N, 2)
C/7S
            IF (TEMP) CALL SETERR(' SPMLE-INCORRECT COLUMN INDEX', 29
     1         , K+10+N, 2)
C/
            ICC = IC(JJ)
            Z(ICC) = AA(J)
            IF (ICC .LT. K) GOTO 6
               IROW(ICC) = IUEND
C NEW ELEMENT IS IN U PART OF ROW
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
            IF (I .EQ. NP1) GOTO  20
            JMIN = IU(I)
            JMAX = IU(I+1)-1
            AKI = -Z(I)
C FORWARD SOLVE
            DO  12 MM = 1, NB
               B(K, MM) = B(K, MM)+AKI*B(I, MM)
  12           CONTINUE
            IF (JMAX .LT. JMIN) GOTO 19
               DO  18 J = JMIN, JMAX
C ELIMINATE ITH ELEMENT
                  JUJ = JA(J)
                  ICC = IC(JUJ)
                  IF (IROW(ICC) .NE. 0) GOTO 17
                     IF (ICC .LT. K) GOTO 13
                        IROW(ICC) = IUEND
C FILL IN-SEE IF IT IS IN U OR L
C FILL IN IS IN U
                        IUEND = ICC
                        GOTO  16
  13                    TEMP = M .GT. ICC
C FILL IS IN L PORTION
                        IF (.NOT. TEMP) TEMP = M .LT. I
                        IF (TEMP) M = I
  14                       IJ = IROW(M)
                           IF (IJ .GE. ICC) GOTO  15
                           M = IJ
                           GOTO  14
  15                    IROW(M) = ICC
                        IROW(ICC) = IJ
C SINCE THIS IS A FILL IN, INITIALIZE SPACE
  16                 Z(ICC) = 0.E0
  17              Z(ICC) = Z(ICC)+AKI*A(J)
  18              CONTINUE
  19        CONTINUE
            GOTO  11
  20     I = IUEND
         PMAX = 0.E0
         NU = -1
  21     IF (I .EQ. NP2) GOTO  23
            PVT = ABS(Z(I))
            NU = NU+1
            IF (PVT .LE. PMAX) GOTO 22
               IMAX = I
               PMAX = PVT
  22        I = IROW(I)
            GOTO  21
C DO THRESHHOLD PIVOTING
  23     IF (ABS(Z(K)) .GE. THRESH*PMAX) IMAX = K
C CHECK FOR SINGULARITY
         IF (ABS(Z(IMAX)) .LE. EPS) GOTO  31
C CHECK IF SUFFICIENT SPACE
         IF (NU+JLU .GT. IAMAX) GOTO  32
         NU = JLU+NU
         JLU = NU
         DK = 1.E0/Z(IMAX)
C FIX UP RIGHT HAND SIDE
         DO  24 MM = 1, NB
            B(K, MM) = B(K, MM)*DK
  24        CONTINUE
         I = IUEND
  25     IF (I .EQ. NP2) GOTO  27
            IF (I .EQ. IMAX) GOTO 26
               NU = NU-1
               A(NU) = Z(I)*DK
               JA(NU) = C(I)
  26        II = I
            I = IROW(I)
C ZERO OUT SPCE SO NEXT TIME WILL NOT MISINTERPRET FILL-IN
            IROW(II) = 0
            GOTO  25
  27     IU(K+1) = JLU
C INTERCHANGE THE COLUMN INDICES
         IF (K .EQ. IMAX) GOTO 28
            JJ = C(K)
            C(K) = C(IMAX)
            C(IMAX) = JJ
            CK = C(K)
            IC(CK) = K
            IC(JJ) = IMAX
  28     CONTINUE
  29     CONTINUE
      IU(N+1) = JLU
      JA(JLU) = JLU
      RETURN
  30  IERR = K+10
      RETURN
  31  IERR = 3*N+10+K
      RETURN
  32  IERR = 2*N+K+10
      RETURN
      END
