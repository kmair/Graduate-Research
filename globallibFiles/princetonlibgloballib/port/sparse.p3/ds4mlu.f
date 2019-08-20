      SUBROUTINE DS4MLU(N, IA, JA, A, IAMAX, IL, IROW, Z, IC, R, C,
     1   IERR, THRESH, EPS, LAST, GROWTH)
      INTEGER N
      INTEGER IA(N), JA(N), IAMAX, IL(N), IROW(N), IC(N)
      INTEGER R(N), C(N), IERR, LAST
      DOUBLE PRECISION A(N), Z(N), THRESH, EPS, GROWTH
      INTEGER ICC, IDI, JUJ, JLU, NUR, NUU
      INTEGER MIN0, I, J, K, M, CMAX
      INTEGER JMIN, IMAX, JMAX, CI, CK, NC
      INTEGER II, JJ, IJ, RI, RK, JAMIN
      INTEGER JAMAX, NU, ISPAC, LASTA, IUEND, IT1
      INTEGER NP1, JDMAX1
      DOUBLE PRECISION AKI, AFT, PVT, BFOR, PMAX
      DOUBLE PRECISION DK, D1MACH
      LOGICAL TEMP, SING
C INPUT PARAMETERS
C N ORDER OF MATRIX
C IA POINTER TO JA AND A OF BEGINNING OF EACH NEW ROW
C    LENGTH N+1
C JA COLUMN INDICES OF NONZERONELEMENTS OF A
C A NONZERO ELEMENTS OF A
C IAMAX - DIMENSION OF A AND JA ARRAYS
C R- INTEGER VECTOR LENGTH N OF ROW PERMUTAIONS
C C - INTEGER VECTOR LENGTH N OF COLUMN PERMUTATIONS
C IC - INTEGER VECTOR OF LENGTH N OF INCERSE COLUMN PERMUTAIONS
C     IC(C(I))=I
C THRESH - THRESHHOLD PIVOTING PARAMETER BETWEEN 0.0 AND 1.0.
C          IF 1.0, PARTIAL PIVOTING WILL BE PEERFORMED
C EPS - SINGULARITY CRITERIA.
C SCRATCH VECTORS
C Z ,DOUBLE PRECISION, LENGTH N
C IROW, INTEGER LENGTH N+1
C OUTPUT
C JA,A NONZERO ELEMENTS AND CORRESPONDING COLUMN INDICES OF LU
C       DECOMPOSITION
C C,IC - COLUMN PERMUTAIONS AFTER THRESHHOLD PIVOTING
C IERR - ERROR CRITERIA- IF NONZERO WORRY
C IF  .LE. N+10 NULL ROW AT IERR-10
C IF N+11 .LE. IERR .LE. 2N+10 INVALID INDEX AT ROW IERR-N-10
C IF 2N+11 .LE. IERR .LE. 3N+10  SINGULAR MATRIX OF RANK IERR-2N-10
C IF 3N+11 .LE. IERR .LE.  RAN OUT OF STORAGE AT ROW IERR-3N-10
C LAST- NUMBER OF ELEMENTS IN A AND JA USED
C GROWTH- ELEMENT GROWTH
C IL- INTEGER VECTOR LENGTH N+1, POINTING TO BEGINNINGS OF EACH ROW OF
C     L IN LU DECOMPOSITION
C INITIALIZATION
      LAST = IA(N+1)
      BFOR = 0.0D0
      IT1 = 0
      AFT = 0.0D0
      LASTA = LAST
      NP1 = N+1
      IERR = 0
      DO  1 I = 1, N
         IROW(I) = 0
   1     CONTINUE
C DETERMINE NEXT ROW OF L AND U
      DO  28 K = 1, N
         SING=.FALSE.
         RK = R(K)
         M = NP1
         JAMIN = IA(RK)
         IROW(NP1) = NP1
         IUEND = NP1
         JDMAX1 = IA(RK+1)
         NC = C(K)
         Z(NC) = 0.D0
         JAMAX = JDMAX1-1
C CHECK FOR NULL ROW
         IF (JAMIN .GT. JAMAX) GOTO  29
         DO  6 J = JAMIN, JAMAX
            JJ = JA(J)
C CHECK FOR VALID COLUMN INDEX
            TEMP = JJ .LE. 0
            IF (.NOT. TEMP) TEMP = JJ .GT. N
            IF (TEMP) GOTO  32
            Z(JJ) = A(J)
            BFOR = DMAX1(BFOR, DABS(Z(JJ)))
            ICC = IC(JJ)
C NEW ELEMENT IS IN U PART SO JUST ADD TO LINKED LIST
            IF (ICC .LT. K) GOTO 2
               IROW(ICC) = IUEND
               IUEND = ICC
               GOTO  5
   2           IF (M .GT. ICC) M = NP1
C NEW ELEMENT IS IN L PART OF THE ROW
   3              II = IROW(M)
                  IF (II .GE. ICC) GOTO  4
                  M = II
                  GOTO  3
   4           IROW(M) = ICC
               IROW(ICC) = II
   5        CONTINUE
   6        CONTINUE
C
C ELIMINATE ROW
         NUR = 0
         I = NP1
   7        I = IROW(I)
            IF (I .EQ. NP1) GOTO  18
C DETERMINE WHICH ROW IN PERMUTATION
            RI = R(I)
            JMIN = IA(RI)
            IDI = IL(I)
C RELEVANT ROW MAY BE IN TWO SECTIONS, THUS EITHER
C ELIMINATE UNTIL U IS FINISHED OR SECTION IS FINISHED
            JMAX = MIN0(IA(RI+1), IDI)-1
            CI = C(I)
            AKI = -Z(CI)
            AFT = DMAX1(AFT, DABS(AKI))
            NUR = NUR+1
            IF (JMAX .LT. JMIN) GOTO 17
   8              DO  14 J = JMIN, JMAX
C ELIMINATE ITH ELEMENT
                     JUJ = JA(J)
                     ICC = IC(JUJ)
                     IF (IROW(ICC) .NE. 0) GOTO 13
                        IF (ICC .LT. K) GOTO 9
                           IROW(ICC) = IUEND
C FILL IN-SEE IF IT IS IN U OR L
                           IUEND = ICC
                           GOTO  12
   9                       TEMP = M .GT. ICC
                           IF (.NOT. TEMP) TEMP = M .LT. I
                           IF (TEMP) M = I
C FILL-IN IS IN L SO FIND ITS ORDERING PLACE
  10                          IJ = IROW(M)
                              IF (IJ .GE. ICC) GOTO  11
                              M = IJ
                              GOTO  10
  11                       IROW(M) = ICC
                           IROW(ICC) = IJ
  12                    Z(JUJ) = 0.D0
  13                 Z(JUJ) = Z(JUJ)+AKI*A(J)
  14                 CONTINUE
C TEST IF THERE IS A SECOND SEGMENT
                  TEMP = IDI .LT. LASTA
                  IF (.NOT. TEMP) TEMP = JMIN .GE. LASTA
                  IF (TEMP) GOTO  16
                  JMIN = JA(IDI)
                  JMAX = IDI-1
  15              IF (JMIN .LE. JMAX) GOTO  8
  16        CONTINUE
  17        CONTINUE
            GOTO  7
  18     I = IUEND
         PMAX = 0.D0
         NU = 0
  19     IF (I .EQ. NP1) GOTO  21
            CI = C(I)
            PVT = DABS(Z(CI))
            NU = NU+1
            IF (PVT .LE. PMAX) GOTO 20
               IMAX = I
               PMAX = PVT
  20        I = IROW(I)
            GOTO  19
C DO THRESHHOLD PIVOTING
  21     IF (DABS(Z(NC)) .GE. THRESH*PMAX) IMAX = K
         CMAX = C(IMAX)
C TEST FOR SINGULARITY
         IF (DABS(Z(CMAX)) .GT. EPS) GOTO  30
             IF (IERR.EQ.0)IERR=3*N+K+10
             SING=.TRUE.
 30      CONTINUE
         NUR = NUR+NU
         ISPAC = JAMAX-JAMIN+1
C TEST IS THERE IS ENOUGH SPACE
         IF (NUR-ISPAC+LAST .GE. IAMAX) GOTO  31
         NUU = JAMIN+NU-1
C DETERMINE IS FILLIN REQUIRES SECOND SEGMENT FOR U
         IF (NUU .GT. JAMAX) NUU = LAST+NU-ISPAC-1
         IL(K) = NUU
         JLU = NUU+1
         IF (NUU .EQ. JAMAX) JLU = LAST
         JA(NUU) = LAST
         AFT = DMAX1(AFT, PMAX)
         IF(.NOT.SING)DK = 1.D0/Z(CMAX)
         IF (SING)DK=D1MACH(2)
         I = IUEND
C STORE DIAGONAL ELEMENT
         A(NUU) = DK
  22     IF (I .EQ. NP1) GOTO  24
            IF (I .EQ. IMAX) GOTO 23
               CI = C(I)
C STORE ELEMENT OF U
               IF (NUU .EQ. LAST) NUU = JDMAX1
               NUU = NUU-1
               A(NUU) = Z(CI)*DK
               JA(NUU) = CI
  23        II = I
            I = IROW(I)
            IROW(II) = 0
            GOTO  22
C STORE ELEMENTS OF L
  24     I = NP1
  25        J = I
            I = IROW(I)
            IROW(J) = 0
            IF (I .EQ. NP1) GOTO  26
            JA(JLU) = I
            CI = C(I)
            A(JLU) = -Z(CI)
            JLU = JLU+1
            IF (JLU .EQ. JDMAX1) JLU = LAST
            GOTO  25
  26     IF (JLU .GT. LAST) LAST = JLU
C INTERCHANGE THE COLUMN INDICES
         IF (K .EQ. IMAX) GOTO 27
            JJ = C(K)
            C(K) = CMAX
            C(IMAX) = JJ
            CK = C(K)
            IC(CK) = K
            IC(JJ) = IMAX
  27     CONTINUE
  28     CONTINUE
      GROWTH = 1.0D0
      IF (BFOR.NE.0.0D0)GROWTH = DMAX1(1.0D0, AFT/BFOR)
      IL(N+1) = LAST
      JA(LAST) = LAST
      RETURN
  29  IERR = K+10
      RETURN
  31  IERR = 2*N+K+10
      RETURN
  32  IERR = N+K+10
      RETURN
      END
