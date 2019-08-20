      SUBROUTINE CS4FNF(N, R, IC, IL, IU, JU, U, GETA, ROW, G, EPS,
     1   JROW)
      INTEGER N
      EXTERNAL GETA
      INTEGER R(N), IC(N), IL(1), IU(1), JU(1), JROW(N)
      COMPLEX U(1), ROW(1)
      REAL G,EPS
      INTEGER NUM, I, J, K, IMIN, JMIN
      INTEGER IMAX, JMAX, TEMP1, TEMP2, JJ, IR
      INTEGER IT1, IT2, IMINM1
      REAL BEF, CABS1, AFT,  AMAX1
      COMPLEX DK, TEMP
      COMPLEX LI
C
C
C       INPUT VARIABLES N,IL,IU,JU
C       OUTPUT VARIABLES--   G,  U,
C       PARAMETERS USED INTERNALLY--
C FIA   * ROW - HOLDS INTERMEDIATE VALUES IN CALCULATION OF  U AND L.
C       *         SIZE = N.
C RF       G      ELEMENT GROWTH
C RN      EPS      LARGEST NONACCEPTABLE PIVOT
C  INTERNAL VARIABLES--
C    JMIN, JMAX - INDICES OF THE FIRST AND LAST POSITIONS IN A ROW TO
C      BE EXAMINED.
C    AFT - NORM OF MATRIX AFTER DECOMPOSITION
C    BEF   NORM OF MATRIX ORIGINALLY
      BEF = 0.0
      AFT = 0.0
      DO  1 I = 1, N
         ROW(I) = (0.E0,0.E0)
   1     CONTINUE
      DO  9 K = 1, N
C  ******  SET THE INITIAL STRUCTURE OF ROW  **************************
         IR = R(K)
         IMIN = IL(K)
         IMINM1 = IMIN-1
         CALL GETA(IR, U(IMIN), JROW, NUM)
         DO  2 J = 1, NUM
            JJ = J+IMINM1
            IF (CABS1(U(JJ)) .GT. BEF) BEF = CABS1(U(JJ))
            IT1 = JROW(J)
            IT2 = IC(IT1)
            ROW(IT2) = U(JJ)
   2        CONTINUE
C  ******  ASSIGN THE KTH ROW OF L AND ADJUST ROW,   ***************
         IMAX = IU(K)-2
         IF (IMIN .GT. IMAX) GOTO 6
            DO  5 I = IMIN, IMAX
               TEMP2 = JU(I)
               LI = -ROW(TEMP2)
               ROW(TEMP2) = (0.E0,0.E0)
               U(I) = LI
               JMIN = IU(TEMP2)
               JMAX = IL(TEMP2+1)-1
               IF (JMIN .GT. JMAX) GOTO 4
                  DO  3 J = JMIN, JMAX
                     TEMP1 = JU(J)
                     ROW(TEMP1) = ROW(TEMP1)+LI*U(J)
   3                 CONTINUE
   4           CONTINUE
   5           CONTINUE
C  ******  ASSIGN KTH ROW OF U AND DIAGONAL D, SET TMP(K)  ************
   6     IF (CABS1(ROW(K)) .LE. EPS) GOTO  10
         DK = (1.E0,0.E0)/ROW(K)
         ROW(K) = (0.E0,0.E0)
         JMIN = IU(K)
         U(JMIN-1) = DK
         AFT = AMAX1(AFT, CABS1(ROW(K)))
         JMAX = IL(K+1)-1
         IF (JMIN .GT. JMAX) GOTO 8
            DO  7 J = JMIN, JMAX
               TEMP1 = JU(J)
               TEMP = ROW(TEMP1)
               ROW(TEMP1) = (0.E0,0.E0)
               IF (CABS1(TEMP) .GT. AFT) AFT = CABS1(TEMP)
               U(J) = TEMP*DK
   7           CONTINUE
   8     CONTINUE
   9     CONTINUE
C  ******  NORMAL RETURN AND ERROR RETURNS  ***************************
      G = AMAX1(AFT/BEF, 1.0)
      RETURN
C  ZERO DIAGONAL ELEMENT
C/6S
C 10  CALL SETERR(22HCSPFNF-SINGULAR MATRIX, 22, K+9, 1)
C/7S
  10  CALL SETERR('CSPFNF-SINGULAR MATRIX', 22, K+9, 1)
C/
      RETURN
      END
