      SUBROUTINE CS4MNF(N, IL, IU, JU, U, ROW, G, EPS)
      INTEGER N, IL(N), IU(N), JU(N)
      REAL G, EPS
      COMPLEX U(N), ROW(N)
      INTEGER IMIN, JMIN, IMAX, JMAX, I, J
      INTEGER K
      REAL  CABS1,   BEF, AFT
      COMPLEX DK, LI, TEMP
      INTEGER TEMP1, TEMP2
C       INPUT VARIABLES N,IL,IU,JU
C       OUTPUT VARIABLES--   G,  U,
C       PARAMETERS USED INTERNALLY--
C FIA   * ROW - HOL S INTERMEDIATE VALUES IN CALCULATION OF  U AND L.
C       *         SIZE = N.
C RF       G      ELEMENT GROWTH
C RN      EPS      LARGEST NONACCEPTABLE PIVOT
C  INTERNAL VARIABLES--
C    JMIN, JMAX - INDICES OF THE FIRST AND LAST POSITIONS IN A ROW TO
C      BE EXAMINED.
C    SUM - USED IN CALCULATING  TMP.
C  ******  FOR EACH ROW  **********************************************
      BEF = 0.0
      AFT = 0.0
      DO  8 K = 1, N
C  ******  SET THE INITIAL STRUCTURE OF ROW  **************************
         JMIN = IL(K)
         JMAX = IL(K+1)-1
         DO  1 J = JMIN, JMAX
            IF (CABS1(U(J)) .GT. BEF) BEF = CABS1(U(J))
            TEMP2 = JU(J)
            ROW(TEMP2) = U(J)
   1        CONTINUE
C  ******  ASSIGN THE KTH ROW OF L AND ADJUST ROW,   ***************
         IMIN = IL(K)
         IMAX = IU(K)-2
         IF (IMIN .GT. IMAX) GOTO 5
            DO  4 I = IMIN, IMAX
               TEMP2 = JU(I)
               LI = -ROW(TEMP2)
               U(I) = LI
               JMIN = IU(TEMP2)
               JMAX = IL(TEMP2+1)-1
               IF (JMIN .GT. JMAX) GOTO 3
                  DO  2 J = JMIN, JMAX
                     TEMP1 = JU(J)
                     ROW(TEMP1) = ROW(TEMP1)+LI*U(J)
   2                 CONTINUE
   3           CONTINUE
   4           CONTINUE
C  ******  ASSIGN KTH ROW OF U AND DIAGONAL D, SET TMP(K)  ************
   5     IF (CABS1(ROW(K)) .LE. EPS) GOTO  9
         DK = (1.E0,0.0E0)/ROW(K)
         JMIN = IU(K)
         U(JMIN-1) = DK
         AFT=AMAX1(AFT,CABS1(ROW(K)))
         JMAX = IL(K+1)-1
         IF (JMIN .GT. JMAX) GOTO 7
            DO  6 J = JMIN, JMAX
               TEMP1 = JU(J)
               TEMP = ROW(TEMP1)
               IF (CABS1(TEMP) .GT. AFT) AFT = CABS1(TEMP)
               U(J) = TEMP*DK
   6           CONTINUE
   7     CONTINUE
   8     CONTINUE
C  ******  NORMAL RETURN AND ERROR RETURNS  ***************************
      G = AFT/BEF
      RETURN
C  ZERO DIAGONAL ELEMENT
C/6S
C  9  CALL SETERR(22HCS4MNF-SINGULAR MATRIX, 22, K+9, 1)
C/7S
   9  CALL SETERR('CS4MNF-SINGULAR MATRIX', 22, K+9, 1)
C/
      RETURN
      END
