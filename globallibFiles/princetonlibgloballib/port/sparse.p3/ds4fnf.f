      SUBROUTINE DS4FNF(N,R,IC,IL,IU,JU,U,GETA,ROW,G,EPS,JROW)
      INTEGER N, IL(1), IU(1), JU(1),JROW(N)
      INTEGER R(N),IC(N)
      EXTERNAL GETA
      DOUBLE PRECISION  ROW(1), G, EPS,U(1)
      INTEGER IMIN, JMIN, IMAX, JMAX, I, J
      INTEGER K
      DOUBLE PRECISION DK, LI, TEMP, BEF, AFT
      INTEGER TEMP1, TEMP2
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
C    SUM - USED IN CALCULATING  TMP.
C  ******  FOR EACH ROW  **********************************************
      BEF = 0.0
      AFT = 0.0
      DO 100 I=1,N
          ROW(I)=0.D0
 100  CONTINUE
      DO  8 K = 1, N
C  ******  SET THE INITIAL STRUCTURE OF ROW  **************************
         IR=R(K)
         IMIN = IL(K)
         IMINM1=IMIN-1
         CALL GETA(IR,U(IMIN),JROW,NUM)
         DO  1 J = 1, NUM
            JJ=J+IMINM1
            IF (DABS(U(JJ)) .GT. BEF) BEF = DABS(U(JJ))
            IT1= JROW(J)
            IT2=IC(IT1)
            ROW(IT2) = U(JJ)
   1        CONTINUE
C  ******  ASSIGN THE KTH ROW OF L AND ADJUST ROW,   ***************
         IMAX = IU(K)-2
         IF (IMIN .GT. IMAX) GOTO 5
            DO  4 I = IMIN, IMAX
               TEMP2 = JU(I)
               LI = -ROW(TEMP2)
               ROW(TEMP2)=0.D0
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
   5     IF (DABS(ROW(K)) .LE. EPS) GOTO  9
         DK = 1D0/ROW(K)
         AFT=DMAX1(AFT,DABS(ROW(K)))
         ROW(K)=0.D0
         JMIN = IU(K)
         U(JMIN-1) = DK
         JMAX = IL(K+1)-1
         IF (JMIN .GT. JMAX) GOTO 7
            DO  6 J = JMIN, JMAX
               TEMP1 = JU(J)
               TEMP = ROW(TEMP1)
               ROW(TEMP1)=0.D0
               IF (DABS(TEMP) .GT. AFT) AFT = DABS(TEMP)
               U(J) = TEMP*DK
   6           CONTINUE
   7     CONTINUE
   8     CONTINUE
C  ******  NORMAL RETURN AND ERROR RETURNS  ***************************
      G = AFT/BEF
      RETURN
C  ZERO DIAGONAL ELEMENT
C/6S
C  9  CALL SETERR(20HDNNF-SINGULAR MATRIX, 20, K+9, 1)
C/7S
   9  CALL SETERR('DNNF-SINGULAR MATRIX', 20, K+9, 1)
C/
      RETURN
      END
