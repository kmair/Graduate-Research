      SUBROUTINE DS4FSF(N,R,IC,GETROW,IL,IU,JU,JUMAX,Q,IM,IERR,
     1JROW)
      INTEGER N, R(1), IC(1),  IL(1), JROW(N)
      EXTERNAL GETROW
      INTEGER IU(1), JU(1), JUMAX, Q(1), IM(1)
      INTEGER QM, VJ, JMIN, JMAX, I, J
      INTEGER K, M, JUPTR
      INTEGER TEMP
C       ---------------------------------------------------------------
C    V.   PARAMETERS
C         FOLLOWING IS A LIST OF PARAMETERS TO THE PROGRAMS.  NAMES ARE
C    UNIFORM AMONG THE VARIOUS SUBROUTINES.  CLASS ABBREVIATIONS ARE--
C       V - SUPPLIES A VALUE TO A SUBROUTINE
C       R - CONTAINS A RESULT RETURNED BY A SUBROUTINE
C       I - IS USED INTERNALLY BY A SUBROUTINE
C       A - IS AN ARRAY
C       N - IS AN INTEGER VARIABLE
C       F - IS A DOUBLE PRECISION VARIABLE.
C CLASS * PARAMETER
C ----- * ---------
C NVA   * IC    - INVERSE OF THE ORDERING OF THE COLUMNS OF  M.  FOR
C       *           EXAMPLE, IF COLUMN 1 IS THE 5TH COLUMN AFTER
C       *           REORDERING, THEN  IC(1)=5.
C       *           SIZE = N.
C NVRA  * IL    - POINTERS TO THE FIRST ELEMENTS OF EACH ROW IN  L.
C       *           SIZE = N+1.
C NVRA  * IU    - POINTERS TO THE FIRST ELEMENTS OF EACH ROW IN  U.
C       *           SIZE = N+1.
C NVRA  * JU    - COLUMN NUMBER CORRESPONDING TO EACH ELEMENT OF  U.
C                 AND L
C NV    * JUMAX - DECLARED DIMENSION OF  JU.
C NV    * N     - NUMBER OF ROWS/COLUMNS IN MATRIX  M.
C NVA   * R     - ORDERING OF THE ROWS OF  M.
C       *           SIZE = N.
C       ---------------------------------------------------------------
C*** SYMBOLIC LU-FACTORIZATION OF A NONSYMMETRIC SPARSE MATRIX
C       INPUT VARIABLES-- N, R, IC,   JUMAX.
C       OUTPUT VARIABLES-- IL,  IU, JU, .
C       PARAMETERS USED INTERNALLY--
C NIA   * Q     - SUPPOSE  MO  IS THE RESULT OF REORDERING  M.  IF
C       *           PROCESSING OF THE ITH ROW OF  MO  (HENCE THE ITH
C       *           ROWS OF  L AND U) IS BEING DONE,  Q(J)  IS
C       *           INITIALLY NONZERO IF  MO(I,J) IS NONZERO.  SINCE
C       *           VALUES NEED NOT BE STORED, EACH ENTRY POINTS TO THE
C       *           NEXT NONZERO.  FOR EXAMPLE, IF N=9 AND THE 5TH ROW
C       *           OF  MO  IS
C       *              0 X X 0 X 0 0 X 0
C       *           THEN  Q  WILL INITIALLY BE
C       *              A 3 5 A 8 A A 10 A 2         (A - ARBITRARY).
C       *           Q(N+1)  POINTS TO THE FIRST NONZERO IN THE ROW AND
C       *           THE LAST NONZERO POINTS TO N+1.  AS THE ALGORITHM
C       *           PROCEEDS, OTHER ELEMENTS OF  Q  ARE INSERTED IN THE
C       *           LIST BECAUSE OF FILLIN.
C       *           SIZE = N+1.
C NIA   * IM    - AT EACH STEP IN THE FACTORING,  IM(I)  IS THE LAST
C       *           ELEMENT OF THE ITH ROW OF  U  WHICH NEEDS TO BE
C       *           CONSIDERED IN COMPUTING FILLIN.
C       *           SIZE = N.
C  INTERNAL VARIABLES--
C    JLPTR - POINTS TO THE LAST POSITION USED IN  JU FOR L.
C    JUPTR - POINTS TO THE LAST POSITION USED IN  JU.
C    JMIN,JMAX - ARE THE INDICES IN  A OR U  OF THE FIRST AND LAST
C                ELEMENTS TO BE EXAMINED IN A GIVEN ROW.
C                FOR EXAMPLE,  JMIN=IA(K), JMAX=IA(K+1)-1.
C  ******  INITIALIZE POINTERS  ***************************************
      IL(1) = 1
      IERR=0
      JUPTR = 0
      IU(1) = 1
       NP1=N+1
C  ******  FOR EACH ROW OF L AND U  ***********************************
      DO  13 K = 1, N
C  ******  SET Q TO THE REORDERED ROW OF A  ***************************
         Q(NP1) = NP1
         M=NP1
         TEMP = R(K)
         CALL GETROW(TEMP,JROW,NUM)
         IF (NUM.LT.1) GO TO 14
         DO  3 J = 1,NUM
            TEMP = JROW(J)
            VJ = IC(TEMP)
            IF (M.GT.VJ)M=NP1
 1            QM=Q(M)
              IF(QM.GE.VJ) GO TO 2
              M=QM
              GO TO 1
   2        IF (QM .EQ. VJ) GOTO  15
            Q(M) = VJ
            Q(VJ) = QM
   3        CONTINUE
C  ******  FOR EACH ENTRY IN THE LOWER TRIANGLE  **********************
         I = N+1
   4        I = Q(I)
            IF (I .GE. K) GOTO  10
C  ******  L(K,I) WILL BE NONZERO, SO ADD IT TO JL  *******************
            JUPTR = JUPTR+1
            IF (JUPTR .GT. JUMAX) GOTO  17
            JU(JUPTR) = I
            QM = I
C  ******  INSPECT ITH ROW FOR FILLIN, ADJUST IM IF POSSIBLE  *********
            JMIN = IU(I)
            JMAX = IM(I)
            IF (JMIN .GT. JMAX) GOTO 9
               DO  8 J = JMIN, JMAX
                  VJ = JU(J)
                  IF (VJ .EQ. K) IM(I) = J
   5                 M = QM
                     QM = Q(M)
                     IF (QM .GE. VJ) GOTO  6
                     GOTO  5
   6              IF (QM .EQ. VJ) GOTO 7
                     Q(M) = VJ
                     Q(VJ) = QM
                     QM = VJ
   7              CONTINUE
   8              CONTINUE
   9        CONTINUE
C  ******  CHECK FOR NULL PIVOT  **************************************
            GOTO  4
  10     IF (I .NE. K) GOTO  16
         JUPTR = JUPTR+1
         IF (JUPTR .GT. JUMAX) GOTO  17
         JU(JUPTR) = I
         IU(K) = JUPTR+1
C  ******  REMAINING ELEMENTS OF Q DEFINE STRUCTURE OF U(K, )  ********
  11        I = Q(I)
            IF (I .GT. N) GOTO  12
            JUPTR = JUPTR+1
            IF (JUPTR .GT. JUMAX) GOTO  17
            JU(JUPTR) = I
C  ******  GET READY FOR NEXT ROW  ************************************
            GOTO  11
  12     IM(K) = JUPTR
         IL(K+1) = JUPTR+1
  13     CONTINUE
      RETURN
C  NULL ROW IN A
  14   IERR=R(K)+10
      RETURN
C  DUPLICATE ENTRY IN ROW OF A
 15   IERR= 10+N+K
      RETURN
C  NULL PIVOT
  16  IERR=10+K+3*N
      RETURN
C  JL STORAGE EXCEEDED
  17  IERR=2*N+K+10
      RETURN
      END
