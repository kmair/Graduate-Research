      SUBROUTINE CSPFLU(N, MRP, MCP, AROW, IWORK, UL, IAMAX,
     1   THRESH, EPS, ISIZE, GROWTH)
      INTEGER N, IAMAX
      EXTERNAL AROW
      INTEGER MRP(N), MCP(N), IWORK(IAMAX), ISIZE
      REAL  THRESH, EPS, GROWTH
      COMPLEX UL(N)
      DOUBLE PRECISION DSTAK(500)
      COMMON /CSTAK/ DSTAK
      INTEGER JLU, ISTKGT, I, K, IERR, LAST
      INTEGER TEMP, TEMP1, IC, II(1), IZ, IC1
      REAL  ANORM
      COMPLEX D(500)
      LOGICAL TEMP2
      EQUIVALENCE (D(1), II(1), DSTAK(1))
C SPARSE DECOMPOSITION
C INPUT PARAMETERS
C N      NUMBER OF EQUATIONS
C MRP     INTEGER VECTOR GIVING ROW PERMUTATION
C MCP     INTEGER VECTOR GIVING COLUMN PERMUTATIONS
C AROW    SUBROUTINE OF THE FORM AROW(I,ROW,JCOL,NUM) WHICH
C         FOR A GIVEN INPUT I RETURNS THE NONZERO ELEMENTS OF
C         THE ITH ROW OF THE MATRIX A IN THE
C         COMPLEX VECTOR ROW AND THE CORRESPONDING INDICES IN
C         JCOL. THE VARIABLE NUM RETURNS THE NUMBER OF NONZERO
C         ELEMENTS IN THE ITH ROW. AROW SHOULD BE DECLARED
C         EXTERNAL IN THE CALLING PROGRAM.
C IAMAX  DIMENSION OF THE UL ARRAY,SHOULD AT
C        LEAST BE THE SIZE OF  THE NUMBER OF NONZERO
C        ELMENTS IN A,PREFERABLY,TWICE AS LARGE
C THRESH REAL VARIABLE BETWEEN 0 AND 1 GIVING
C        A THRESHHOLD FOR PIVOTING. IF THRESH IS 0, NO PIVOTING
C        WILL BE DONE.  IF THRESH=1.E0,THEN GAUSSIAN
C        ELIMINATION WITH PARTIAL PIVOTING WILL BE DONE
C EPS    TEST FOR SINGULARITY
C OUTPUT PARAMETERS
C C      THE REORDERING DONE TO INSURE STABILITY,AS DICTATED
C        BY THRESH
C IWORK   INTEGER VECTOR ,LENGTH 2N+1+IAMAX WHOSE FIRST N+1 COMOPONENTS
C         POINT TO THE BEGINNING OF TH ROWS OF L IN THE UL
C         AND WHOSE NEXT N COMPONENTS POINT TO THE BEGINNING
C         OF THE ROWS OF U IN THE UL. COMPONENTS 2N+2 UNTIL
C         2N+1+ISIZE GIVE THE COLUMN INDICES OF THE COMPONENTS
C         OF UL
C UL      THE LU DECOMPOSITION OF THE SPARSE MATRIX A
C ISIZE  ACTUAL NUMBER OF ELEMENTS IN THE A ARRAY THAT WAS NEEDED
C        FOR THE DECOMPOSITION
C GROWTH NUMERICAL ELEMENT GROWTH. IF GROWTH MUCH .GT. 1,WORRY
C SPACE ALLOCATED AND DEALLOCATED-2N+1 INTEGER AND N DOUBLE
C PRECISION LOCATIONS
C ERROR STATES
C 1 N.LT.1        FATAL
C 2 R AND C NOT IN 1 THROUGH N
C 3 IAMAX LESS THAN N
C 10+K     NULL ROW           FATAL
C 3N+K+10     SINGULAR MATRIX OF RANK K      RECOVERABLE
C 2N+K+10    RAN OUT OF SPACE WHEN PROCEESING ROW K
C N+K+10   INVALID COLUMN INDEX WHEN PROCESSING ROW K
C/6S
C     IF (N .LT. 1) CALL SETERR(13HCSPFLU-N.LT.1, 13, 1, 2)
C     IF (IAMAX .LT. N) CALL SETERR(25HCSPFLU-INSUFFICIENT SPACE, 25, 3,
C    1   2)
C/7S
      IF (N .LT. 1) CALL SETERR('CSPFLU-N.LT.1', 13, 1, 2)
      IF (IAMAX .LT. N) CALL SETERR('CSPFLU-INSUFFICIENT SPACE', 25, 3,
     1   2)
C/
      CALL ENTER(1)
      IZ = ISTKGT(N, 5)
      IC = ISTKGT(2*N+1, 2)
      IC1 = IC-1
      DO  1 I = 1, N
         TEMP2 = MRP(I) .LT. 1
         IF (.NOT. TEMP2) TEMP2 = MRP(I) .GT. N
C/6S
C        IF (TEMP2) CALL SETERR(23HCSPFLU-MRP OUT OF RANGE, 23, 2, 2)
C/7S
         IF (TEMP2) CALL SETERR('CSPFLU-MRP OUT OF RANGE', 23, 2, 2)
C/
         K = MCP(I)
         TEMP2 = K .LT. 1
         IF (.NOT. TEMP2) TEMP2 = K .GT. N
C/6S
C        IF (TEMP2) CALL SETERR(23HCSPFLU-MCP OUT OF RANGE, 23, 2, 2)
C/7S
         IF (TEMP2) CALL SETERR('CSPFLU-MCP OUT OF RANGE', 23, 2, 2)
C/
         TEMP1 = K+IC1
         II(TEMP1) = I
   1     CONTINUE
      TEMP = IC+N
      JLU = 2*N+2
      CALL CS4FLU(N,IWORK, IWORK(JLU), UL, IAMAX, IWORK(N+2), II(TEMP)
     1   , D(IZ), II(IC), MRP, MCP, IERR, THRESH, EPS, LAST, AROW,
     1   GROWTH, ANORM)
      IF (IERR .EQ. 0) GOTO 8
         IF (IERR .GT. N+10) GOTO 2
C/6S
C           CALL SETERR(15HCSPFLU-NULL ROW, 15, IERR, 2)
C/7S
            CALL SETERR('CSPFLU-NULL ROW', 15, IERR, 2)
C/
            GOTO  7
   2        IF (IERR .LE. 3*N+10) GOTO 3
C/6S
C              CALL SETERR(22HCSPFLU-SINGULAR MATRIX, 22, IERR, 1)
C/7S
               CALL SETERR('CSPFLU-SINGULAR MATRIX', 22, IERR, 1)
C/
               GOTO  6
   3           IF (IERR .GT. 2*N+10) GOTO 4
C/6S
C                 CALL SETERR(29HCSPFLU-INCORRECT COLUMN INDEX, 29,
C    1               IERR, 2)
C/7S
                  CALL SETERR('CSPFLU-INCORRECT COLUMN INDEX', 29,
     1               IERR, 2)
C/
                  GOTO  5
C/6S
C  4              CALL SETERR(25HCSPFLU-INSUFFICIENT SPACE, 25, IERR+N
C    1               , 1)
C/7S
   4              CALL SETERR('CSPFLU-INSUFFICIENT SPACE', 25, IERR+N
     1               , 1)
C/
   5        CONTINUE
   6     CONTINUE
   7  CONTINUE
   8  ISIZE = LAST-1
      CALL LEAVE
      RETURN
      END
