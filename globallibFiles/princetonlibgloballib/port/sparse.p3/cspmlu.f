      SUBROUTINE CSPMLU(N,MRP, MCP, IA, JA, A, IAMAX, IL, THRESH
     1   , EPS, ISIZE, GROWTH)
      INTEGER N
      INTEGER MRP(N), MCP(N), IA(1), JA(1), IAMAX, IL(N)
      INTEGER ISIZE
      REAL THRESH, EPS, GROWTH
      COMPLEX A(1)
      DOUBLE PRECISION DSTAK(500)
      COMMON /CSTAK/ DSTAK
      INTEGER ISTKGT, I, K, IERR, LAST, TEMP
      INTEGER TEMP1, IC, II(1000), IZ, IC1
      COMPLEX D(500)
      LOGICAL TEMP2
      EQUIVALENCE (D(1), II(1), DSTAK(1))
C SPARSE DECOMPOSITION
C INPUT PARAMETERS
C N      NUMBER OF EQUATIONS
C MRP     INTEGER VECTOR GIVING ROW PERMUTATION
C MCP     INTEGER VECTOR GIVING COLUMN PERMUTATIONS
C IA     INTEGER VECTOR OF LENGTH N+1 GIVING BEGINNING
C        OF EACH ROW IN THE A AND JA ARRAYS
C JA     INTEGER VECTOR OF LENGTH IAMAX,GIVING COLUMN INDICES
C        OF EACH ELEMENT IN THE A ARRAY
C A      COMPLEX ARRAY OF THE NONZERO ELEMENTS IN
C        THE COEFFICIENT MATRIX STORED BY ROWS
C        DESTROYED ON OUTPUT
C IAMAX  DIMENSION OF THE JA AND A ARRAYS,SHOULD AT
C        LEAST BE THE SIZE OF THE NUMBER OF NONZERO
C        ELMENTS IN A,PREFERABLY,TWICE AS LARGE
C THRESH REAL VARIABLE BETWEEN 0 AND 1 GIVING
C        A THRESHHOLD FOR PIVOTING. IF THRESH IS 0, NO PIVOTING
C        WILL BE DONE.  IF THRESH=1.E0,THEN GAUSSIAN
C        ELIMINATION WITH PARTIAL PIVOTING WILL BE DONE
C EPS    TEST FOR SINGULARITY
C OUTPUT PARAMETERS
C MCP    THE REORDERING DONE TO INSURE STABILITY,AS DICTATED
C        BY THRESH
C A       THE LU DECOMPOSITION OF A
C JA      THE COLUMN INDICES OF THE LU DECOMPOSITION
C IL     INTEGER VECTOR OF LENGTH N+1 WHICH POINTS TO THE
C        BEGINNING OF EACH ROW OF L IN JA ARRAY
C ISIZE  ACTUAL NUMBER OF ELEMENTS IN THE A ARRAY THAT WAS NEEDED
C        FOR THE DECOMPOSITION
C GROWTH NUMERICAL ELEMENT GROWTH. IF THIS MUCH .GT. 1, THEN THE
C        COMPUTED DECOMPOSITION MAY BE THE DECOMPOSITION OF A MATRIX
C        THAT IS NOT VERY CLOSE TO THE ORIGINAL MATRIX. RAISING
C        THRESH MIGHT ALLEVIATE THE SITUATION.
C SPACE ALLOCATED AND DEALLOCATED-2N+1 INTEGER AND N DOUBLE
C PRECISION LOCATIONS
C ERROR STATES
C 1 N.LT.1        FATAL
C 2 R  NOT IN 1 THROUGH N
C 3 C NOT IN 1 THROUGH N
C 4 IAMAX LESS THAN IA(N+1)-1
C 10+K     NULL ROW           FATAL
C 10+N+K   INVALID INDEX IN ROW K FATAL
C 10+3N+K     SINGULAR MATRIX OF RANK K      RECOVERABLE
C 10+2N+K    RAN OUT OF SPACE WHEN PROCEESING ROW K
C/6S
C     IF (N .LT. 1) CALL SETERR(13HCSPMLU-N.LT.1, 13, 1, 2)
C     IF (IAMAX .LT. IA(N+1)-1) CALL SETERR(24HCSPLU-INSUFFICIENT SPACE,
C    1   24, 4, 2)
C/7S
      IF (N .LT. 1) CALL SETERR('CSPMLU-N.LT.1', 13, 1, 2)
      IF (IAMAX .LT. IA(N+1)-1) CALL SETERR('CSPLU-INSUFFICIENT SPACE',
     1   24, 4, 2)
C/
      CALL ENTER(1)
      IZ = ISTKGT(N, 5)
      IC = ISTKGT(2*N+1, 2)
      IC1 = IC-1
      DO  1 I = 1, N
         TEMP2 = MRP(I) .LT. 1
         IF (.NOT. TEMP2) TEMP2 = MRP(I) .GT. N
C/6S
C        IF (TEMP2) CALL SETERR(23HCSPMLU-MRP OUT OF RANGE, 23, 2, 2)
C/7S
         IF (TEMP2) CALL SETERR('CSPMLU-MRP OUT OF RANGE', 23, 2, 2)
C/
         K = MCP(I)
         TEMP2 = K .LT. 1
         IF (.NOT. TEMP2) TEMP2 = K .GT. N
C/6S
C        IF (TEMP2) CALL SETERR(23HCSPMLU-MCP OUT OF RANGE, 23, 3, 2)
C/7S
         IF (TEMP2) CALL SETERR('CSPMLU-MCP OUT OF RANGE', 23, 3, 2)
C/
         TEMP1 = K+IC1
         II(TEMP1) = I
   1     CONTINUE
      TEMP = IC+N
      CALL CS4MLU(N,IA, JA, A, IAMAX, IL, II(TEMP), D(IZ), II(IC), MRP
     1   , MCP, IERR, THRESH, EPS, LAST, GROWTH)
      IF (IERR .EQ. 0) GOTO 6
         IF (IERR .GT. N+10) GOTO 2
C/6S
C           CALL SETERR(15HCSPMLU-NULL ROW, 15, IERR, 2)
C/7S
            CALL SETERR('CSPMLU-NULL ROW', 15, IERR, 2)
C/
            GOTO  5
C/6S
C  2        IF (IERR .LE. 2*N+10) CALL SETERR(
C    1         29HCSPMLU-INCORRECT COLUMN INDEX, 29, IERR, 2)
C/7S
   2        IF (IERR .LE. 2*N+10) CALL SETERR(
     1         'CSPMLU-INCORRECT COLUMN INDEX', 29, IERR, 2)
C/
            IF (IERR .LE. 3*N+10) GOTO 3
C/6S
C              CALL SETERR(25HCSPMLU-INSUFFICIENT SPACE, 25, IERR, 1)
C/7S
               CALL SETERR('CSPMLU-INSUFFICIENT SPACE', 25, IERR, 1)
C/
               CALL LEAVE
               GOTO  4
C/6S
C  3           CALL SETERR(22HCSPMLU-SINGULAR MATRIX, 22, IERR, 1)
C/7S
   3           CALL SETERR('CSPMLU-SINGULAR MATRIX', 22, IERR, 1)
C/
               CALL LEAVE
   4        RETURN
   5  CONTINUE
   6  ISIZE = LAST-1
      CALL LEAVE
      RETURN
      END
