      SUBROUTINE SPFCE(N, MRP, MCP, AROW, IWORK, UL, IAMAX, ISIZE,
     1   COND, Z)
      INTEGER N, IAMAX
      EXTERNAL AROW
      INTEGER MRP(101), MCP(101), IWORK(IAMAX), ISIZE
      REAL UL(IAMAX), COND, Z(N)
      DOUBLE PRECISION D(500)
      COMMON /CSTAK/ D
      INTEGER JLU, ISTKGT, I, IERR, LAST, TEMP
      INTEGER TEMP1, IC, II(1000), IC1
      REAL GROWTH, R(1000), ANORM
      LOGICAL TEMP2
      EQUIVALENCE (R(1), II(1))
      EQUIVALENCE (R(1), D(1))
C THIS SUBROUTINE CALLS THE DECOMPOSITION ROUTINE AND DETERMINES
C AN ESTIMATE OF THE CONDITION NUMBER OF A REAL SPARSE MATRIX
C INPUT PARAMETERS
C    N        ORDER OF THE PROBLEM
C    MRP      N VECTOR GIVING MATRIX ROW PERMUTATIONS
C    AROW      USER WRITTEN SUBROUTINE WHICH GIVES THE NONZERO ELEMENTS
C              OF A SPECIFIED ROW AND THEIR COLUMN INDICES. THE CALLING
C              SEQUENCE IS AROW(I,ROW,JROW,NUM)
C              WHERE I IS THE SPECIFIED ROW, NUM IS THE NUMBER OF
C              NONZERO ELEMENTS IN THAT RWO, ROW IS A VECTOR OF THESE
C              ELEMENTS AND JROW IS THE CORRESPONDING COLUMN INDICES.
C    IAMAX     DECLARED LENGTH OF UL, IWORK SHOULD BE IAMAX+2N+1 LONG.
C OUTPUT PARAMETERS
C    MCP      INTEGER N VECTOR CONTAINING COLUMN PERMUTATIONS FOR
C             STABILITY
C    IWORK     INTGER VECTOR CONTAINING POINTER INFORMATION FOR LU
C             DECOMPOSITION
C    UL        NONZERO ELEMENTS OF LU DECOMPOSITION
C    ISIZE     ACTUAL NUMBER OF NONZERO ELEMENTS IN DECOMPOSITION
C    COND     AN ESTIMATE OF THE CONDITION NUMBER OF THE MATRIX A
C    Z        N VECTOR CONTAINING APPROXIMATE NULL VECTOR
C EXTRA STORAGE ALLOCATED  -  N REAL  AND 2N+1 INTEGER LOCATIONS
C THE SUBROUTINES SASUM, SPFLU, AND S4FCE ARE CALLED
C ERROR CONDITIONS-
C    1     N.LT.1       FATAL
C    2    IA.LT.N       FATAL
      CALL ENTER(1)
C/6S
C     IF (N .LT. 1) CALL SETERR(13HSPFCE-N.LT.1 , 13, 1, 2)
C/7S
      IF (N .LT. 1) CALL SETERR('SPFCE-N.LT.1 ', 13, 1, 2)
C/
C      COMPUTE NORM OF MATRIX STORED IN COMPACT FORM
C/6S
C     IF (N .LT. 1) CALL SETERR(13H SPFCE-N.LT.1, 13, 1, 2)
C     IF (IAMAX .LT. N) CALL SETERR(25H SPFCE-INSUFFICIENT SPACE, 25, 3,
C    1   2)
C/7S
      IF (N .LT. 1) CALL SETERR(' SPFCE-N.LT.1', 13, 1, 2)
      IF (IAMAX .LT. N) CALL SETERR(' SPFCE-INSUFFICIENT SPACE', 25, 3,
     1   2)
C/
      IC = ISTKGT(2*N+1, 2)
      IC1 = IC-1
      DO  1 I = 1, N
         TEMP2 = MRP(I) .LT. 1
         IF (.NOT. TEMP2) TEMP2 = MRP(I) .GT. N
C/6S
C        IF (TEMP2) CALL SETERR(23H SPFCE-MRP OUT OF RANGE, 23, 2, 2)
C/7S
         IF (TEMP2) CALL SETERR(' SPFCE-MRP OUT OF RANGE', 23, 2, 2)
C/
         MCP(I) = I
         TEMP1 = I+IC1
         II(TEMP1) = I
   1     CONTINUE
      TEMP = IC+N
      JLU = 2*N+2
      CALL S4FLU(N, IWORK, IWORK(JLU), UL, IAMAX, IWORK(N+2), II(TEMP)
     1   , Z, II(IC), MRP, MCP, IERR, 1.0, 0.0, LAST, AROW, GROWTH,
     1   ANORM)
      IF (IERR .EQ. 0) GOTO 5
         IF (IERR .GT. N+10) GOTO 2
C/6S
C           CALL SETERR(15H SPFCE-NULL ROW, 15, IERR, 2)
C/7S
            CALL SETERR(' SPFCE-NULL ROW', 15, IERR, 2)
C/
            GOTO  6
C/6S
C  2        IF (IERR .LE. 2*N+10) CALL SETERR(
C    1         29H SPFCE-INCORRECT COLUMN INDEX, 29, IERR, 2)
C/7S
   2        IF (IERR .LE. 2*N+10) CALL SETERR(
     1         ' SPFCE-INCORRECT COLUMN INDEX', 29, IERR, 2)
C/
            IF (IERR .LE. 3*N+10) GOTO 3
C/6S
C              CALL SETERR(25H SPFCE-INSUFFICIENT SPACE, 25, IERR, 1)
C/7S
               CALL SETERR(' SPFCE-INSUFFICIENT SPACE', 25, IERR, 1)
C/
               GOTO  6
   3        CONTINUE
   5  ISIZE = LAST-1
      CALL ISTKRL(1)
      TEMP = ISTKGT(N, 3)
      CALL S4FCE(N, IWORK, IWORK(JLU), UL, IAMAX, IWORK(N+2), R(
     1   TEMP), ANORM, COND, Z, MCP)
C/6S
C     IF (IERR .NE. 0) CALL SETERR(22H SPFCE-SINGULAR MATRIX, 22, IERR
C    1   , 1)
C/7S
      IF (IERR .NE. 0) CALL SETERR(' SPFCE-SINGULAR MATRIX', 22, IERR
     1   , 1)
C/
   6  CALL LEAVE
      RETURN
      END
