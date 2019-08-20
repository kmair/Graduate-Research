      SUBROUTINE RCSBA(A, M, N, R, C, RC)
      INTEGER M, N
      INTEGER RC(M)
      REAL A(M, N), R(M), C(M)
      INTEGER NERROR, NERR
C TO GET THE ROW AND COLUMN SCALE FACTORS FOR A
C AND PERFORM THE SCALING.
C/6S
C     IF (M .LT. 1) CALL SETERR(17H RCSBA - M .LT. 1, 17, 1, 2)
C     IF (N .LT. 1) CALL SETERR(17H RCSBA - N .LT. 1, 17, 2, 2)
C/7S
      IF (M .LT. 1) CALL SETERR(' RCSBA - M .LT. 1', 17, 1, 2)
      IF (N .LT. 1) CALL SETERR(' RCSBA - N .LT. 1', 17, 2, 2)
C/
      CALL ENTER(1)
C GET ROW AND COLUMN SCALE FACTORS.
      CALL RCSBM(A, M, N, R, C, RC)
C APPLY THE SCALE FACTORS TO A.
      CALL RCSBS(A, M, N, R, C, RC)
      IF (NERROR(NERR) .EQ. 0) GOTO 1
         CALL ERROFF
C/6S
C        CALL SETERR(43H RCSBA - MUST HAVE 1/(S*L) IN MACHINE RANGE, 43,
C    1      3, 1)
C/7S
         CALL SETERR(' RCSBA - MUST HAVE 1/(S*L) IN MACHINE RANGE', 43,
     1      3, 1)
C/
         RETURN
   1  CALL LEAVE
      RETURN
      END
