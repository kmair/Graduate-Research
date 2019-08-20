      SUBROUTINE RCSM(A, M, N, R, C, RC)
      INTEGER M, N
      INTEGER RC(N)
      REAL A(M, N), R(M), C(N)
C TO GET THE ROW AND COLUMN SCALE FACTORS FOR A.
C/6S
C     IF (M .LT. 1) CALL SETERR(16H RCSM - M .LT. 1, 16, 1, 2)
C     IF (N .LT. 1) CALL SETERR(16H RCSM - N .LT. 1, 16, 2, 2)
C/7S
      IF (M .LT. 1) CALL SETERR(' RCSM - M .LT. 1', 16, 1, 2)
      IF (N .LT. 1) CALL SETERR(' RCSM - N .LT. 1', 16, 2, 2)
C/
C GET THE ROW FACTOR.
      CALL RCSR(A, M, N, R)
C GET THE COLUMN SCALE FACTOR.
      CALL RCSC(A, M, N, R, C, RC)
      RETURN
      END