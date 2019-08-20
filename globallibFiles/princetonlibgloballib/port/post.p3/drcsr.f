      SUBROUTINE DRCSR(A, M, N, R)
      INTEGER M, N
      DOUBLE PRECISION A(M, N), R(M)
      INTEGER I, J
      DOUBLE PRECISION DABS, DMAX1
C TO GET THE ROW SCALE FACTOR FOR A.
C/6S
C     IF (M .LT. 1) CALL SETERR(16HDRCSR - M .LT. 1, 16, 1, 2)
C     IF (N .LT. 1) CALL SETERR(16HDRCSR - N .LT. 1, 16, 2, 2)
C/7S
      IF (M .LT. 1) CALL SETERR('DRCSR - M .LT. 1', 16, 1, 2)
      IF (N .LT. 1) CALL SETERR('DRCSR - N .LT. 1', 16, 2, 2)
C/
      DO  2 I = 1, M
         R(I) = 0
         DO  1 J = 1, N
            R(I) = DMAX1(DABS(A(I, J)), R(I))
   1        CONTINUE
   2     CONTINUE
      RETURN
      END
