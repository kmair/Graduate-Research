      SUBROUTINE RCSBR(A, M, N, R)
      INTEGER M, N
      REAL A(M, N), R(M)
      INTEGER I, J, N2, MIN0, MAX0
      REAL ABS, AMAX1
      INTEGER TEMP, TEMP1
C TO GET THE ROW SCALE FACTOR FOR A.
      CALL ENTER(1)
C/6S
C     IF (M .LT. 1) CALL SETERR(17H RCSBR - M .LT. 1, 17, 1, 2)
C     IF (N .LT. 1) CALL SETERR(17H RCSBR - N .LT. 1, 17, 2, 2)
C/7S
      IF (M .LT. 1) CALL SETERR(' RCSBR - M .LT. 1', 17, 1, 2)
      IF (N .LT. 1) CALL SETERR(' RCSBR - N .LT. 1', 17, 2, 2)
C/
      N2 = (N+1)/2
      DO  2 I = 1, M
         R(I) = 0
         TEMP1 = MAX0(1, N2+1-I)
         TEMP = MIN0(N, M+N2-I)
         DO  1 J = TEMP1, TEMP
            R(I) = AMAX1(ABS(A(I, J)), R(I))
   1        CONTINUE
   2     CONTINUE
      CALL LEAVE
      RETURN
      END
