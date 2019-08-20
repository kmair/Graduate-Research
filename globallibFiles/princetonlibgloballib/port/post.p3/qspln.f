      SUBROUTINE QSPLN(A, NA, NXMK, K, Y, NY, INTVAL, SBASIS, ND
     1   , SPLINE)
      INTEGER K, NXMK, NA, ND, NY
      INTEGER INTVAL
      REAL A(NXMK, NA), Y(NY), SBASIS(NY, K, ND), SPLINE(NY, NA, ND)
      INTEGER MIN0, MAX0, J, IA, ID, IY
      REAL T
      INTEGER TEMP
      LOGICAL TEMP1
C TO EVALUATE A B-SPLINE WHEN THE BASIS SPLINES ARE KNOWN.
C SCRATCH SPACE ALLOCATED - NONE.
C/6S
C     IF (NA .LT. 1) CALL SETERR(16H QSPLN - NA.LT.1, 16, 1, 2)
C     IF (NXMK .LE. 0) CALL SETERR(18H QSPLN - NXMK.LE.0, 18, 2, 2)
C     IF (K .LT. 2) CALL SETERR(15H QSPLN - K.LT.2, 15, 3, 2)
C     IF (NY .LT. 1) CALL SETERR(16H QSPLN - NY.LT.1, 16, 4, 2)
C/7S
      IF (NA .LT. 1) CALL SETERR(' QSPLN - NA.LT.1', 16, 1, 2)
      IF (NXMK .LE. 0) CALL SETERR(' QSPLN - NXMK.LE.0', 18, 2, 2)
      IF (K .LT. 2) CALL SETERR(' QSPLN - K.LT.2', 15, 3, 2)
      IF (NY .LT. 1) CALL SETERR(' QSPLN - NY.LT.1', 16, 4, 2)
C/
      TEMP1 = INTVAL .LT. 1
      IF (.NOT. TEMP1) TEMP1 = INTVAL .GT. NXMK+K-1
C/6S
C     IF (TEMP1) CALL SETERR(
C    1   45H QSPLN - INTERVAL POINTS OUTSIDE (X(1),X(NX)), 45, 5, 2)
C     IF (ND .LT. 1) CALL SETERR(16H QSPLN - ND.LT.1, 16, 6, 2)
C/7S
      IF (TEMP1) CALL SETERR(
     1   ' QSPLN - INTERVAL POINTS OUTSIDE (X(1),X(NX))', 45, 5, 2)
      IF (ND .LT. 1) CALL SETERR(' QSPLN - ND.LT.1', 16, 6, 2)
C/
      DO  6 IY = 1, NY
         DO  5 ID = 1, ND
            DO  4 IA = 1, NA
               T = 0
               J = MAX0(INTVAL-K+1, 1)
                  GOTO  2
   1              J = J+1
   2              IF (J .GT. MIN0(INTVAL, NXMK)) GOTO  3
                  TEMP = J+K-INTVAL
                  T = T+A(J, IA)*SBASIS(IY, TEMP, ID)
                  GOTO  1
   3           SPLINE(IY, IA, ID) = T
   4           CONTINUE
   5        CONTINUE
   6     CONTINUE
      RETURN
      END
