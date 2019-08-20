      DOUBLE PRECISION FUNCTION DDOT(N, SX, INCX, SY, INCY)
      INTEGER N, INCX, INCY
      DOUBLE PRECISION SX(INCX, 1), SY(INCY, 1)
      INTEGER I
C        RETURNS DOT PRODUCT OF SX AND SY
      DDOT = 0.0D0
      IF (N .EQ. 0) RETURN
C/6S
C     IF (N .LT. 0) CALL SETERR(17H  DDOT - N .LT. 0, 17, 1, 2)
C     IF (INCX .LE. 0) CALL SETERR(19H  DDOT - INCX .LT.0, 20, 1, 2)
C     IF (INCY .LE. 0) CALL SETERR(19H  DDOT - INCY .LT.0, 20, 1, 2)
C/7S
      IF (N .LT. 0) CALL SETERR('  DDOT - N .LT. 0', 17, 1, 2)
      IF (INCX .LE. 0) CALL SETERR('  DDOT - INCX .LT.0', 20, 1, 2)
      IF (INCY .LE. 0) CALL SETERR('  DDOT - INCY .LT.0', 20, 1, 2)
C/
      DO  1 I = 1, N
         DDOT = DDOT+SX(1, I)*SY(1, I)
   1     CONTINUE
      RETURN
      END
