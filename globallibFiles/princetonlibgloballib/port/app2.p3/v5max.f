      REAL FUNCTION V5MAX(M, A, INDEX)
      INTEGER M
      INTEGER INDEX
      REAL A(M)
      INTEGER I
      REAL B
C  THIS REAL FUNCTION RETURNS THE LARGEST COMPONENT
C  OF THE REAL VECTOR A.
C/6S
C     IF (M .LT. 1) CALL SETERR(26HV5MAX  - INVALID DIMENSION, 26, 1, 2)
C/7S
      IF (M .LT. 1) CALL SETERR('V5MAX  - INVALID DIMENSION', 26, 1, 2)
C/
      B = A(1)
      INDEX = 1
      DO  2 I = 1, M
         IF (A(I) .LE. B) GOTO 1
            B = A(I)
            INDEX = I
   1     CONTINUE
   2     CONTINUE
      V5MAX = B
      RETURN
      END
