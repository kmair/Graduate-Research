      SUBROUTINE C5EQK(NPTS, X, FN, M, N, P, Q, QK, EN)
      INTEGER NPTS
      INTEGER M, N
      REAL X(NPTS), FN(NPTS), P(1), Q(1), QK(NPTS), EN(NPTS)
      INTEGER I
      REAL TCHBP, PK
C   PROCEDURE C5EQK  COMPUTES EN AND QK.
C   EN=ERROR VALUES AT MESH POINTS.
C   QK=VALUE OF DENOMINATOR POLYNOMIAL AT MESH POINTS.
C/6S
C     IF (NPTS .LE. 0 .OR. M .LT. 0 .OR. N .LT. 0) CALL SETERR(
C    1   23HC5EQK-INVALID DIMENSION, 23, 1, 2)
C/7S
      IF (NPTS .LE. 0 .OR. M .LT. 0 .OR. N .LT. 0) CALL SETERR(
     1   'C5EQK-INVALID DIMENSION', 23, 1, 2)
C/
      DO  1 I = 1, NPTS
         QK(I) = TCHBP(N, Q, X(I), X(1), X(NPTS))
C/6S
C        IF (QK(I) .EQ. 0.0E0) CALL SETERR(21HC5EQK-DIVISOR .EQ. 0., 21,
C    1      2, 2)
C/7S
         IF (QK(I) .EQ. 0.0E0) CALL SETERR('C5EQK-DIVISOR .EQ. 0.', 21,
     1      2, 2)
C/
         PK = TCHBP(M, P, X(I), X(1), X(NPTS))
         EN(I) = (FN(I)*QK(I)-PK)/QK(I)
   1     CONTINUE
      RETURN
      END
