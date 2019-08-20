      INTEGER FUNCTION L5RGXD(NPTS, EN, NEX, IEXT, ILRG, TOL)
      INTEGER NPTS, NEX
      INTEGER IEXT(NEX), ILRG, TOL
      DOUBLE PRECISION EN(NPTS)
      INTEGER J, K, L
      DOUBLE PRECISION HOLD
C    FUNCTION L5RGXD FINDS THE NO. OF ERROR EXTREMA WITH MAGNITUDES
C    WITHIN TOLERANCE OF MAGNITUDE OF LARGEST ERROR.
C/6S
C     IF (NPTS .LE. 0) CALL SETERR(24HL5RGXD-INVALID DIMENSION, 24, 1, 2
C    1   )
C     IF (NEX .LE. 0 .OR. ILRG .LE. 0) CALL SETERR(
C    1   20HL5RGXD-INVALID INDEX, 20, 2, 2)
C/7S
      IF (NPTS .LE. 0) CALL SETERR('L5RGXD-INVALID DIMENSION', 24, 1, 2
     1   )
      IF (NEX .LE. 0 .OR. ILRG .LE. 0) CALL SETERR(
     1   'L5RGXD-INVALID INDEX', 20, 2, 2)
C/
      K = 0
      DO  1 J = 1, NEX
         L = IEXT(J)
         HOLD = DABS(EN(ILRG))-DABS(EN(L))
         IF (HOLD .LE. 10.0D0**(-TOL)*DABS(EN(ILRG))) K = K+1
   1     CONTINUE
      L5RGXD = K
      RETURN
      END
