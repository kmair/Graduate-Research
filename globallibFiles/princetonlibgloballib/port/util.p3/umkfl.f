      SUBROUTINE UMKFL( F, E, M )
C
C     UMKFL DECOMPOSES A FLOATING POINT NUMBER, F, INTO
C     ITS EXPONENT, E, AND MANTISSA, M.
C
C     IF F .EQ. 0, E = 0 AND M = 0.0D0
C
C     IF F .NE. 0, F = (B**E) * M, WHERE
C                  B = I1MACH(10) AND
C                  1/B .LE. ABS(M) .LT. 1.0
C
      REAL F, M
      REAL S2MACH
      INTEGER E
C
      REAL    BEXP(6)
      INTEGER EXP(6)
      INTEGER NMAX
C
      DATA NMAX /0/
C
      DATA EXP(1), BEXP(1) /1024, 1.0E0 /
      DATA EXP(2), BEXP(2) / 256, 1.0E0 /
      DATA EXP(3), BEXP(3) /  64, 1.0E0 /
      DATA EXP(4), BEXP(4) /  16, 1.0E0 /
      DATA EXP(5), BEXP(5) /   4, 1.0E0 /
      DATA EXP(6), BEXP(6) /   1, 1.0E0 /
C
C     INITIALIZE TABLES IF FIRST TIME THROUGH
C
      IF( NMAX .NE. 0 ) GO TO 20
C
      DO 10 I = 1, 6
      IF( EXP(I) .GE. I1MACH(13) ) GO TO 10
        NMAX = NMAX + 1
        EXP(NMAX) = EXP(I)
        BEXP(NMAX) = S2MACH( 1.0E0, I1MACH(10), EXP(I) )
 10   CONTINUE
C
C     HERE WE GO
C
 20   M = ABS(F)
      IF( M .NE. 0.0E0 ) GO TO 30
      E = 0
      RETURN
C
C     M .NE. 0, COMPUTE E
C
 30   E = 1
 40   IF( M .GE. 1.0E0 ) GO TO 50
        M = M * BEXP(1)
        E = E - EXP(1)
        GO TO 40
C
 50   DO 70 I = 1, NMAX
 60   IF( M .LT. BEXP(I) ) GO TO 70
        M = M / BEXP(I)
        E = E + EXP(I)
        GO TO 60
 70   CONTINUE
C
      M = M / BEXP(NMAX)
      IF( F .LT. 0.0E0 ) M = -M
      RETURN
      END
