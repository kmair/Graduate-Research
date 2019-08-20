      SUBROUTINE VDTOB( E10, M10, E, M )
C
C     VDTOB CONVERTS A BASE 10 REPRESENTATION
C     OF A FLOATING POINT NUMBER INTO A MACHINE BASE
C     REPRESENTATION.
C
      REAL M, M10, BASE
      DOUBLE PRECISION D1MACH, L10B, DLOG10
      INTEGER E, E10
C
      DATA L10B / 0.0D0 /
      DATA BASE / 0.0E0 /
C
      IF( BASE .NE. 0.0E0 ) GO TO 1
        L10B = D1MACH(5)
        BASE = I1MACH(10)
C
 1    IF( M10 .NE. 0.0E0 ) GO TO 5
        M = 0.0E0
        E = 0
        RETURN
C
 5    M = ABS(M10)
      E = IDFLR( (DBLE(FLOAT(E10)) + DLOG10(DBLE(M)))/L10B )
      M = ( 10.0D0 ** (DBLE(FLOAT(E10)) - DBLE(FLOAT(E))*L10B) ) * M
C
 10   IF( M .LT. 1.0E0 ) GO TO 20
        M = M / BASE
        E = E + 1
        GO TO 10
C
 20   IF( M10 .LT. 0.0E0 ) M = -M
      RETURN
C
      END
