      SUBROUTINE DVBTOD( E, M, E10, M10 )
C
C     DVBTOD CONVERTS A MACHINE BASE REPRESENTATION
C     OF A FLOATING POINT NUMBER INTO A BASE 10
C     REPRESENTATION.
C
      DOUBLE PRECISION M, M10, EL10B, DLOG10
      DOUBLE PRECISION D1MACH
      INTEGER E, E10
C
      IF( M .NE. 0.0D0 ) GO TO 5
        M10 = 0.0D0
        E10 = 0
        RETURN
C
  5   CALL DUMKFL(M,E10,M10)
      EL10B = DBLE(FLOAT(E+E10))*D1MACH(5)
C
      M10 = DABS(M10)
      E10 = IDFLR( EL10B + DLOG10(M10) )
      M10 = (10.0D0 ** (EL10B - DBLE(FLOAT(E10)))) * M10
C
 10   IF( M10 .LT. 1.0D0 ) GO TO 20
        M10 = M10/10.0D0
        E10 = E10 + 1
        GO TO 10
C
 20   IF( M .LT. 0.0D0 ) M10 = -M10
      RETURN
C
      END
