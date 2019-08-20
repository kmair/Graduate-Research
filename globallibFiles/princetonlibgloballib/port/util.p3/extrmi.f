      SUBROUTINE EXTRMI(NPTS, FN, NEX, IEXT, IMAX, IMIN, IMAG)
      INTEGER NPTS
      INTEGER FN(NPTS), NEX, IEXT(NPTS), IMAX, IMIN, IMAG
      INTEGER EXT, SEXT, EMAX, EMIN, IABS, I
C   THIS SUBROUTINE FINDS THE EXTREME POINTS OF THE DISCRETE
C   FUNCTION FN(I).
C   INPUT-
C   NPTS   - THE NUMBER OF POINTS
C   FN     - THE DISCRETE FUNCTION.
C   OUTPUT-
C   NEX    - THE NUMBER OF EXTREMA.
C   IEXT   - THE INDICES OF THE EXTREMAL POINTS IN THE DISCRETE
C            FUNCTION FN(I).
C   IMAX   - THE INDEX OF THE MAXIMUM FUNCTION VALUE.
C   IMIN   - THE INDEX OF THE MINIMUM FUNCTION VALUE.
C   IMAG   - THE INDEX OF THE FUNCTION VALUE WITH
C            MAXIMUM MAGNITUDE.
C   ERROR STATES
C   1      - INVALID DIMENSION.
C/6S
C     IF (NPTS .LT. 1) CALL SETERR(26HEXTREM - INVALID DIMENSION, 26, 1,
C    1   2)
C/7S
      IF (NPTS .LT. 1) CALL SETERR('EXTREM - INVALID DIMENSION', 26, 1,
     1   2)
C/
      NEX = 1
      IEXT(1) = 1
      EXT = FN(1)
      IMAX = 1
      EMAX = EXT
      IMIN = 1
      EMIN = EXT
      IF (EXT .EQ. 0) GOTO 1
         SEXT = EXT/IABS(EXT)
         GOTO  2
   1     SEXT = 0
   2  I = 2
         GOTO  4
   3     I = I+1
   4     IF (I .GT. NPTS)GOTO  14
         IF (FN(I)*SEXT .GE. 0) GOTO 5
            NEX = NEX+1
            GOTO  9
   5        IF (IABS(FN(I)) .GT. IABS(EXT)) GOTO 8
               IF (NEX .LE. 1) GOTO 6
                  GOTO  3
   6              GOTO  10
   8     CONTINUE
   9     IEXT(NEX) = I
         EXT = FN(I)
         SEXT = EXT/IABS(EXT)
  10     IF (EMAX .GE. FN(I)) GOTO 11
            IMAX = I
            EMAX = FN(I)
            GOTO  13
  11        IF (FN(I) .GE. EMIN) GOTO 12
               IMIN = I
               EMIN = FN(I)
  12     CONTINUE
  13     CONTINUE
         GOTO  3
  14  IF (EMAX .GE. IABS(EMIN)) GOTO 15
         IMAG = IMIN
         GOTO  16
  15     IMAG = IMAX
  16  RETURN
      END