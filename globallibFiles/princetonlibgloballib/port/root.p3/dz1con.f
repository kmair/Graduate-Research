      DOUBLE PRECISION FUNCTION DZ1CON(N, R)
      INTEGER N
      DOUBLE PRECISION R(N, N)
      INTEGER I, NP, J
      DOUBLE PRECISION D1MACH, RMIN, RMAX
C LOWER BOUND ON CONDITION NUMBER OF UPPER TRIANGULAR MATRIX R.
      RMAX = DABS(R(N, N))
      RMIN = RMAX
      I = N-1
         GOTO  2
   1     I = I-1
   2     IF (I .LT. 1)GOTO  4
         NP = N+1
         DO  3 J = NP, N
            RMAX = DMAX1(RMAX, DABS(R(I, J)))
   3        CONTINUE
         RMIN = DMIN1(RMIN, DABS(R(I, I)))
         GOTO  1
   4  IF (RMIN .GE. 1.0D0 .AND. RMAX .GT. 0.0D0) GOTO 6
         IF (RMAX .LT. 0.5D0*D1MACH(2)*RMIN) GOTO 5
            DZ1CON = D1MACH(2)
            RETURN
   5  CONTINUE
   6  DZ1CON = RMAX/RMIN
      RETURN
      END
