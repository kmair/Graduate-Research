      REAL FUNCTION Z1CON(N, R)
      INTEGER N
      REAL R(N, N)
      INTEGER I, NP, J
      REAL R1MACH, RMIN, RMAX, ABS, AMAX1, AMIN1
C LOWER BOUND ON CONDITION NUMBER OF UPPER TRIANGULAR MATRIX R.
      RMAX = ABS(R(N, N))
      RMIN = RMAX
      I = N-1
         GOTO  2
   1     I = I-1
   2     IF (I .LT. 1)GOTO  4
         NP = N+1
         DO  3 J = NP, N
            RMAX = AMAX1(RMAX, ABS(R(I, J)))
   3        CONTINUE
         RMIN = AMIN1(RMIN, ABS(R(I, I)))
         GOTO  1
   4  IF (RMIN .GE. 1.0E0 .AND. RMAX .GT. 0.0E0) GOTO 6
         IF (RMAX .LT. 0.5E0*R1MACH(2)*RMIN) GOTO 5
            Z1CON = R1MACH(2)
            RETURN
   5  CONTINUE
   6  Z1CON = RMAX/RMIN
      RETURN
      END
