      DOUBLE PRECISION FUNCTION DACOSH(X)
      DOUBLE PRECISION X
      DOUBLE PRECISION T, DSQRT, DLOG
C    THIS FUNCTION SUBPROGRAM COMPUTES ARCCOSH(X) IN DOUBLE PRECISION.
C    THE FORMULA USED IN THIS COMPUTATION IS -
C      ARCCOSH(X) = LN(X+ SQRT(X**2 -1))
C    THE FUNCTION ONLY EXISTS FOR X GREATER THAN OR EQUAL TO 1.
C    IN ORDER TO AVOID OVERFLOW IN THE MACHINE WITH THE ABOVE
C    REPRESENTATION, IT IS REWRITTEN AS -
C      ARCCOSH(X) = DLOG(X) + DLOG(1 + SQRT((1+1/X)*(1-1/X)))
C    INPUT-
C      X - VALUE WHOSE ARCCOSH IS DESIRED
C    OUTPUT-
C      DACOSH = ARCCOSH(X)
C    ERROR STATES-
C      1 - X .LT. 1
C/6S
C     IF (X .LT. 1.0D0) CALL SETERR(
C    1   35HDACOSH - NO SOLUTION FOR X.LT.1.0D0, 35, 1, 2)
C/7S
      IF (X .LT. 1.0D0) CALL SETERR(
     1   'DACOSH - NO SOLUTION FOR X.LT.1.0D0', 35, 1, 2)
C/
      T = DSQRT((1.0D0/X+1.0D0)*(1.0D0-1.0D0/X))
      DACOSH = DLOG(X)+DLOG(T+1.0D0)
      RETURN
      END
