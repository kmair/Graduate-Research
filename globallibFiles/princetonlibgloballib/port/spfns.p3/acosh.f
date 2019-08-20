      REAL FUNCTION ACOSH(X)
      REAL X
      REAL T, SQRT, ALOG
C    THIS FUNCTION SUBPROGRAM COMPUTES ARCCOSH(X) IN REAL.
C    THE FORMULA USED IN THIS COMPUTATION IS -
C      ARCCOSH(X) = LN(X+ SQRT(X**2 -1))
C    THE FUNCTION ONLY EXISTS FOR X GREATER THAN OR EQUAL TO 1.
C    IN ORDER TO AVOID OVERFLOW IN THE MACHINE WITH THE ABOVE
C    REPRESENTATION, IT IS REWRITTEN AS -
C      ARCCOSH(X) = ALOG(X) + ALOG(1 + SQRT((1+1/X)*(1-1/X)))
C    INPUT-
C      X - VALUE WHOSE ARCCOSH IS DESIRED
C    OUTPUT-
C       ACOSH = ARCCOSH(X)
C    ERROR STATES-
C      1 - X .LT. 1
C/6S
C     IF (X .LT. 1.0E0) CALL SETERR(
C    1   35H ACOSH - NO SOLUTION FOR X.LT.1.0E0, 35, 1, 2)
C/7S
      IF (X .LT. 1.0E0) CALL SETERR(
     1   ' ACOSH - NO SOLUTION FOR X.LT.1.0E0', 35, 1, 2)
C/
      T = SQRT((1.0E0/X+1.0E0)*(1.0E0-1.0E0/X))
      ACOSH = ALOG(X)+ALOG(T+1.0E0)
      RETURN
      END
