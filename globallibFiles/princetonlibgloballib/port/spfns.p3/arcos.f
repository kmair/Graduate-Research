      REAL FUNCTION ARCOS(X)
      REAL X
      REAL T, ATAN2, ABS, SQRT
C    THIS FUNCTION SUBPROGRAM COMPUTES ARCCOS(X) MAKING USE OF THE
C    BUILT-IN FUNCTION ATAN2(A1,A2), WHICH COMPUTES THE ARCTAN(A1/A2).
C    THE RANGE OF ARCCOS(X) IS BETWEEN 0 AND PI.
C    THE FORMULA USED IS  ARCCOS(X) = ARCTAN(SQRT(1-X**2)/X)
C    NOTE THAT ARCCOS(X) LOSES HALF ITS MEANINGFUL DIGITS WHEN X
C    APPROACHES -1 OR +1.
C    ERROR STATES-
C      1 - IF ABS(X) .GT. 1
C    INPUT-
C      X - THE VALUE WHOSE ARCCOS IS DESIRED
C    OUTPUT-
C       ARCOS=ARCCOS(X)
C    CHECK FOR ERROR STATES, AND PRINT OUT ERROR MESSAGE
C/6S
C     IF (ABS(X) .GT. 1.0E0) CALL SETERR(
C    1   41H ARCOS - NO SOLUTION FOR  ABS(X).GT.1.0E0, 41, 1, 2)
C/7S
      IF (ABS(X) .GT. 1.0E0) CALL SETERR(
     1   ' ARCOS - NO SOLUTION FOR  ABS(X).GT.1.0E0', 41, 1, 2)
C/
C    TO COMPUTE ARCCOS(X)
      T = SQRT(1.0E0-X*X)
      ARCOS = ATAN2(T, X)
      RETURN
      END
