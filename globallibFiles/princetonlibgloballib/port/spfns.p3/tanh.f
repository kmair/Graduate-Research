      REAL FUNCTION TANH(X)
      REAL X
      REAL T, S, CT, FACT, SUM, X2
      REAL SIGN, ABS, EXP
C    THIS FUNCTION SUBPROGRAM COMPUTES TANH(X) IN REAL.
C    THE GENERAL FORMULA USED HERE IS
C        TANH(X) = (EXP(X)-EXP(-X))/(EXP(X)+EXP(-X))
C    EXCEPT FOR THE CASE WHEN X IS SMALL.
C    IN THE GENERAL CASE, SINCE TANH(X) IS AN ODD FUNCTION, AND FOR
C    REASONS OF OVERFLOW IN THE MACHINE, IT IS REWRITTEN AS
C        TANH(X) = SIGN(X) * (1-(EXP(-ABS(X))**2)/(1+(EXP(-ABS(X))**2)
C    IN CASE FOR SMALL X, THE SERIES EXPANSION
C    TANH(X) =
C      2X*(SUM(FROM 0 TO INF)(X**2N)/FACT(2N+1))  / (EXP(X)+EXP(-X))
C    IS USED IN PLACE OF THE GENERAL FORMULA.  THIS SERIES EXPANSION
C    GIVES ACCURATE RESULTS WITH RELATIVELY FEW TERMS.
C    INPUT-
C      X - THE VALUE WHOSE TANH IS DESIRED
C    OUTPUT-
C       TANH = TANH(X)
      IF (ABS(X) .LT. 0.125E0) GOTO 1
         T = EXP(-ABS(X))**2
         TANH = SIGN((1.0E0-T)/(T+1.0E0), X)
         RETURN
   1  SUM = 1.0E0
      FACT = 1.0E0
      CT = 1.0E0
      X2 = X*X
      GOTO  3
   2     CONTINUE
   3     CT = CT+2.0E0
         FACT = (FACT*X2)/((CT-1.0E0)*CT)
         S = SUM+FACT
         IF (S .EQ. SUM) GOTO  4
         SUM = S
         GOTO  2
   4  T = EXP(X)
      TANH = (2.0E0*X*SUM)/(T+1.0E0/T)
      RETURN
      END
