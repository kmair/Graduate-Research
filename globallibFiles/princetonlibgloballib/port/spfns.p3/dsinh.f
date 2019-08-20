      DOUBLE PRECISION FUNCTION DSINH(X)
      DOUBLE PRECISION X
      DOUBLE PRECISION T, CT, X2, S, FACT, SUM
      DOUBLE PRECISION DEXP
C    THIS FUNCTION SUBPROGRAM COMPUTES SINH(X) IN DOUBLE PRECISION.
C    THE GENERAL FORMULA USED HERE IS
C        SINH(X) = (EXP(X)-EXP(-X))/2
C    EXCEPT FOR THE CASE WHEN X IS SMALL.
C    IN THE GENERAL CASE, SINCE SINH(X) IS AN ODD FUNCTION, AND FOR
C    REASONS OF OVERFLOW IN THE MACHINE, IT IS REWRITTEN AS
C    SINH(X)  =  SIGN(X)*(EXP(ABS(X/2))*(EXP(ABS(X/2))*0.5))-
C                (0.5/EXP(ABS(X/2)))/EXP(ABS(X/2))
C    IN THE CASE FOR SMALL X, A SERIES EXPANSION
C        SINH(X) = X * (SUM(FROM 0 TO INFINITY) (X**2N)/FACT(2N+1))
C    IS USED IN PLACE OF THE GENERAL FORMULA.  THIS SERIES EXPANSION
C    GIVES ACCURATE RESULTS WITH RELATIVELY FEW TERMS.
C    INPUT-
C      X - THE VALUE WHOSE SINH IS DESIRED
C    OUTPUT-
C      DSINH = SINH(X)
      IF (DABS(X) .LT. 0.125D0) GOTO 1
         T = DEXP(DABS(X/2.0D0))
         DSINH = DSIGN(T*(0.5D0*T)-(0.5D0/T)/T, X)
         RETURN
   1  SUM = 1.0D0
      FACT = 1.0D0
      CT = 1.0D0
      X2 = X*X
      GOTO  3
   2     CONTINUE
   3     CT = CT+2.0D0
         FACT = (FACT*X2)/((CT-1.0D0)*CT)
         S = SUM+FACT
         IF (S .EQ. SUM) GOTO  4
         SUM = S
         GOTO  2
   4  DSINH = X*SUM
      RETURN
      END