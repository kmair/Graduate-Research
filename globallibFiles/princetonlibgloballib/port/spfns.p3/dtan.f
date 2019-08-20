      DOUBLE PRECISION FUNCTION DTAN(X)
      DOUBLE PRECISION X
      DOUBLE PRECISION DSIN, DCOS
C    THIS FUNCTION SUBPROGRAM COMPUTES TAN(X) MAKING USE OF THE TWO
C    BUILT-IN SINE AND COSINE FUNCTIONS.  THE FORMULA USED IS
C        TAN(X)  =  SIN(X) / COS(X)
      DTAN = DSIN(X)/DCOS(X)
      RETURN
      END