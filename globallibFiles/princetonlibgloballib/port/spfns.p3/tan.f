      REAL FUNCTION TAN(X)
      REAL X
      REAL SIN, COS
C    THIS FUNCTION SUBPROGRAM COMPUTES TAN(X) MAKING USE OF THE TWO
C    BUILT-IN SINE AND COSINE FUNCTIONS.  THE FORMULA USED IS
C        TAN(X)  =  SIN(X) / COS(X)
      TAN = SIN(X)/COS(X)
      RETURN
      END
