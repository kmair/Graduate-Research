      SUBROUTINE D0DESU(X,NX,GLBMAX)
C
C  TO RAISE THE GLOBAL MAXIMUM.
C
      DOUBLE PRECISION X(NX)
      REAL GLBMAX(NX)
C
      DO 10 I=1,NX
 10      GLBMAX(I)=AMAX1(GLBMAX(I),ABS(SNGL(X(I))))
C
      RETURN
C
      END
