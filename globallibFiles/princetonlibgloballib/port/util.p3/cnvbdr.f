      SUBROUTINE CNVBDR(N,A,B)
C
C     CNVBDR CONVERTS THE N DOUBLE PRECISION ITEMS IN A
C     TO REAL ITEMS PUTTING THE RESULT IN B.
C     A BACKWARDS DO LOOP IS USED.
C
      DOUBLE PRECISION A(1)
      REAL B(1)
C
      I = N
C
 10   IF(I .LE. 0) RETURN
        B(I) = A(I)
        I = I - 1
        GO TO 10
C
      END
