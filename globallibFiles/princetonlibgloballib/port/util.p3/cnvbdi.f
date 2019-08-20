      SUBROUTINE CNVBDI(N,A,B)
C
C     CNVBDI CONVERTS THE N DOUBLE PRECISION ITEMS IN A
C     TO INTEGER ITEMS PUTTING THE RESULT IN B.
C     A BACKWARDS DO LOOP IS USED.
C
      DOUBLE PRECISION A(1)
      INTEGER B(1)
C
      I = N
C
 10   IF(I .LE. 0) RETURN
        B(I) = A(I)
        I = I - 1
        GO TO 10
C
      END