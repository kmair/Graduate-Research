      SUBROUTINE CNVFDR(N,A,B)
C
C     CNVFDR CONVERTS THE N DOUBLE PRECISION ITEMS IN A
C     TO REAL ITEMS PUTTING THE RESULT IN B.
C     A FOWARDS DO LOOP IS USED.
C
      DOUBLE PRECISION A(1)
      REAL B(1)
C
      IF(N .LE. 0) RETURN
C
      DO 10 I = 1, N
 10     B(I) = A(I)
C
      RETURN
C
      END
