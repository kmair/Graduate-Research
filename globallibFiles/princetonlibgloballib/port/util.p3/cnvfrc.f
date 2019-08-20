      SUBROUTINE CNVFRC(N,A,B)
C
C     CNVFRC CONVERTS THE N REAL ITEMS IN A
C     TO COMPLEX ITEMS PUTTING THE RESULT IN B.
C     A FOWARDS DO LOOP IS USED.
C
      REAL A(1)
C/R
C     REAL B(2,N)
C/C
      COMPLEX B(1)
C/
C
      IF(N .LE. 0) RETURN
C
      DO 10 I = 1, N
C/R
C       B(1,I) = A(I)
C10     B(2,I) = 0.0
C/C
 10     B(I) = CMPLX( A(I), 0.0 )
C/
C
      RETURN
C
      END
