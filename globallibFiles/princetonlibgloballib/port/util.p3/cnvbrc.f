      SUBROUTINE CNVBRC(N,A,B)
C
C     CNVBRC CONVERTS THE N REAL ITEMS IN A
C     TO COMPLEX ITEMS PUTTING THE RESULT IN B.
C     A BACKWARDS DO LOOP IS USED.
C
      REAL A(1)
C/R
C     REAL B(2,N)
C/C
      COMPLEX B(1)
C/
C
      I = N
C
 10   IF(I .LE. 0) RETURN
C/R
C       B(1,I) = A(I)
C       B(2,I) = 0.0
C/C
        B(I) = CMPLX( A(I), 0.0 )
C/
        I = I - 1
        GO TO 10
C
      END
