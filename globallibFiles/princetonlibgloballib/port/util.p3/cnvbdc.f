      SUBROUTINE CNVBDC(N,A,B)
C
C     CNVBDC CONVERTS THE N DOUBLE PRECISION ITEMS IN A
C     TO COMPLEX ITEMS PUTTING THE RESULT IN B.
C     A BACKWARDS DO LOOP IS USED.
C
      DOUBLE PRECISION A(1)
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
C       B(1,I) = SNGL(A(I))
C       B(2,I) = A(I)
C/C
        B(I) = CMPLX( SNGL(A(I)), 0.0 )
C/
        I = I - 1
        GO TO 10
C
      END
