      SUBROUTINE CNVFIC(N,A,B)
C
C     CNVFIC CONVERTS THE N INTEGER ITEMS IN A
C     TO COMPLEX ITEMS PUTTING THE RESULT IN B.
C     A FORWARD DO LOOP IS USED.
C
      INTEGER A(1)
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
C       B(1,I) = FLOAT(A(I))
C10     B(2,I) = 0.0
C/C
 10     B(I) = CMPLX( FLOAT(A(I)), 0.0 )
C/
C
      RETURN
C
      END
