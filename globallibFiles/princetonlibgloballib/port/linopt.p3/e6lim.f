      SUBROUTINE E6LIM(N, A, B, C, I, J, F, PIV, D, E, ZL, NN)
      INTEGER NN, N
      INTEGER I, J
      LOGICAL PIV
      REAL A, B, C, F, D(N), E(N)
      REAL ZL(NN, N)
      INTEGER K
      REAL ABS, H, S
C
C
C
C THIS SUBROUTINE APPLIES ONE STEP OF GAUSSIAN ELIMINATION WITH
C PARTIAL PIVOTING IN THEI AND JTH PLANES TO ELIMINATE THE
C VALUE CONTAINED IN A WHICH IS IN THE I TH PLANE USING
C B IN THE JTH PLANE. THE TRANSFORMATIONS CONSTRUCTED ARE APPLIED
C AS CONGRUENCE TRANSFORMATIONS TO A TRIDIAGONAL MATRIX
C IN VECTORS D AND E AND AS A COLUMN TRANSFORMATION TO THE ZL MATRIX.
C
      H = E(J)
      IF (I .GT. J+1) H = 0
      F = H
      IF (ABS(A) .LE. ABS(B)) GOTO 2
         C = B/A
         F = H-C*D(I)
         S = D(I)
         D(I) = D(J)-C*(H+F)
         D(J) = S
         DO  1 K = 1, N
            S = ZL(K, I)
            ZL(K, I) = ZL(K, J)-C*S
            ZL(K, J) = S
   1        CONTINUE
         B = A
         PIV = .TRUE.
         GOTO  5
   2     C = 0.E0
         IF (A .EQ. 0.E0) GOTO 4
            C = A/B
            F = H-C*D(J)
            D(I) = D(I)-C*(H+F)
            DO  3 K = 1, N
               ZL(K, I) = ZL(K, I)-C*ZL(K, J)
   3           CONTINUE
   4     PIV = .FALSE.
   5  RETURN
      END
