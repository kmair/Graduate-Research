      SUBROUTINE DE6CHG(A, IDIM, N, J, I)
      INTEGER IDIM, N
      INTEGER J, I
      DOUBLE PRECISION A(IDIM, N)
      INTEGER JP1, IP1, JM1, K
      DOUBLE PRECISION TEMP
C
C THIS SUBROUTINE IS CALLED BY DP6MDC TO INTERCHANGE ROWS AND
C COLUMNS I AND J OF A SYMMETRIC MATRIX WHEN ONE IS WORKING ONLY
C WITH THE LOWER TRIANGULAR PORTION OF THAT MATRIX.
C
C
C INTERCHANGE THE ELEMENTS BELOW BOTH DIAGONALS
C
      JP1 = J+1
      IF (JP1 .GT. N) GOTO 2
         DO  1 K = JP1, N
            TEMP = A(K, J)
            A(K, J) = A(K, I)
            A(K, I) = TEMP
   1        CONTINUE
   2  IF (I+1 .GT. J-1) GOTO 4
         IP1 = I+1
         JM1 = J-1
         DO  3 K = IP1, JM1
            TEMP = A(K, I)
            A(K, I) = A(J, K)
            A(J, K) = TEMP
   3        CONTINUE
C
C INTERCHANGE THE DIAGONAL ELEMENTS
C
   4  TEMP = A(I, I)
      A(I, I) = A(J, J)
      A(J, J) = TEMP
      RETURN
      END
