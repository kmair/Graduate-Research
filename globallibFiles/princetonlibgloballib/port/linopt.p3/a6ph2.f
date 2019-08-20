      SUBROUTINE A6PH2(N, A, B, IDIM, V1, V2, U2)
      INTEGER IDIM
      INTEGER N
      REAL A(IDIM, 1), B(IDIM, 1), V1, V2, U2
      INTEGER I
      REAL T
C
C
C THIS SUBROUTINE APPLIES A 2 X 2 HOUSEHOLDER TO THE
C TWO COLUMNS A AND B
C
      IF (N .LT. 1) RETURN
      DO  1 I = 1, N
         T = A(1, I)+U2*B(1, I)
         A(1, I) = A(1, I)+T*V1
         B(1, I) = B(1, I)+T*V2
   1     CONTINUE
      RETURN
      END
