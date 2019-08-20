      SUBROUTINE T6DIA(J, K, N, D, E, ZL, NN)
      INTEGER NN, N
      INTEGER J, K
      REAL D(N), E(N), ZL(NN, N)
      INTEGER I, L
      REAL ABS, AMAX1, C, DEN, ALPHA, SIGMA
      REAL C1, C2
C
C THIS SUBROUTINE TAKES A SYMMETRIC TRIDIAGONAL MATRIX WHOSE
C DIAGONAL ELEMENTS ARE STORED IN ELEMENTS J THROUGH K OF THE
C VECTOR D AND WHOSE OFF DIAGONAL ELEMENTS ARE STORED IN
C ELEMENTS J THROUGH K-1 AND REDUCES IT BY CONGRUENCE
C TRANSFORMATIONS WITHOUT PIVOTING TO BLOCK DIAGONAL FORM
C WHERE THE BLOCKS ARE OF ORDER 1 AND 2.  THE RIGHT
C TRANSFORMATIONS ARE ALSO APPLIED TO THE CORRESPONDING COLUMNS
C OF THE Z MATRIX.;
C
      I = K
   1     SIGMA = AMAX1(ABS(E(I-1)), ABS(D(I-1)))
         ALPHA = .5
         IF (I-1 .GT. J) SIGMA = AMAX1(SIGMA, ABS(E(I-2)))
         IF (SIGMA*ABS(D(I)) .GE. ALPHA*E(I-1)*E(I-1)) GOTO 3
            IF (I .EQ. J+1) GOTO  7
            DEN = D(I)*D(I-1)/E(I-1)-E(I-1)
            C1 = (-D(I))/E(I-1)*E(I-2)/DEN
            C2 = (-(C1*D(I-1)+E(I-2)))/E(I-1)
            DO  2 L = 1, N
               ZL(L, I-2) = ZL(L, I-2)+C2*ZL(L, I)+C1*ZL(L, I-1)
   2           CONTINUE
            D(I-2) = D(I-2)+C1*E(I-2)
            E(I-2) = 0.E0
            I = I-2
            GOTO  6
   3        IF (E(I-1) .EQ. 0.E0) GOTO 5
               C = E(I-1)/D(I)
               DO  4 L = 1, N
                  ZL(L, I-1) = ZL(L, I-1)-C*ZL(L, I)
   4              CONTINUE
               D(I-1) = D(I-1)-C*E(I-1)
               E(I-1) = 0.E0
C
   5        I = I-1
   6     IF (I .EQ. J) GOTO  7
         GOTO  1
   7  RETURN
      END
