      SUBROUTINE P6MDC(A, IDIM, N, CHANGE, IT)
      INTEGER IDIM, N
      INTEGER IT
      REAL A(IDIM, N), CHANGE(N)
      INTEGER IP1, IP2, JM1, JP1, I, J
      INTEGER K
      REAL LAMBDA, SAVE, TEMP, AIP1I, AII
      REAL ALPHA, DET, SIGMA, SQRT, AIP1
C
C GIVEN A SYMMETRIC INDEFINITE MATRIX OF ORDER N,THIS SUBROUTINE
C DETERMINES ITS DECOMPOSITION INTO PMDM(TRANSPOSE)P(TRANSPOSE)
C WHERE P IS A PERMUTATION MATRIX,M IS A UNIT LOWER TRIANGULAR
C MATRIX, AND D IS A BLOCK DIAGONAL MATRIX WITH BLOCKS OF ORDER
C 1 OR 2 WHERE D(I+1,I) IS NONZERO WHENEVER M(I+1,I) IS ZERO.
C ONLY THE LOWER TRIANGULAR PORTION OF A IS USED. THE DECOMPOSITION
C IS PLACED IN THE LOWER TRIANGULAR PORTION. THUS IF ALL THE ELEMENTS
C OF A ARE SPECIFIED,THE STRICT UPPER TRIANGLE IS NOT DESTROYED BUT
C THE DIAGONAL IS DESTROYED.  ON OUTPUT THE VECTOR CHANGE OF
C LENGTH N WILL CONTAIN A RECORD OF THE PERMUTATIONS GENERATED.  THE
C INTEGER VARIABLE IDIM GIVES THE ROW DIMENSION O THE A MATRIX.
C
      CHANGE(N) = N
      ALPHA = (SQRT(17.E0)+1.E0)/8.E0
      I = IT
   1  IF (I .GE. N) GOTO  19
         AII = ABS(A(I, I))
         CHANGE(I) = I
C
C FIND THE LARGEST OFF DIAGONAL ELEMENT IN THE ITH COLUMN
C
         J = I+1
         IP1 = I+1
         LAMBDA = ABS(A(IP1, I))
         IP2 = I+2
         IF (IP2 .GT. N) GOTO 4
            DO  3 K = IP2, N
               IF (ABS(A(K, I)) .LE. LAMBDA) GOTO 2
                  LAMBDA = ABS(A(K, I))
                  J = K
   2           CONTINUE
   3           CONTINUE
   4     TEMP = ALPHA*LAMBDA
         IF (AII .GE. TEMP) GOTO 15
            SIGMA = LAMBDA
C
C FIND THE   LARGEST OFFDIAGONAL ELEMENT IN THE JTH COLUMN
C
            JM1 = J-1
            IF (IP1 .GT. JM1) GOTO 6
               DO  5 K = IP1, JM1
                  IF (ABS(A(J, K)) .GT. SIGMA) SIGMA = ABS(A(J, K))
   5              CONTINUE
   6        JP1 = J+1
            IF (JP1 .GT. N) GOTO 8
               DO  7 K = JP1, N
                  IF (ABS(A(K, J)) .GT. SIGMA) SIGMA = ABS(A(K, J))
   7              CONTINUE
   8        IF (AII*SIGMA .GE. TEMP*LAMBDA) GOTO 14
               IF (ABS(A(J, J)) .GE. ALPHA*SIGMA) GOTO 13
                  CHANGE(I) = J
C
C PERFORM A 2 BY 2 PIVOT STEP
C
                  IF (J .EQ. IP1) GOTO 9
                     CALL E6CHG(A, IDIM, N, J, IP1)
                     TEMP = A(J, I)
                     A(J, I) = A(IP1, I)
                     A(IP1, I) = TEMP
   9              DET = A(I, I)*A(IP1, IP1)/A(IP1, I)-A(IP1, I)
                  AIP1I = A(IP1, I)
                  AII = A(I, I)/AIP1I
                  AIP1 = A(IP1, IP1)
                  IF (IP2 .GT. N) GOTO 12
                     DO  11 J = IP2, N
                        TEMP = (A(J, I)-AII*A(J, IP1))/DET
                        SAVE = (-(AIP1*TEMP+A(J, IP1)))/AIP1I
                        DO  10 K = J, N
                           A(K, J) = A(K, J)+A(K, I)*SAVE+A(K, IP1)*
     1                        TEMP
  10                       CONTINUE
                        A(J, I) = SAVE
                        A(J, IP1) = TEMP
  11                    CONTINUE
  12              CHANGE(IP1) = -1
                  I = IP2
                  GOTO  1
C
C INTERCHANGE THE ITH AND JTH ROWS AND COLUMNS
C
  13           CHANGE(I) = J
               CALL E6CHG(A, IDIM, N, J, I)
  14        CONTINUE
C
C PERFORM A 1 X 1 PIVOT
C
  15     IF (A(I, I) .EQ. 0.E0) GOTO 18
            AII = A(I, I)
            DO  17 J = IP1, N
               SAVE = (-A(J, I))/AII
               DO  16 K = J, N
                  A(K, J) = A(K, J)+A(K, I)*SAVE
  16              CONTINUE
               A(J, I) = SAVE
  17           CONTINUE
  18     I = IP1
         GOTO  1
  19  RETURN
      END
