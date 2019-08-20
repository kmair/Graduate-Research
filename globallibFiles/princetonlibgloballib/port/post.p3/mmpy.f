      SUBROUTINE MMPY(A, MA, NA, B, NB, C)
      INTEGER MA, NA, NB
      REAL A(MA, NA), B(NA, NB), C(MA, NB)
      INTEGER I, J, K
      REAL T
      LOGICAL TEMP
C  TO MULTIPLY THE MATRICES A*B AND PUT THE RESULT IN C.
C  INPUT -
C    A  - THE MATRIX A
C    MA - IS MA BY
C    NA - NA.
C    B  - THE MATRIX B
C    NB - IS NA BY NB.
C  OUTPUT -
C    C - C=A*B IS MA BY NB.
C  SCRATCH SPACE ALLOCATED - NONE.
C  ERROR STATES -
C    1 - BAD DIMENSIONS FOR A AND/OR B.
      TEMP = MA .LT. 1
      IF (.NOT. TEMP) TEMP = NA .LT. 1
      IF (.NOT. TEMP) TEMP = NB .LT. 1
C/6S
C     IF (TEMP) CALL SETERR(37H MMPY - BAD DIMENSIONS FOR A AND/OR B,
C    1   37, 1, 2)
C/7S
      IF (TEMP) CALL SETERR(' MMPY - BAD DIMENSIONS FOR A AND/OR B',
     1   37, 1, 2)
C/
      DO  3 I = 1, MA
         DO  2 J = 1, NB
            T = 0
            DO  1 K = 1, NA
               T = T+A(I, K)*B(K, J)
   1           CONTINUE
            C(I, J) = T
   2        CONTINUE
   3     CONTINUE
      RETURN
      END
