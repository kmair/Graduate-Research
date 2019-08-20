      SUBROUTINE BNDFE(N, ML, L, INT, NB, B)
      INTEGER N, NB, ML
      INTEGER INT(N)
      REAL L(N, ML), B(N, NB)
      INTEGER I, J, K, M1, MIN0
      REAL X
      INTEGER TEMP, TEMP1, TEMP2
C TO SOLVE L*X = B, WHERE L IS A LOWER TRIANGULAR BANDED MATRIX.
C MNEMONIC - BANDED FORWARD-ELIMINATION.
C INPUT -
C   N   - THE ORDER OF THE SYSTEM.
C   ML  - THE NUMBER OF NONZERO ELEMENTS OF L ON AND BELOW THE DIAGONAL.
C   L   - THE LOWER TRIANGULAR BANDED MATRIX.
C   INT - THE ORDERING OF THE ROWS OF THE SYSTEM, DUE TO PIVOTING.
C   NB  - THE NUMBER OF RIGHT-HAND-SIDES.
C   B   - THE RIGHT-HAND-SIDES.
C OUTPUT -
C   B - THE SOLUTION VECTORS, X.
C SCRATCH STORAGE ALLOCATED - NONE.
C ERROR STATES -
C   1 - N.LT.1.
C   2 - ML.LT.1.
C   3 - NB.LT.1.
C   4 - INT(I) NOT ONE OF 1,...,N.
C L(N,ML-1).
C CHECK THE INPUT FOR ERRORS.
C/6S
C     IF (N .LT. 1) CALL SETERR(15H BNDFE - N.LT.1, 15, 1, 2)
C     IF (ML .LT. 1) CALL SETERR(16H BNDFE - ML.LT.1, 16, 2, 2)
C     IF (NB .LT. 1) CALL SETERR(16H BNDFE - NB.LT.1, 16, 3, 2)
C/7S
      IF (N .LT. 1) CALL SETERR(' BNDFE - N.LT.1', 15, 1, 2)
      IF (ML .LT. 1) CALL SETERR(' BNDFE - ML.LT.1', 16, 2, 2)
      IF (NB .LT. 1) CALL SETERR(' BNDFE - NB.LT.1', 16, 3, 2)
C/
C PROTECT AGAINST AN EXISTING ERROR STATE.
      CALL ENTSRC(I, 0)
      M1 = ML-1
      DO  5 K = 1, N
         I = INT(K)
C/6S
C        IF (I .LT. 1 .OR. I .GT. N) CALL SETERR(
C    1      34H BNDFE - INT(I) NOT ONE OF 1,...,N, 34, 4, 2)
C/7S
         IF (I .LT. 1 .OR. I .GT. N) CALL SETERR(
     1      ' BNDFE - INT(I) NOT ONE OF 1,...,N', 34, 4, 2)
C/
         IF (I .EQ. K) GOTO 2
            DO  1 J = 1, NB
C INTERCHANGE THE ELEMENTS OF B.
               X = B(K, J)
               B(K, J) = B(I, J)
               B(I, J) = X
   1           CONTINUE
   2     IF (M1 .EQ. 0 .OR. K .EQ. N) GOTO  6
         TEMP1 = K+1
         TEMP = MIN0(M1+K, N)
         DO  4 I = TEMP1, TEMP
            TEMP2 = I-K
            X = L(K, TEMP2)
            DO  3 J = 1, NB
               B(I, J) = B(I, J)-X*B(K, J)
   3           CONTINUE
   4        CONTINUE
   5     CONTINUE
   6  RETURN
      END