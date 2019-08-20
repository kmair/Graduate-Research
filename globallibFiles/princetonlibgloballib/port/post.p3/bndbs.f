      SUBROUTINE BNDBS(N, M, U, NB, B)
      INTEGER M, N, NB
      REAL U(N, M), B(N, NB)
      INTEGER I, J, K, L, MIN0
      REAL X
      INTEGER TEMP
C TO SOLVE U*X = B, WHERE U IS AN UPPER TRIANGULAR BANDED MATRIX.
C MNEMONIC - BANDED BACK-SOLVE.
C INPUT -
C   N  - THE ORDER OF THE SYSTEM.
C   M  - THE NUMBER OF NONZERO ENTRIES ON AND ABOVE
C        THE DIAGONAL OF U.
C   U  - THE UPPER TRIANGULAR BAND MATRIX.
C   NB - THE NUMBER OF RIGHT-HAND-SIDES.
C   B  - THE RIGHT-HAND-SIDES.
C OUTPUT -
C   B - THE SOLUTION VECTORS, X.
C SCRATCH SPACE ALLOCATED - NONE.
C ERROR STATES -
C   1 - N.LT.1.
C   2 - M.LT.1.
C   3 - NB.LT.1.
C   4 - U(I,1)=0.
C CHECK THE INPUT FOR ERRORS.
C/6S
C     IF (N .LT. 1) CALL SETERR(15H BNDBS - N.LT.1, 15, 1, 2)
C     IF (M .LT. 1) CALL SETERR(15H BNDBS - M.LT.1, 15, 2, 2)
C     IF (NB .LT. 1) CALL SETERR(16H BNDBS - NB.LT.1, 16, 3, 2)
C/7S
      IF (N .LT. 1) CALL SETERR(' BNDBS - N.LT.1', 15, 1, 2)
      IF (M .LT. 1) CALL SETERR(' BNDBS - M.LT.1', 15, 2, 2)
      IF (NB .LT. 1) CALL SETERR(' BNDBS - NB.LT.1', 16, 3, 2)
C/
C PROTECT AGAINST AN EXISTING ERROR STATE.
      CALL ENTSRC(I, 0)
      DO  6 J = 1, NB
         L = 1
         I = N
            GOTO  2
   1        I = I-1
   2        IF (I .LT. 1) GOTO  5
            X = B(I, J)
            IF (L .LE. 1) GOTO 4
               DO  3 K = 2, L
                  TEMP = I-1+K
                  X = X-U(I, K)*B(TEMP, J)
   3              CONTINUE
C/6S
C  4        IF (U(I, 1) .EQ. 0.0E0) CALL SETERR(17H BNDBS - U(I,1)=0,
C    1         17, 4, 2)
C/7S
   4        IF (U(I, 1) .EQ. 0.0E0) CALL SETERR(' BNDBS - U(I,1)=0',
     1         17, 4, 2)
C/
            B(I, J) = X/U(I, 1)
            L = MIN0(L+1, M)
            GOTO  1
   5     CONTINUE
   6     CONTINUE
      RETURN
      END
