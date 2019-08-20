      SUBROUTINE DBNDLU(N, ML, M, G, L, INT)
      INTEGER M, N, ML
      INTEGER INT(N)
      DOUBLE PRECISION G(N, M), L(N, ML)
      INTEGER I, J, K, M1, M2, LL
      INTEGER MIN0
      REAL FLOAT
      LOGICAL SING
      DOUBLE PRECISION D1MACH, X, EPS, DABS, NORM, DMAX1
      INTEGER TEMP, TEMP1
C TO OBTAIN THE LU DECOMPOSITION OF A BANDED MATRIX,
C USING GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING.
C MNEMONIC - DOUBLE PRECISION BAND LU DECOMPOSITION.
C INPUT -
C   N   - THE ORDER OF THE MATRIX.
C   ML  - THE NUMBER OF NONZERO ELEMENTS OF A ON AND BELOW THE DIAGONAL.
C   M   - THE NUMBER OF NONZERO ELEMENTS IN EACH ROW OF A.
C   G   - THE MATRIX A, WITH G(I,J) = A(I,I+J-ML).
C OUTPUT -
C   L   - THE LOWER TRIANGULAR BANDED FACTOR OF A.
C   G   - THE UPPER TRIANGULAR BANDED FACTOR OF A.
C   INT - THE ROW PIVOTING USED.
C SCRATCH STORAGE ALLOCATED - NONE.
C ERROR STATES -
C   1 - N.LT.1.
C   2 - ML.LT.1.
C   3 - M.LT.ML.
C   4 - SINGULAR MATRIX. (RECOVERABLE)
C L(N,ML-1).
C CHECK THE INPUT FOR ERRORS.
C/6S
C     IF (N .LT. 1) CALL SETERR(15HDBNDLU - N.LT.1, 15, 1, 2)
C     IF (ML .LT. 1) CALL SETERR(16HDBNDLU - ML.LT.1, 16, 2, 2)
C     IF (M .LT. ML) CALL SETERR(16HDBNDLU - M.LT.ML, 16, 3, 2)
C/7S
      IF (N .LT. 1) CALL SETERR('DBNDLU - N.LT.1', 15, 1, 2)
      IF (ML .LT. 1) CALL SETERR('DBNDLU - ML.LT.1', 16, 2, 2)
      IF (M .LT. ML) CALL SETERR('DBNDLU - M.LT.ML', 16, 3, 2)
C/
C PROTECT AGAINST AN EXISTING ERROR STATE.
      CALL ENTSRC(I, 0)
      SING = .FALSE.
      EPS = D1MACH(4)*FLOAT(N*(M-1)*(ML-1))
      M1 = ML-1
      M2 = M-ML
      LL = M1
      I = 1
         GOTO  2
   1     I = I+1
   2     IF (I .GT. MIN0(M1, N)) GOTO  5
C SET TO 0 THOSE ELEMENTS
C OF G WHICH ARE UNDEFINED.
         TEMP = ML+1-I
         DO  3 J = TEMP, M
            TEMP1 = J-LL
            G(I, TEMP1) = G(I, J)
   3        CONTINUE
         LL = LL-1
         TEMP = M-LL
         DO  4 J = TEMP, M
            G(I, J) = 0.0D0
   4        CONTINUE
         GOTO  1
   5  I = 1
         GOTO  7
   6     I = I+1
   7     IF (I .GT. MIN0(M2, N)) GOTO  9
C ZERO OUT LOWER RHS WART.
         TEMP = ML+I
         DO  8 J = TEMP, M
            TEMP1 = N+1-I
            G(TEMP1, J) = 0.0D0
   8        CONTINUE
         GOTO  6
C GET || A || SUB INFINITY.
   9  NORM = 0.0D0
      DO  11 I = 1, N
         INT(I) = I
         X = 0.0D0
         DO  10 J = 1, M
            X = X+DABS(G(I, J))
  10        CONTINUE
         NORM = DMAX1(NORM, X)
  11     CONTINUE
      DO  20 K = 1, N
         X = G(K, 1)
         I = K
         LL = MIN0(M1+K, N)
         IF (K .GE. LL) GOTO 14
            TEMP = K+1
C GET THE PIVOT ROW.
            DO  13 J = TEMP, LL
               IF (DABS(G(J, 1)) .LE. DABS(X)) GOTO 12
                  X = G(J, 1)
                  I = J
  12           CONTINUE
  13           CONTINUE
  14     INT(K) = I
         IF (DABS(X) .GT. NORM*EPS) GOTO 15
            SING = .TRUE.
            G(K, 1) = NORM*EPS
  15     IF (ML .EQ. 1 .OR. K .EQ. N) GOTO  20
         IF (I .EQ. K) GOTO 17
            DO  16 J = 1, M
C NEED TO INTERCHANGE THE ROWS.
               X = G(K, J)
               G(K, J) = G(I, J)
               G(I, J) = X
  16           CONTINUE
  17     IF (K .GE. LL) GOTO  20
         TEMP = K+1
         DO  19 I = TEMP, LL
            X = G(I, 1)/G(K, 1)
            TEMP1 = I-K
            L(K, TEMP1) = X
            DO  18 J = 2, M
               G(I, J-1) = G(I, J)-X*G(K, J)
  18           CONTINUE
            G(I, M) = 0.0D0
  19        CONTINUE
  20     CONTINUE
C/6S
C     IF (SING) CALL SETERR(24HDBNDLU - SINGULAR MATRIX, 24, 4, 1)
C/7S
      IF (SING) CALL SETERR('DBNDLU - SINGULAR MATRIX', 24, 4, 1)
C/
      RETURN
      END
