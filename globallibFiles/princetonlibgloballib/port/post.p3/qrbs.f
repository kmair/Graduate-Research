      SUBROUTINE QRBS(M, N, QR, ALFA, PIVOT, NB, B, X)
      INTEGER M, N, NB
      INTEGER PIVOT(N)
      REAL QR(M, N), ALFA(N), B(M, NB), X(N, NB)
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      INTEGER ISTKGT, I, JB, IS(1000), IZ
      REAL SDOT, RS(1000), WS(1000)
      LOGICAL LS(1000)
      INTEGER TEMP, TEMP1
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C TO SOLVE R*X = B.
C MNEMONIC - QR BACKSOLVE.
C INPUT -
C   M     - THE NUMBER OF ROWS IN THE MATRIX.
C   N     - THE NUMBER OF COLUMNS IN THE MATRIX.
C   QR    - THE QR FACTORIZATION, AS DESCRIBED IN  QRD.
C   ALFA  - THE DIAGONAL OF R, AS DESCRIBED IN  QRD.
C   PIVOT - THE PIVOTING VECTOR, AS DESCRIBED IN  QRD.
C   NB    - THE NUMBER OF RIGHT-HAND-SIDES.
C   B     - THE RIGHT-HAND-SIDES.
C OUTPUT -
C   X - THE SOLUTION VECTORS.
C SCRATCH STORAGE ALLOCATED - N*MU WORDS.
C ERROR STATES -
C   1 - N.LT.1.
C   2 - M.LT.N.
C   3 - NB.LT.1.
C   4 - ALFA(I)=0.
C   5 - PIVOT(I) NOT ONE OF 1,...,N.
C THE PORT LIBRARY STACK AND ITS ALIASES.
C DEFINE Z(J) WS(IZ-1+J)
C CHECK THE INPUT FOR ERRORS.
C/6S
C     IF (N .LT. 1) CALL SETERR(14H QRBS - N.LT.1, 14, 1, 2)
C     IF (M .LT. N) CALL SETERR(14H QRBS - M.LT.N, 14, 2, 2)
C     IF (NB .LT. 1) CALL SETERR(15H QRBS - NB.LT.1, 15, 3, 2)
C/7S
      IF (N .LT. 1) CALL SETERR(' QRBS - N.LT.1', 14, 1, 2)
      IF (M .LT. N) CALL SETERR(' QRBS - M.LT.N', 14, 2, 2)
      IF (NB .LT. 1) CALL SETERR(' QRBS - NB.LT.1', 15, 3, 2)
C/
      DO  1 I = 1, N
C/6S
C        IF (ALFA(I) .EQ. 0.) CALL SETERR(17H QRBS - ALFA(I)=0, 17, 4, 2
C    1      )
C        IF (PIVOT(I) .LT. 1 .OR. PIVOT(I) .GT. N) CALL SETERR(
C    1      35H QRBS - PIVOT(I) NOT ONE OF 1,...,N, 35, 5, 2)
C/7S
         IF (ALFA(I) .EQ. 0.) CALL SETERR(' QRBS - ALFA(I)=0', 17, 4, 2
     1      )
         IF (PIVOT(I) .LT. 1 .OR. PIVOT(I) .GT. N) CALL SETERR(
     1      ' QRBS - PIVOT(I) NOT ONE OF 1,...,N', 35, 5, 2)
C/
   1     CONTINUE
      IZ = ISTKGT(N, 3)
C DO ALL THE RIGHT-HAND-SIDES.
      DO  6 JB = 1, NB
         TEMP = IZ+N
         WS(TEMP-1) = B(N, JB)/ALFA(N)
         I = N-1
            GOTO  3
   2        I = I-1
   3        IF (I .LT. 1) GOTO  4
            TEMP = IZ-1+I
            TEMP1 = IZ+I
            WS(TEMP) = (-(SDOT(N-I, QR(I, I+1), M, WS(TEMP1), 1)-B(I,
     1         JB)))/ALFA(I)
            GOTO  2
   4     DO  5 I = 1, N
            TEMP1 = PIVOT(I)
            TEMP = IZ+I
            X(TEMP1, JB) = WS(TEMP-1)
   5        CONTINUE
   6     CONTINUE
      CALL ISTKRL(1)
      RETURN
      END
