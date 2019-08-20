      SUBROUTINE RCSBC(A, M, N, R, C, RC)
      INTEGER M, N
      INTEGER RC(M)
      REAL A(M, N), R(M), C(M)
      INTEGER I, J, N2, RD2, MIN0, MAX0
      REAL R1MACH, L, S, D1, D2, AIJ
      REAL ABS, AAIJ, AMAX1
      INTEGER TEMP, TEMP1, TEMP2
C TO GET THE COLUMN SCALE FACTOR FOR (1/R)*A.
      CALL ENTER(1)
C/6S
C     IF (M .LT. 1) CALL SETERR(17H RCSBC - M .LT. 1, 17, 1, 2)
C     IF (N .LT. 1) CALL SETERR(17H RCSBC - N .LT. 1, 17, 2, 2)
C/7S
      IF (M .LT. 1) CALL SETERR(' RCSBC - M .LT. 1', 17, 1, 2)
      IF (N .LT. 1) CALL SETERR(' RCSBC - N .LT. 1', 17, 2, 2)
C/
      S = R1MACH(1)
      L = R1MACH(2)
      DO  1 I = 1, M
         IF (R(I) .EQ. 0.) GOTO  1
C/6S
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(
C    1      37H RCSBC - MUST HAVE S .LE. R(I) .LE. L, 37, 3, 2)
C/7S
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(
     1      ' RCSBC - MUST HAVE S .LE. R(I) .LE. L', 37, 3, 2)
C/
   1     CONTINUE
      N2 = (N+1)/2
      DO  17 J = 1, M
         D2 = 0
C -1 = UNDERFLOW, 0 = IN-RANGE, +1 = OVERFLOW.
         RD2 = -1
         TEMP1 = MAX0(1, J+N2-M)
         TEMP = MIN0(N, J+N2-1)
         DO  16 I = TEMP1, TEMP
            TEMP2 = J+N2-I
            AIJ = A(TEMP2, I)
            AAIJ = ABS(AIJ)
            TEMP2 = J+N2-I
            D1 = R(TEMP2)
            IF (AIJ .EQ. 0. .OR. D1 .EQ. 0.) GOTO  16
            IF (D1 .GE. 1.) GOTO 8
               IF (AAIJ .LE. D1*L) GOTO 2
                  IF (RD2 .LT. 1) D2 = 0
C CHECK FOR OVERFLOW.
C OVERFLOW.
                  RD2 = 1
                  D2 = AMAX1(D2, AAIJ*(S/D1))
                  GOTO  7
   2              IF (RD2 .LE. 0) GOTO 3
                     GOTO  16
C THIS ELEMENT IS IN-RANGE.
C ALREADY OVERFLOWED, NO EFFECT.
   3                 IF (RD2 .NE. 0) GOTO 4
                        D2 = AMAX1(D2, AAIJ/D1)
                        GOTO  5
   4                    RD2 = 0
C RD2 = -1.
                        D2 = AAIJ/D1
   5              CONTINUE
   6              CONTINUE
   7           CONTINUE
               GOTO  15
   8           IF (AAIJ .GE. D1*S) GOTO 9
                  IF (RD2 .GE. 0) GOTO  16
C ELEMENT UNDERFLOW, D1 >= 1.
C NO-EFFECT.
                  D2 = AMAX1(D2, AAIJ*(L/D1))
                  GOTO  14
   9              IF (RD2 .LE. 0) GOTO 10
                     GOTO  16
C IN-RANGE.
C NO-EFFECT.
  10                 IF (RD2 .NE. 0) GOTO 11
                        D2 = AMAX1(D2, AAIJ/D1)
                        GOTO  12
  11                    RD2 = 0
C UNDERFLOWED SO FAR.
                        D2 = AAIJ/D1
  12              CONTINUE
  13              CONTINUE
  14        CONTINUE
  15        CONTINUE
  16        CONTINUE
         C(J) = D2
         RC(J) = RD2
  17     CONTINUE
      CALL LEAVE
      RETURN
      END
