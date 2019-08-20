      SUBROUTINE RCSBS(A, M, N, R, C, RC)
      INTEGER M, N
      INTEGER RC(M)
      REAL A(M, N), R(M), C(M)
      INTEGER I, J, N2, RD2, MIN0, MAX0
      REAL R1MACH, L, S, D1, D2
      LOGICAL BADNGE
      INTEGER TEMP, TEMP1, TEMP2
C TO SCALE ((1/R)*A)*(1/C).
C/6S
C     IF (M .LT. 1) CALL SETERR(17H RCSBS - M .LT. 1, 17, 1, 2)
C     IF (N .LT. 1) CALL SETERR(17H RCSBS - N .LT. 1, 17, 2, 2)
C/7S
      IF (M .LT. 1) CALL SETERR(' RCSBS - M .LT. 1', 17, 1, 2)
      IF (N .LT. 1) CALL SETERR(' RCSBS - N .LT. 1', 17, 2, 2)
C/
      CALL ENTER(1)
      S = R1MACH(1)
      L = R1MACH(2)
      DO  1 I = 1, M
         IF (R(I) .EQ. 0.) GOTO  1
C/6S
C        IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(
C    1      37H RCSBS - MUST HAVE S .LE. R(I) .LE. L, 37, 3, 2)
C/7S
         IF (R(I) .LT. S .OR. R(I) .GT. L) CALL SETERR(
     1      ' RCSBS - MUST HAVE S .LE. R(I) .LE. L', 37, 3, 2)
C/
   1     CONTINUE
      DO  2 I = 1, M
         IF (C(I) .EQ. 0.) GOTO  2
C/6S
C        IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(
C    1      37H RCSBS - MUST HAVE S .LE. C(I) .LE. L, 37, 4, 2)
C        IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(
C    1      36H RCSS - MUST HAVE RC(I) IN (-1,0,+1), 36, 5, 2)
C/7S
         IF (C(I) .LT. S .OR. C(I) .GT. L) CALL SETERR(
     1      ' RCSBS - MUST HAVE S .LE. C(I) .LE. L', 37, 4, 2)
         IF (RC(I) .LT. (-1) .OR. RC(I) .GT. 1) CALL SETERR(
     1      ' RCSS - MUST HAVE RC(I) IN (-1,0,+1)', 36, 5, 2)
C/
   2     CONTINUE
C CHECK 1/(S*L) RANGE.
      BADNGE = .FALSE.
      IF (S*L .GT. 1.) GOTO 3
         IF (1./L .GT. S*L) BADNGE = .TRUE.
         GOTO  4
   3     IF (S*L .GT. 1./S) BADNGE = .TRUE.
C S*L > 1.
C/6S
C  4  IF (BADNGE) CALL SETERR(
C    1   43H RCSBX - MUST HAVE 1/(S*L) IN MACHINE RANGE, 43, 6, 1)
C/7S
   4  IF (BADNGE) CALL SETERR(
     1   ' RCSBX - MUST HAVE 1/(S*L) IN MACHINE RANGE', 43, 6, 1)
C/
      N2 = (N+1)/2
      DO  32 I = 1, M
         D1 = R(I)
         IF (D1 .EQ. 0.) GOTO  32
         TEMP1 = MAX0(1, N2+1-I)
         TEMP = MIN0(N, M+N2-I)
         DO  31 J = TEMP1, TEMP
            TEMP2 = I+J-N2
            D2 = C(TEMP2)
            TEMP2 = I+J-N2
            RD2 = RC(TEMP2)
            IF (A(I, J) .NE. 0. .AND. D2 .NE. 0.) GOTO 5
               GOTO  31
   5           IF (D1 .LT. 1.) GOTO 18
                  IF (RD2 .LE. 0) GOTO 10
                     IF (D2 .LT. 1.) GOTO 6
                        A(I, J) = S*((A(I, J)/D1)/D2)
C D2 OVERFLOWED.
                        GOTO  9
   6                    IF (D1*D2 .LT. 1.) GOTO 7
                           A(I, J) = S*(A(I, J)/(D1*D2))
C D2 < 1.
                           GOTO  8
   7                       A(I, J) = A(I, J)*(S/(D1*D2))
   8                    CONTINUE
   9                 CONTINUE
                     GOTO  17
  10                 IF (D2 .LT. 1.) GOTO 11
                        A(I, J) = (A(I, J)/D1)/D2
                        GOTO  16
  11                    IF (RD2 .GE. 0) GOTO 14
                           IF (D2 .LT. 1./D1) GOTO 12
                              A(I, J) = A(I, J)*((L/D1)/D2)
C D2 UNDERFLOWED.
                              GOTO  13
  12                          A(I, J) = L*(A(I, J)/(D1*D2))
  13                       CONTINUE
                           GOTO  15
  14                       A(I, J) = A(I, J)/(D1*D2)
C D2 < 1.
  15                 CONTINUE
  16              CONTINUE
  17              CONTINUE
                  GOTO  29
  18              IF (RD2 .LE. 0) GOTO 21
                     IF (D1*D2 .LT. 1.) GOTO 19
                        A(I, J) = S*(A(I, J)/(D1*D2))
C D1 < 1.
C D2 OVERFLOWED.
                        GOTO  20
  19                    A(I, J) = A(I, J)*((S/D1)/D2)
  20                 CONTINUE
                     GOTO  28
  21                 IF (D2 .LT. 1.) GOTO 22
                        A(I, J) = A(I, J)/(D1*D2)
                        GOTO  27
  22                    IF (RD2 .GE. 0) GOTO 25
                           IF (D1*D2 .GT. 1.) GOTO 23
                              A(I, J) = L*(A(I, J)/(D1*D2))
C D2 UNDERFLOWED.
                              GOTO  24
  23                          A(I, J) = A(I, J)*(L/(D1*D2))
  24                       CONTINUE
                           GOTO  26
  25                       A(I, J) = (A(I, J)/D1)/D2
C D2 < 1.
  26                 CONTINUE
  27              CONTINUE
  28              CONTINUE
  29        CONTINUE
  30        CONTINUE
  31        CONTINUE
  32     CONTINUE
      CALL LEAVE
      RETURN
      END
