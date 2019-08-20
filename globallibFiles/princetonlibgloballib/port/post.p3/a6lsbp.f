      SUBROUTINE A6LSBP(I, NU, ORDER, BC, E, MAXORD, CE, R, N)
      INTEGER NU
      INTEGER I, ORDER(NU, NU, 2), BC(1), E(1), MAXORD(NU, 2), CE(NU)
      INTEGER R(NU), N
      INTEGER MOD, MAX0, J, L, NBCS, DM
      INTEGER II, LR
      INTEGER TEMP2
      LOGICAL TEMP, TEMP1
C BC(NU,2,2),E(NU,2,2),
C E(I-1),R(N).
      IF (BC(I) .GE. 0) GOTO 1
         N = 1
         R(N) = 0
         RETURN
C LR = 1 FOR LEFT, LR = 2 FOR RIGHT.
   1  LR = (I-1)/(2*NU)+1
C DM = 1 FOR DIRICHLET, DM = 2 FOR MIXED BOUNDARY CONDITIONS.
      DM = MOD((I-1)/NU, 2)+1
      II = MOD(I, NU)
      IF (II .EQ. 0) II = NU
C B(I) = B(II,DM,LR).
      N = 0
      DO  2 J = 1, NU
         CE(J) = J
   2     CONTINUE
C CE = COMPLEMENT OF E.
      IF (I .GT. 2*NU) GOTO 7
         J = 1
            GOTO  4
   3        J = J+1
   4        IF (J .GE. I) GOTO  6
            IF (BC(J) .LT. 0) GOTO 5
               TEMP2 = E(J)
               CE(TEMP2) = 0
   5        CONTINUE
            GOTO  3
   6     CONTINUE
         GOTO  12
   7     J = 2*NU+1
            GOTO  9
   8        J = J+1
   9        IF (J .GE. I) GOTO  11
            IF (BC(J) .LT. 0) GOTO 10
               TEMP2 = E(J)
               CE(TEMP2) = 0
  10        CONTINUE
            GOTO  8
  11     CONTINUE
  12  DO  19 J = 1, NU
         IF (CE(J) .EQ. 0) GOTO  19
         NBCS = 0
         L = 1
            GOTO  14
  13        L = L+1
  14        IF (L .GE. I) GOTO  15
            TEMP = E(L) .EQ. J
            IF (TEMP) TEMP = BC(L) .GE. 0
            IF (TEMP) NBCS = NBCS+1
            GOTO  13
  15     TEMP = DM .EQ. 1
         IF (TEMP) TEMP = MAXORD(J, LR) .GT. BC(I)
         IF (TEMP) GOTO 16
            TEMP1 = DM .EQ. 2
            IF (TEMP1) TEMP1 = ORDER(J, II, LR) .GT. BC(I)
            TEMP = TEMP1
  16     IF (.NOT. TEMP) GOTO 18
            IF (NBCS .GE. MAX0(MAXORD(J, 1), MAXORD(J, 2))) GOTO 17
               N = N+1
               R(N) = J
  17        CONTINUE
  18     CONTINUE
  19     CONTINUE
      RETURN
      END
