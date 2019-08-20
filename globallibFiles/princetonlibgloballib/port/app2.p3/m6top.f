      SUBROUTINE M6TOP(M, N, B, ILO, IHI, JLO, JHI, X, IOP, Y, H)
      INTEGER M, N
      INTEGER ILO, IHI, JLO, JHI, IOP
      REAL B(M, N), X(1), Y(1), H(1)
      INTEGER I, J
      REAL TOL, R1MACH, ABS
      IF (IOP .NE. 1) GOTO 6
         DO  1 I = ILO, IHI
C  COMPUTE  Y = CX.
            H(I) = 0.0E0
   1        CONTINUE
         DO  4 J = JLO, JHI
            IF (X(J) .EQ. 0.0E0) GOTO 3
               DO  2 I = ILO, IHI
                  H(I) = H(I)+B(I, J)*X(J)
   2              CONTINUE
   3        CONTINUE
   4        CONTINUE
         DO  5 I = ILO, IHI
            Y(I) = H(I)
   5        CONTINUE
         GOTO  29
   6     IF (IOP .NE. 2) GOTO 12
            DO  7 J = JLO, JHI
C  COMPUTE  Y = C(T)X.
               H(J) = 0.0E0
   7           CONTINUE
            DO  10 I = ILO, IHI
               IF (X(I) .EQ. 0.0E0) GOTO 9
                  DO  8 J = JLO, JHI
                     H(J) = H(J)+B(I, J)*X(I)
   8                 CONTINUE
   9           CONTINUE
  10           CONTINUE
            DO  11 J = JLO, JHI
               Y(J) = H(J)
  11           CONTINUE
            GOTO  28
  12        IF (IOP .NE. 3) GOTO 18
               DO  13 I = ILO, IHI
C  COMPUTE  Y = RX.
                  H(I) = 0.0E0
  13              CONTINUE
               DO  16 J = ILO, IHI
                  IF (X(J) .EQ. 0.0E0) GOTO 15
                     DO  14 I = J, IHI
                        H(I) = H(I)+B(I, J)*X(J)
  14                    CONTINUE
  15              CONTINUE
  16              CONTINUE
               DO  17 I = ILO, IHI
                  Y(I) = H(I)
  17              CONTINUE
               GOTO  27
  18           TOL = 1.0E2*R1MACH(1)
C  SOLVE RY = X.
               DO  19 I = ILO, IHI
                  Y(I) = X(I)
  19              CONTINUE
               I = IHI
                  GOTO  21
  20              I = I-1
  21              IF (I .LT. ILO) GOTO  26
                  J = I+1
                     GOTO  23
  22                 J = J+1
  23                 IF (J .GT. IHI) GOTO  24
                     Y(I) = Y(I)-B(I, J)*Y(J)
                     GOTO  22
  24              IF (ABS(B(I, I)) .GE. TOL) GOTO 25
C/6S
C                    CALL SETERR(24HM5TOP  - SINGULAR SYSTEM, 24, 6, 1)
C/7S
                     CALL SETERR('M5TOP  - SINGULAR SYSTEM', 24, 6, 1)
C/
                     RETURN
  25              Y(I) = Y(I)/B(I, I)
                  GOTO  20
  26           CONTINUE
  27     CONTINUE
  28  CONTINUE
  29  RETURN
      END
