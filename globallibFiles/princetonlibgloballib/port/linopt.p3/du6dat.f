      SUBROUTINE DU6DAT(IT, POSDEF, N, IT1, D, E, Y, ZL, NN)
      INTEGER NN, N
      INTEGER IT, IT1
      LOGICAL POSDEF
      DOUBLE PRECISION D(N), E(N), Y(N), ZL(NN, N)
      INTEGER I, J, ITM1
      LOGICAL PIV
      DOUBLE PRECISION TEMP, C, F
      LOGICAL TEMP1
C
C THIS SUBROUTINE IS CALLED BY BOTH DQ6DRP AND DQ6ADD WHEN TRYING
C TO MAKE THE REPRESENTATION OF THE NULL SPACE CONJUGATE TO
C THE HESSIAN OF THE FUNCTION TO BE MINIMIZED
C
      E(N) = 0.D0
      ITM1 = IT-1
      I = N
   1  IF (I .EQ. IT-1) GOTO  15
         IF (I .EQ. IT) GOTO 11
            IF (E(I-2) .EQ. 0.D0) GOTO 10
               CALL DE6LIM(N, Y(I-1), Y(I-2), C, I-1, I-2, E(I-2),
     1            PIV, D, E, ZL, NN)
               CALL DE6LIM(N, Y(I), Y(I-2), C, I, I-2, F, PIV, D, E, ZL,
     1            NN)
               IF (E(I) .EQ. 0.D0) GOTO 5
                  IF (.NOT. PIV) GOTO 3
                     TEMP = D(I+1)
C
C**********************
C
                     D(I+1) = D(I-1)
                     D(I-1) = TEMP
                     TEMP = E(I)
                     E(I) = E(I-2)
                     E(I-2) = TEMP
                     DO  2 J = 1, N
                        TEMP = ZL(J, I-1)
                        ZL(J, I-1) = ZL(J, I+1)
                        ZL(J, I+1) = TEMP
   2                    CONTINUE
   3              E(I-1) = (-C)*E(I-2)
                  CALL DE6LIM(N, F, E(I-2), C, I, I-1, E(I-1), PIV, D, E
     1               , ZL, NN)
                  IF (.NOT. PIV) GOTO 4
                     F = E(I)
                     E(I) = (-C)*F
                     CALL DE6LIM(N, F, E(I-1), C, I+1, I, E(I), PIV,
     1                  D, E, ZL, NN)
   4              CALL DT6DIA(I-2, I+1, N, D, E, ZL, NN)
                  I = I-2
                  GOTO  9
   5              IF (PIV) GOTO 6
                     E(I-1) = (-C)*E(I-2)
                     CALL DE6LIM(N, F, E(I-2), C, I, I-1, E(I-1), PIV, D
     1                  , E, ZL, NN)
                     GOTO  8
   6                 E(I-1) = E(I-2)
                     E(I-2) = F
                     DO  7 J = 1, N
                        TEMP = ZL(J, I)
                        ZL(J, I) = ZL(J, I-1)
                        ZL(J, I-1) = TEMP
   7                    CONTINUE
                     TEMP = D(I)
                     D(I) = D(I-1)
                     D(I-1) = TEMP
   8              CALL DT6DIA(I-2, I, N, D, E, ZL, NN)
                  I = I-2
   9           GOTO  1
  10        CONTINUE
  11     IF (E(I) .EQ. 0.D0) GOTO 13
            CALL DE6LIM(N, Y(I), Y(I-1), C, I, I-1, E(I-1), PIV,
     1         D, E, ZL, NN)
C
C**********************
C
            IF (.NOT. PIV) GOTO 12
               F = E(I)
               E(I) = (-C)*F
               CALL DE6LIM(N, F, E(I-1), C, I+1, I, E(I), PIV, D, E, ZL,
     1            NN)
  12        CALL DT6DIA(I-1, I+1, N, D, E, ZL, NN)
            I = I-1
            GOTO  14
  13        CALL DE6LIM(N, Y(I), Y(I-1), C, I, I-1, E(I-1), PIV,
     1         D, E, ZL, NN)
            IF (I .GT. IT1) CALL DT6DIA(I-1, I, N, D, E, ZL, NN)
            I = I-1
  14     CONTINUE
         GOTO  1
  15  POSDEF = .TRUE.
      DO  16 I = IT1, N
         TEMP1 = D(I) .LE. 0.D0
         IF (.NOT. TEMP1) TEMP1 = E(I) .NE. 0.D0
         IF (TEMP1) POSDEF = .FALSE.
  16     CONTINUE
      RETURN
      END
