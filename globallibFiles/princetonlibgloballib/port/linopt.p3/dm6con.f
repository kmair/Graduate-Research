      SUBROUTINE DM6CON(Q, IQQ, IT, JT, POSDEF, JSIM, D, E, Y, ZL,
     1   N, IWUNIT)
      INTEGER JT, N, IQQ
      INTEGER IT, JSIM(N)
      LOGICAL POSDEF
      DOUBLE PRECISION Q(IQQ, JT), D(N), E(N), Y(N), ZL(N, N)
      INTEGER IDEX, JDEX, I, J
      DOUBLE PRECISION DDOT, A, SUM
      INTEGER IWUNIT
C
C THIS SUBROUTINE MAKES THE ITTH COLUMN OF ZL
C CONJUGATE TO THE OTHER COLUMNS
      DO  2 I = 1, JT
         SUM = 0.D0
         IDEX = JSIM(I)
         DO  1 J = 1, JT
            JDEX = JSIM(J)
            A = Q(JDEX, IDEX)
            IF (JDEX .GT. IDEX) A = Q(IDEX, JDEX)
            SUM = SUM+A*ZL(J, IT)
   1        CONTINUE
         E(I) = SUM
   2     CONTINUE
C FORM Z(TRANSPOSE)QZ
      DO  3 I = IT, JT
         Y(I) = DDOT(JT, ZL(1, I), 1, E(1), 1)
   3     CONTINUE
      D(IT) = Y(IT)
      POSDEF = .TRUE.
      DO  4 I = IT, JT
         E(I) = 0.D0
   4     CONTINUE
      IF (IT .GE. JT) GOTO 5
         IF (IT .LT. JT-1) CALL DU6DAT(IT+2, POSDEF, JT, IT+1, D, E, Y
     1      , ZL, N)
         E(IT) = Y(IT+1)
         CALL DT6DIA(IT, IT+1, JT, D, E, ZL, N)
         IF (E(IT) .NE. 0.D0) POSDEF = .FALSE.
   5  IF (D(IT) .LE. 0.D0) POSDEF = .FALSE.
      RETURN
      END
