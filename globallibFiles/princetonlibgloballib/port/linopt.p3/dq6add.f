      SUBROUTINE DQ6ADD(A, IQ, N, IT, POSDEF, IA, Q, IQQ, JT, JSIM,
     1   IAC, D, E, Y, ZL)
      INTEGER IA, N, IQQ
      INTEGER IQ, IT, JT, JSIM(N), IAC(IA)
      LOGICAL POSDEF
      DOUBLE PRECISION A(IA, N), Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)
      INTEGER JDEX, I, J, IAQ, ITP1
      DOUBLE PRECISION ACUM
C
C
C THIS SUBROUTINE UPDATES L AND Z CONTAINED IN THE ZL MATRIX
C WHEN ROW IQ OF THE A MATRIX BECOMES THE IT+1ST ROW OF
C THAT MATRIX SO THAT NOW THERE ARE IT+1 ACTIVE CONSTRAINTS.
C NOTE THAT L THE LOWER TRIANGULAR FACTOR OF THE ACTIVE
C CONSTRAINT MATRIX WILL GAIN A ROW AND Z,WHICH SPANS THE
C NULL SPACE OF THE ACTIVE CONSTRAINTS WILL LOSE A COLUMN.
C
      IAQ = IAC(IQ)
C
C APPLY Q TO NEW ROW TO BE ADDED
C
      ITP1 = IT+1
      DO  4 I = 1, JT
         ACUM = 0.D0
         DO  1 J = 1, JT
            JDEX = JSIM(J)
            ACUM = ACUM+ZL(J, I)*A(IAQ, JDEX)
   1        CONTINUE
         IF (I .LE. IT) GOTO 2
            Y(I) = ACUM
            GOTO  3
   2        Q(ITP1, I) = ACUM
   3     CONTINUE
   4     CONTINUE
      IF (IT .LT. JT-1) CALL DU6DAT(IT+2, POSDEF, JT, IT+2, D, E, Y, ZL,
     1   N)
      D(ITP1) = Y(ITP1)
      IF (IQ .EQ. ITP1) GOTO 5
         IAC(IQ) = IAC(ITP1)
         IAC(ITP1) = IAQ
   5  IT = ITP1
      RETURN
      END
