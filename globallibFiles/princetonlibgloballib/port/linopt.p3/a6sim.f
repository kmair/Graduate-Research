      SUBROUTINE A6SIM(Q, IQQ, IT, JT, IQ, POSDEF, JSIM, D, E, Y,
     1   ZL, N)
      INTEGER N, IQQ
      INTEGER IT, JT, IQ, JSIM(N)
      LOGICAL POSDEF
      REAL Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)
      INTEGER IB, I, J, K, ITE, JTM1
      INTEGER ITP1
      REAL DUM, V1, V2, U2
C THIS SUBROUTINES FIXES UP THE D,Z,Q, AND L MATRICES
C WHEN A SIMPLE CONSTRAINT IS ACTIVATED.
C
      IQ = -IQ
      ITP1 = IT+1
C ZERO OUT THE BOTTOM OF THE VECTOR
      IF (ITP1 .GE. JT) GOTO 2
         DO  1 I = ITP1, JT
            Y(I) = ZL(IQ, I)
   1        CONTINUE
         CALL U6DAT(IT+2, POSDEF, JT, IT+2, D, E, Y, ZL, N)
C UPDATE R
   2  IF (IT .LE. 0) GOTO 4
         DUM = ZL(IQ, ITP1)
         DO  3 IB = 1, IT
            I = ITP1-IB
            Y(I) = 0.E0
            CALL G6TH2(DUM, ZL(IQ, I), V1, V2, U2, DUM)
            CALL A6PH2(JT, ZL(1, ITP1), ZL(1, I), 1, V1, V2, U2)
            CALL A6PH2(1, Y(I), D(I), 1, V1, V2, U2)
            IF (IB .GT. 1) CALL A6PH2(IB-1, Y(I+1), Q(I+1, I), 1, V1,
     1         V2, U2)
   3        CONTINUE
         ZL(IQ, ITP1) = DUM
   4  JTM1 = JT-1
C SHOVE THINGS OVER
      IF (ITP1 .GE. JT) GOTO 10
   6        DO  8 K = ITP1, JTM1
               DO  7 I = 1, JT
                  ZL(I, K) = ZL(I, K+1)
   7              CONTINUE
               D(K) = D(K+1)
               E(K) = E(K+1)
   8           CONTINUE
   9     CONTINUE
  10  IF (IQ .EQ. JT) GOTO 12
         DO  11 J = 1, JT
            ZL(IQ, J) = ZL(JT, J)
  11        CONTINUE
         ITE = JSIM(IQ)
         JSIM(IQ) = JSIM(JT)
         JSIM(JT) = ITE
  12  JT = JT-1
      RETURN
      END
