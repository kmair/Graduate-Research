      SUBROUTINE DD6SIM(A, IA, N, Q, IQQ, IS, IT, JT, POSDEF,
     1   JSIM, IAC, D, E, Y, ZL, IWUNIT)
      INTEGER IA, N, IQQ
      INTEGER IS, IT, JT, JSIM(N), IAC(N)
      LOGICAL POSDEF
      DOUBLE PRECISION A(IA, N), Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)
      INTEGER IDEX, JDEX, I, JTM1, ITP1
      INTEGER IWUNIT
      DOUBLE PRECISION V1, V2, U2
C
C THIS SUBROUTINE FIXES UP THE D,Z,Q,AND L MATRICES
C WHEN A SIMPLE CONSTRAINT IS DELETED FROM THE SET OF
C ACTIVE CONSTRAINTS
C
      IS = -IS
      ITP1 = IT+1
      IDEX = JSIM(IS)
      JT = JT+1
      ZL(JT, JT) = 0.D0
C SINCE THE NEW COLUMN WILL BE THE ITTH POSITION SOME SWAPPING
C MUST BE DONE
      D(JT) = D(ITP1)
      IF (JT .LE. 1) GOTO 2
         JTM1 = JT-1
         DO  1 I = 1, JTM1
            ZL(I, JT) = ZL(I, ITP1)
            ZL(I, ITP1) = 0.D0
            ZL(JT, I) = 0.D0
   1        CONTINUE
   2  ZL(JT, ITP1) = 1.D0
      IF (IT .LE. 0) GOTO 5
         DO  3 I = 1, IT
            JDEX = IAC(I)
            Y(I) = A(JDEX, IDEX)
   3        CONTINUE
         DO  4 I = 1, IT
            CALL DG6TH2(D(I), Y(I), V1, V2, U2, D(I))
            CALL DA6PH2(JT, ZL(1, I), ZL(1, ITP1), 1, V1, V2, U2)
            IF (I .NE. IT) CALL DA6PH2(IT-I, Q(I+1, I), Y(I+1), 1, V1,
     1         V2, U2)
   4        CONTINUE
   5  JSIM(IS) = JSIM(JT)
      JSIM(JT) = IDEX
      CALL DM6CON(Q, IQQ, ITP1, JT, POSDEF, JSIM, D, E, Y, ZL
     1   , N, IWUNIT)
      RETURN
      END
