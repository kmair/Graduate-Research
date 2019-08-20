      SUBROUTINE DQ6DRP(Q, IQQ, IS, IT, IP, N, POSDEF, JT, IAC,
     1   JSIM, D, E, Y, ZL, IWUNIT)
      INTEGER N, IQQ
      INTEGER IS, IT, IP, JT, IAC(N), JSIM(N)
      LOGICAL POSDEF
      DOUBLE PRECISION Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)
      INTEGER IP1, IP2, I, J, ITE, ISM1
      INTEGER ITM1, ISP1
      INTEGER IWUNIT
      DOUBLE PRECISION T, V1, V2, U2
C
C THIS SUBROUTINE CHANGES THE D,E, AND ZL MATRIX WHEN CONSTRAINT
C IS IS ROPPED FROM THE SET OF ACTIVE CONSTRAINTS TO MAKE
C IT-1 ACTIVE CONSTRAINTS
C
      ISP1 = IS+1
      ITM1 = IT-1
      ITE = IAC(IS)
C
C UPDATES L BY SHOVING UP ROWS
C
      IF (IS .EQ. IT) GOTO 7
         ISM1 = IS-1
         IF (IS .EQ. 1) GOTO 3
            DO  2 J = 1, ISM1
               DO  1 I = ISP1, IT
                  Q(I-1, J) = Q(I, J)
   1              CONTINUE
   2           CONTINUE
   3     DO  6 I = IS, ITM1
            IP1 = I+1
            IAC(I) = IAC(IP1)
            CALL DG6TH2(Q(IP1, I), D(IP1), V1, V2, U2, D(I))
            CALL DA6PH2(JT, ZL(1, I), ZL(1, IP1), 1, V1, V2, U2)
            IF (I .EQ. ITM1) GOTO 5
               IP2 = I+2
               DO  4 J = IP2, IT
                  T = Q(J, I)+U2*Q(J, IP1)
                  Q(J-1, I) = Q(J, I)+T*V1
                  Q(J, IP1) = Q(J, IP1)+T*V2
   4              CONTINUE
   5        CONTINUE
   6        CONTINUE
   7  CALL DM6CON(Q, IQQ, IT, JT, POSDEF, JSIM, D, E, Y, ZL, N, IWUNIT)
      IAC(IT) = ITE
      IT = IT-1
      RETURN
      END
