      SUBROUTINE DG6TZD(A, Q, N, IT, IP, POSDEF, IA, IQQ, JT, JSIM,
     1   IAC, D, E, Y, ZL, IE, EPSI)
      INTEGER IA, N, IQQ
      INTEGER IT, IP, JT, JSIM(N), IAC(IA)
      LOGICAL POSDEF
      DOUBLE PRECISION A(IA, N), Q(IQQ, N), D(N), E(N), Y(N), ZL(N, N)
      INTEGER IB, IC, JC, KE, II, JDEX
      INTEGER IZ, IFIX, IM1, IP1, NM1, I
      INTEGER J, K, ITE, ITT, I1, K1
      INTEGER ITP1, NMITM1
      INTEGER IE
      REAL SNGL
      DOUBLE PRECISION TEMP, DDOT, DEL, SIGMA, TAU, EPS
      DOUBLE PRECISION SUM, DSQRT, EPSI
      EPS=EPSI
C
C THIS SUBROUTINE DETERMINES L, THE LOWER TRIANGULAR FACTOR
C OF THE LQ FACTORIZATION OF THE FIRST IT ROWS OF A AND Z,
C A MATRIX WHICH SPANS THE NULL SPACE OF A SUCH THAT Z(TRANSPOSE)QZ
C IS BLOCK DIAGONAL. THE MATRICES LA AND Z ON OUTPUT WILL BE IN THE
C ZL ARRAY.  THE VECTOR D WILL CONTAIN THE DIAGONAL OF THE
C BLOCK DIAGONAL MATRIX AND E THE OFFDIAGONAL ELEMENTS. THE
C VECTOR Y IS JUST A SCRATCH VECTOR.
C
      IZ = N
C
C
C SAVE THE DIAGONAL OF Q SO IT WONT BE DESTROYED
C
      DO  1 I = 1, JT
         D(I) = Q(I, I)
   1     CONTINUE
      POSDEF = .TRUE.
C
      DO  5 I = 1, JT
         IC = JSIM(I)
         DO  4 J = 1, I
            JC = JSIM(J)
            IF (IC .LT. JC) GOTO 2
               Q(I, J) = Q(IC, JC)
               GOTO  3
   2           Q(I, J) = Q(JC, IC)
   3        CONTINUE
   4        CONTINUE
   5     CONTINUE
C
C REDUCE ZL TO TRIANGULAR FORM
C AND APPLY THE TRANSFORMATIONS ON THE RIGHT AND LEFT TO THE
C Q MATRIX
C
      I = 0
      IF (IT .EQ. 0) GOTO 18
         ITT = IT
         DO  17 II = 1, IT
            I = I+1
            IC = IAC(I)
            DO  6 J = 1, JT
               JDEX = JSIM(J)
               ZL(I, J) = A(IC, JDEX)
   6           CONTINUE
C APPLY PREVIOUS TRANSFORMATIONS TO THE NEW ROW
            IF (I .LE. 1) GOTO 9
               IM1 = I-1
               DO  8 J = 1, IM1
                  TAU = DDOT(JT-J+1, ZL(J, J), IZ, ZL(I, J), IZ)/Y(J)
                  DO  7 K = J, JT
                     ZL(I, K) = ZL(I, K)-TAU*ZL(J, K)
   7                 CONTINUE
   8              CONTINUE
   9        SIGMA = DDOT(JT-I+1, ZL(I, I), IZ, ZL(I, I), IZ)
C TEST FOR LINEAR DEPENDENCE
C
            IF (SIGMA .GE. EPS) GOTO 10
               ITE = IAC(II)
               IF (II .GT. IE) GOTO 110
                  IE = IE - 1
                  IP = IP - 1
                  ITE = IAC(IP)
 110           IAC(II) = IAC(ITT)
               IAC(ITT) = ITE
               ITT = ITT-1
               I = I-1
               GOTO  16
  10        SIGMA = DSQRT(SIGMA)
            IF (ZL(I, I) .LT. 0.D0) SIGMA = -SIGMA
            ZL(I, I) = ZL(I, I)+SIGMA
            Y(I) = SIGMA*ZL(I, I)
            IP1 = I+1
C
C
C
C APPLY THE SAME TRANSFORMATION AS A CONGRUENT TRANSFORMATIONS
C TO  THE LOWER TRIANGULAR PART OF THE Q MATRIX
C
            DEL = 0.D0
            DO  11 J = 1, JT
               TAU = 0.D0
               IF (J .GE. I) TAU = DDOT(J-I+1, ZL(I, I), IZ, Q(J, I),
     1            IQQ)
               IF (J .NE. JT) TAU = TAU+DDOT(JT-J, ZL(I, J+1), IZ, Q(J+1
     1            , J), 1)
               E(J) = TAU/Y(I)
               IF (J .GE. I) DEL = DEL+E(J)*ZL(I, J)
  11           CONTINUE
            DEL = DEL/(2.D0*Y(I))
            IM1 = I-1
            DO  15 J = I, JT
               E(J) = E(J)-DEL*ZL(I, J)
               IF (I .EQ. 1) GOTO 13
                  DO  12 K = 1, IM1
                     Q(J, K) = Q(J, K)-ZL(I, J)*E(K)
  12                 CONTINUE
  13           DO  14 K = I, J
                  Q(J, K) = Q(J, K)-ZL(I, J)*E(K)-ZL(I, K)*E(J)
  14              CONTINUE
  15           CONTINUE
            IF (I .EQ. N) GOTO  19
  16        CONTINUE
  17        CONTINUE
  18  CONTINUE
  19  IT = I
C
C NOW REDUCE RELEVANT PART OF Q TO BLOCK DIAGONAL FORM
C
      ITP1 = IT+1
      IF (IT .EQ. JT) GOTO 30
         CALL DP6MDC(Q, IQQ, JT, E, ITP1)
C
C APPLY THE TRANSFORMATIONS USED TO REDUCE Q TO THE LAST JT-IT COLUMNS
C OF THE IDENTITY MATRIX
C
         ZL(JT, JT) = 1.D0
         IF (IT .GE. JT-1) GOTO 29
            NM1 = JT-1
            Y(JT) = JT
            DO  20 J = ITP1, NM1
               ZL(JT, J) = 0.D0
  20           CONTINUE
            NMITM1 = JT-IT-1
            DO  28 IB = 1, NMITM1
               I = JT-IB
               ZL(I, I) = 1.D0
               K1 = I+1
               IF (E(I+1) .LT. 0.D0) K1 = I+2
               IP1 = I+1
               Y(I) = I
               DO  23 J = IP1, JT
                  SUM = 0.D0
                  KE = Y(J)
                  IF (KE .LT. K1) GOTO 22
                     DO  21 K = K1, KE
                        SUM = SUM+ZL(K, J)*Q(K, I)
  21                    CONTINUE
  22              ZL(I, J) = SUM
  23              CONTINUE
               IM1 = I-1
               IF (I .EQ. ITP1) GOTO 25
                  DO  24 J = ITP1, IM1
                     ZL(I, J) = 0.D0
  24                 CONTINUE
  25           IF (E(I) .LT. 0.D0) GOTO 27
                  I1 = I
C
C APPLY THE PERMUTATIONS WHICH GO WITH THE ITH TRANSFORMATION
C
                  IF (E(I+1) .LT. 0.D0) I1 = I+1
                  K = E(I)
                  DO  26 J = I1, JT
                     IF (IFIX(SNGL(Y(J))) .LT. K) Y(J) = K
                     TEMP = ZL(I1, J)
                     ZL(I1, J) = ZL(K, J)
                     ZL(K, J) = TEMP
  26                 CONTINUE
  27           CONTINUE
  28           CONTINUE
  29     CONTINUE
C
C RESTORE THE DIAGONAL OF Q AND PICK OUT D AND E FROM Q
C
  30  IF (IT .EQ. 0) GOTO 32
         DO  31 I = 1, IT
            Q(I, I) = D(I)
  31        CONTINUE
  32  IF (IT .EQ. JT) GOTO 36
         DO  35 I = ITP1, JT
            TEMP = D(I)
            D(I) = Q(I, I)
            Q(I, I) = TEMP
            IF (D(I) .LE. 0.D0) POSDEF = .FALSE.
            E(I) = 0.D0
            IF (I .EQ. JT) GOTO 34
               IF (E(I+1) .GT. 0.D0) GOTO 33
                  E(I) = Q(I+1, I)
                  POSDEF = .FALSE.
  33           CONTINUE
  34        CONTINUE
  35        CONTINUE
C
C APPLY THE TRANSFORMATION FROM THE REDUCTION OF A TO THE LAST
C N-IT COLUMNS OF THE ZL MATRIX TO FORM THE FINAL Z MATRIX
C
  36  IF (IT .EQ. 0) GOTO 45
         DO  44 IB = 1, IT
            I = IT+1-IB
            IP1 = I+1
            D(I) = (-Y(I))/ZL(I, I)
            ZL(I, I) = 1D0-Y(I)/(D(I)*D(I))
            IF (I .EQ. 1) GOTO 38
               IM1 = I-1
               DO  37 J = 1, IM1
                  Q(I, J) = ZL(I, J)
  37              CONTINUE
C
C FORM THE ITH COLUMN
C
  38        TAU = 1.D0/D(I)
            DO  39 K = IP1, JT
               ZL(K, I) = TAU*ZL(I, K)
  39           CONTINUE
            IF (I .EQ. JT) GOTO 43
               DO  41 J = IP1, JT
                  TAU = DDOT(JT-I, ZL(I, IP1), IZ, ZL(IP1, J), 1)/Y(I)
                  DO  40 K = IP1, JT
                     ZL(K, J) = ZL(K, J)-TAU*ZL(I, K)
  40                 CONTINUE
                  Y(J) = TAU
  41              CONTINUE
C
C FORM THE ITH ROW
C
               SIGMA = Y(I)/D(I)
               DO  42 J = IP1, JT
                  ZL(I, J) = SIGMA*Y(J)
  42              CONTINUE
  43        CONTINUE
  44        CONTINUE
  45  RETURN
      END
