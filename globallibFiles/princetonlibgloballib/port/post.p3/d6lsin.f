      LOGICAL FUNCTION D6LSIN(K, X, NX, AF, AF1, NU, NRHS, NRHSG
     1   , MQ, XQ, WQ, GETJAC, SEPATE, G, GT, B, BT, ORDER, IGSSIS, SET,
     2   TQ, SBASIS, A1, A1T, A2, A2T, A3, A3T, A4, A4T, F1, F1T, F2,
     3   F2T, ZERO, GSBSIS, GDIM1, GDIM2, GSDIM1)
      INTEGER K, NRHS, GDIM1, GDIM2, MQ, NU
      INTEGER NX, GSDIM1
      EXTERNAL AF, AF1
      INTEGER NRHSG, ORDER(NU, NU, 2), IGSSIS
      LOGICAL AF, GETJAC, SEPATE, SET, ZERO(NU)
      DOUBLE PRECISION X(NX), XQ(MQ), WQ(MQ), G(GDIM1, GDIM2), GT(GDIM1,
     1   GDIM2), B(GDIM1, NRHS)
      DOUBLE PRECISION BT(GDIM1, NRHS), TQ(MQ), SBASIS(MQ, K, 2), A1(MQ,
     1   NU, NU), A1T(MQ, NU, NU), A2(MQ, NU, NU)
      DOUBLE PRECISION A2T(MQ, NU, NU), A3(MQ, NU, NU), A3T(MQ, NU, NU),
     1   A4(MQ, NU, NU), A4T(MQ, NU, NU), F1(MQ, NU, NRHS)
      DOUBLE PRECISION F1T(MQ, NU, NRHS), F2(MQ, NU, NRHS), F2T(MQ, NU
     1   , NRHS), GSBSIS(GSDIM1, 1)
      INTEGER ILR, INT, MAX0, I, J, P
      INTEGER Q, JRHS, GIDX1, GIDX2, ERRATE, ID(2)
      INTEGER IQ, NSPLN
      LOGICAL A12ERO, A40, A1230, A4ZERO
      DOUBLE PRECISION T, SCALE, TT
      INTEGER TEMP4
      LOGICAL TEMP, TEMP1, TEMP2, TEMP3
      DATA ID(1)/0/
      DATA ID(2)/1/
      ERRATE = 0
      NSPLN = NX-K
      IF (GETJAC) CALL SETD(GDIM1*GDIM2, 0D0, G)
C INITIALIZE G.
      TEMP = GETJAC
      IF (TEMP) TEMP = SEPATE
      IF (TEMP) CALL SETD(GDIM1*GDIM2, 0D0, GT)
C GT TOO.
      IF (NRHSG .GT. 0) CALL SETD(GDIM1*NRHSG, 0D0, B)
C INITIALIZE B.
      TEMP = SEPATE
      IF (TEMP) TEMP = NRHSG .GT. 0
      IF (TEMP) CALL SETD(GDIM1*NRHSG, 0D0, BT)
C INITIALIZE BT.
      IF (GETJAC) CALL SETI(2*NU**2, -1, ORDER)
C INITIALIZE ORDER.
C DO INTEGRALS OVER EACH MESH INTERVAL.
      DO  33 INT = K, NSPLN
C/6S
C        IF (X(INT) .GT. X(INT+1)) CALL SETERR(
C    1      37HDGLSIN - X IS NOT MONOTONE INCREASING, 37, 12, 2)
C/7S
         IF (X(INT) .GT. X(INT+1)) CALL SETERR(
     1      'DGLSIN - X IS NOT MONOTONE INCREASING', 37, 12, 2)
C/
         IF (INT+K .GT. NX) GOTO 1
            TEMP4 = INT+K
C/6S
C           IF (X(TEMP4) .LE. X(INT)) CALL SETERR(
C    1         37HDGLSIN - X IS NOT MONOTONE INCREASING, 37, 12, 2)
C/7S
            IF (X(TEMP4) .LE. X(INT)) CALL SETERR(
     1         'DGLSIN - X IS NOT MONOTONE INCREASING', 37, 12, 2)
C/
   1     IF (X(INT) .EQ. X(INT+1)) GOTO  33
C GET THE QUADRATURE POINTS ON (X(INT),X(INT+1)).
         DO  2 IQ = 1, MQ
            TQ(IQ) = 0.5D0*((X(INT+1)+X(INT))+(X(INT+1)-X(INT))*XQ(IQ))
   2        CONTINUE
         SCALE = 0.5D0*(X(INT+1)-X(INT))
         IF (.NOT. SET) GOTO 3
            TEMP4 = INT-K
            CALL MOVEFD(2*MQ*K, GSBSIS(1, TEMP4+1), SBASIS)
            GOTO  5
   3        CALL DBSPL1(K, X, NX, TQ, MQ, INT, ID, 2, SBASIS)
C GET BASIS SPLINES.
            IF (IGSSIS .LE. 1) GOTO 4
               TEMP4 = INT-K
               CALL MOVEFD(2*MQ*K, SBASIS, GSBSIS(1, TEMP4+1))
   4        CONTINUE
C DEFAULT A AND F VALUES.
   5     CALL SETD(4*MQ*NU*(2*NU+NRHS), 0D0, A1)
C GET A AND F.
         IF (.NOT. AF(TQ, MQ, NU, NRHS, NRHSG, GETJAC, SEPATE, A1, A1T
     1      , A2, A2T, A3, A3T, A4, A4T, F1, F1T, F2, F2T, INT, SBASIS
     2      , K, X, NX, AF1)) GOTO 6
            D6LSIN = .TRUE.
            RETURN
C DO THE I-TH ODE.
   6     DO  32 I = 1, NU
            IF (.NOT. GETJAC) GOTO 18
               A12ERO = .TRUE.
               A4ZERO = .TRUE.
               DO  17 J = 1, NU
                  DO  16 IQ = 1, MQ
                     TEMP = A1(IQ, I, J) .EQ. 0D0
                     IF (TEMP) TEMP = A1T(IQ, I, J) .EQ. 0D0
                     IF (TEMP) TEMP = A2(IQ, I, J) .EQ. 0D0
                     IF (TEMP) TEMP = A2T(IQ, I, J) .EQ. 0D0
                     IF (TEMP) TEMP = A3(IQ, I, J) .EQ. 0D0
                     IF (TEMP) TEMP = A3T(IQ, I, J) .EQ. 0D0
                     A1230 = TEMP
                     TEMP = A4(IQ, I, J) .EQ. 0D0
                     IF (TEMP) TEMP = A4T(IQ, I, J) .EQ. 0D0
                     A40 = TEMP
                     A12ERO = A12ERO .AND. A1230
                     A4ZERO = A4ZERO .AND. A40
                     TEMP = INT .EQ. K
                     IF (TEMP) TEMP = IQ .EQ. 1
                     IF (TEMP) GOTO 7
                        TEMP1 = INT .EQ. NSPLN
                        IF (TEMP1) TEMP1 = IQ .EQ. MQ
                        TEMP = TEMP1
   7                 IF (.NOT. TEMP) GOTO 14
                        IF (INT .NE. K) GOTO 8
                           ILR = 1
C SET ORDER.
                           GOTO  9
   8                       ILR = 2
   9                    TEMP1 = A1(IQ, I, J) .NE. 0D0
                        IF (.NOT. TEMP1) TEMP1 = A1T(IQ, I, J) .NE. 0D0
                        IF (.NOT. TEMP1) GOTO 10
                           ORDER(I, J, ILR) = MAX0(ORDER(I, J, ILR), 2)
                           GOTO  13
  10                       TEMP2 = A2(IQ, I, J) .NE. 0D0
                           IF (.NOT. TEMP2) TEMP2 = A2T(IQ, I, J) .NE.
     1                        0D0
                           IF (.NOT. TEMP2) TEMP2 = A3(IQ, I, J) .NE.
     1                        0D0
                           IF (.NOT. TEMP2) TEMP2 = A3T(IQ, I, J) .NE.
     1                        0D0
                           IF (.NOT. TEMP2) GOTO 11
                              ORDER(I, J, ILR) = MAX0(ORDER(I, J, ILR)
     1                           , 1)
                              GOTO  12
  11                          TEMP3 = A4(IQ, I, J) .NE. 0D0
                              IF (.NOT. TEMP3) TEMP3 = A4T(IQ, I, J)
     1                            .NE. 0D0
                              IF (TEMP3) ORDER(I, J, ILR) = MAX0(ORDER(I
     1                           , J, ILR), 0)
  12                    CONTINUE
  13                    TEMP1 = K .EQ. NSPLN
                        IF (TEMP1) TEMP1 = I .EQ. NU
                        IF (TEMP1) TEMP1 = J .EQ. NU
                        IF (TEMP1) TEMP1 = IQ .EQ. MQ
                        IF (TEMP1) CALL MOVEFI(NU**2, ORDER, ORDER(1, 1,
     1                     2))
  14                 A1(IQ, I, J) = A1(IQ, I, J)*WQ(IQ)
                     A2(IQ, I, J) = A2(IQ, I, J)*WQ(IQ)
                     A3(IQ, I, J) = A3(IQ, I, J)*WQ(IQ)
                     A4(IQ, I, J) = A4(IQ, I, J)*WQ(IQ)
                     IF (.NOT. SEPATE) GOTO 15
                        A1T(IQ, I, J) = A1T(IQ, I, J)*WQ(IQ)
                        A2T(IQ, I, J) = A2T(IQ, I, J)*WQ(IQ)
                        A3T(IQ, I, J) = A3T(IQ, I, J)*WQ(IQ)
                        A4T(IQ, I, J) = A4T(IQ, I, J)*WQ(IQ)
  15                 CONTINUE
  16                 CONTINUE
C END IQ.
C DOES U(J) NOT APPEAR IN ODE(I)?
                  TEMP = A12ERO
                  IF (TEMP) TEMP = A4ZERO
                  ZERO(J) = TEMP
  17              CONTINUE
C END J.
               TEMP = .NOT. A4ZERO
               IF (TEMP) TEMP = A12ERO
               IF (TEMP) TEMP = MQ .EQ. K-1
               IF (TEMP) ERRATE = 1
               TEMP = A12ERO
               IF (TEMP) TEMP = A4ZERO
               IF (TEMP) ERRATE = 2
C MAKE THE I-TH EQUATION'S RESIDUAL
  18        DO  25 P = 1, K
C ORTHOGONAL TO B-SUB-P.
               GIDX1 = I+(INT-K-1+P)*NU
C       GET THE RIGHT-HAND-SIDE B.
               JRHS = 1
                  GOTO  20
  19              JRHS = JRHS+1
  20              IF (JRHS .GT. NRHSG) GOTO  24
                  T = 0
                  DO  21 IQ = 1, MQ
                     T = T+(F1(IQ, I, JRHS)*SBASIS(IQ, P, 2)-F2(IQ, I,
     1                  JRHS)*SBASIS(IQ, P, 1))*WQ(IQ)
  21                 CONTINUE
                  B(GIDX1, JRHS) = B(GIDX1, JRHS)+SCALE*T
                  IF (.NOT. SEPATE) GOTO 23
                     TT = 0
                     DO  22 IQ = 1, MQ
                        TT = TT+(F1T(IQ, I, JRHS)*SBASIS(IQ, P, 2)-F2T(
     1                     IQ, I, JRHS)*SBASIS(IQ, P, 1))*WQ(IQ)
  22                    CONTINUE
                     BT(GIDX1, JRHS) = BT(GIDX1, JRHS)+SCALE*TT
  23              CONTINUE
                  GOTO  19
  24           CONTINUE
  25           CONTINUE
            IF (.NOT. GETJAC) GOTO  32
C GET THE J-TH COMPONENT OF (I,P) RELATION.
            DO  31 J = 1, NU
               IF (ZERO(J)) GOTO  31
C MAKE THE I-TH EQUATION'S RESIDUAL
               DO  30 P = 1, K
C ORTHOGONAL TO B-SUB-P.
                  GIDX1 = I+(INT-K-1+P)*NU
                  GIDX2 = J-I+(K+1-P)*NU
C Q-TH COEFFICIENT OF THE (I,P,J) RELATION.
                  DO  29 Q = 1, K
                     T = 0
                     DO  26 IQ = 1, MQ
                        T = T+(A1(IQ, I, J)*SBASIS(IQ, Q, 2)+A2(IQ, I, J
     1                     )*SBASIS(IQ, Q, 1))*SBASIS(IQ, P, 2)+(A3(IQ
     2                     , I, J)*SBASIS(IQ, Q, 2)+A4(IQ, I, J)*SBASIS(
     3                     IQ, Q, 1))*SBASIS(IQ, P, 1)
  26                    CONTINUE
                     G(GIDX1, GIDX2) = G(GIDX1, GIDX2)+T*SCALE
                     IF (.NOT. SEPATE) GOTO 28
                        TT = 0
                        DO  27 IQ = 1, MQ
                           TT = TT+(A1T(IQ, I, J)*SBASIS(IQ, Q, 2)+A2T(
     1                        IQ, I, J)*SBASIS(IQ, Q, 1))*SBASIS(IQ, P
     2                        , 2)+(A3T(IQ, I, J)*SBASIS(IQ, Q, 2)+A4T(
     3                        IQ, I, J)*SBASIS(IQ, Q, 1))*SBASIS(IQ, P
     4                        , 1)
  27                       CONTINUE
                        GT(GIDX1, GIDX2) = GT(GIDX1, GIDX2)+TT*SCALE
  28                 GIDX2 = GIDX2+NU
  29                 CONTINUE
C END Q.
  30              CONTINUE
C END P.
  31           CONTINUE
C END J.
  32        CONTINUE
C END I.
  33     CONTINUE
C END INT.
      TEMP = .NOT. SET
      IF (TEMP) TEMP = IGSSIS .GT. 1
      IF (TEMP) SET = .TRUE.
C/6S
C     IF (ERRATE .EQ. 1) CALL SETERR(
C    1   33HDGLSIN - MQ=K-1 WHEN ORDER(I,.)=0, 33, 14, 1)
C     IF (ERRATE .EQ. 2) CALL SETERR(26HDGLSIN - ODE(I) IS VACUOUS, 26
C    1   , 15, 1)
C/7S
      IF (ERRATE .EQ. 1) CALL SETERR(
     1   'DGLSIN - MQ=K-1 WHEN ORDER(I,.)=0', 33, 14, 1)
      IF (ERRATE .EQ. 2) CALL SETERR('DGLSIN - ODE(I) IS VACUOUS', 26
     1   , 15, 1)
C/
      D6LSIN = .FALSE.
      RETURN
      END
