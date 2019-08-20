      SUBROUTINE DQ7RS1(K, LR, P, PP, Q, R, W)
C
C  ***  PERMUTE COLUMN K OF R TO COLUMN P, MODIFY Q ACCORDINGLY  ***
C
      INTEGER K, LR, P, PP
      DOUBLE PRECISION Q(PP,P), R(LR), W(P)
C     DIMSNSION R(P*(P+1)/2)
C
      EXTERNAL DH2RFA, DH2RFG,DV7CPY
      DOUBLE PRECISION DH2RFG
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER I, I1, J, JM1, JP1, J1, KM1, K1, PM1
      DOUBLE PRECISION A, B, T, WJ, X, Y, Z, ZERO
C
      DATA ZERO/0.0D+0/
C
C+++++++++++++++++++++++++++++++  BODY  ++++++++++++++++++++++++++++++++
C
      IF (K .GE. P) GO TO 999
      KM1 = K - 1
      K1 = K * KM1 / 2
      CALL DV7CPY(K, W, R(K1+1))
      WJ = W(K)
      PM1 = P - 1
      J1 = K1 + KM1
      DO 50 J = K, PM1
         JM1 = J - 1
         JP1 = J + 1
         IF (JM1 .GT. 0) CALL DV7CPY(JM1, R(K1+1), R(J1+2))
         J1 = J1 + JP1
         K1 = K1 + J
         A = R(J1)
         B = R(J1+1)
         IF (B .NE. ZERO) GO TO 10
              R(K1) = A
              X = ZERO
              Z = ZERO
              GO TO 40
 10      R(K1) = DH2RFG(A, B, X, Y, Z)
         IF (J .EQ. PM1) GO TO 30
         I1 = J1
         DO 20 I = JP1, PM1
              I1 = I1 + I
              CALL DH2RFA(1, R(I1), R(I1+1), X, Y, Z)
 20           CONTINUE
 30      CALL DH2RFA(PP, Q(1,J), Q(1,JP1), X, Y, Z)
 40      T = X * WJ
         W(J) = WJ + T
         WJ = T * Z
 50      CONTINUE
      W(P) = WJ
      CALL DV7CPY(P, R(K1+1), W)
 999  RETURN
C  ***  LAST LINE OF DQ7RS1 FOLLOWS  ***
      END
