      SUBROUTINE DL7PF(BC, BX, C, G, IW, LIW, LW, M, P, PP, W, X)
      INTEGER LIW, M, P, PP, LW
      INTEGER IW(LIW)
      DOUBLE PRECISION BC(2, M), BX(2, P), C(PP, M), G(P), W(LW), X(P)
      EXTERNAL DV2NRM,DL7P2
      INTEGER MPP, I, Q, R, S, I1
      INTEGER W1, CN, LR
      DOUBLE PRECISION DV2NRM, CNI, ONE, ZERO
      DATA ONE/1.D+0/
      DATA ZERO/0.D+0/
C LP SOLVER, INEQUALITIES ONLY, STARTING FROM FEASIBLE X.
C *** LOCAL VARIABLES ***
C *** BODY ***
      IW(1) = -2
      MPP = M+P
      IF (LIW .LT. MPP+7) GOTO  60
      CN = P+2
      W1 = CN+MPP
      S = W1+5*P
      R = S+P
      LR = P*(P+1)/2
      Q = R+LR
      IW(1) = -(Q+P*P)
      IF (IW(1)+LW .LT. 0) GOTO  60
      IW(1) = 0
      IW(3) = 0
      IW(4) = 0
      IW(5) = 0
      IW(6) = 0
      IW(7) = P
      I1 = MPP+7
      DO  10 I = 8, I1
         IW(I) = I-7
  10     CONTINUE
      I1 = CN
      DO  20 I = 1, P
         W(I1) = ONE
         I1 = I1+1
  20     CONTINUE
      IF (M .LE. 0) GOTO 50
         DO  40 I = 1, M
            CNI = DV2NRM(P, C(1, I))
            IF (CNI .GT. ZERO) GOTO 30
               IW(1) = I+MPP
               GOTO  60
  30        W(I1) = CNI
            I1 = I1+1
  40        CONTINUE
  50  CALL DL7P2(IW(8), BC, BX, C, W(CN), G, IW(1), IW(2), LR, M, IW(3),
     1   IW(4), IW(5), MPP, MPP, P, IW(7), PP, W(Q), W(R), W(1), W(S), W
     2   (W1), X)
  60  RETURN
      END
