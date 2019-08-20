      SUBROUTINE  L7CHP(A, ADD2QR, C, CN, G, LR, M, M0, MC, ME,
     1   MOVE, MPP, MPP0, NCA, NCD, P, PC, PP, Q, R, RCOND, S, W, X, Y)
      INTEGER MPP, MPP0, M, P, LR, PP
      INTEGER A(MPP), M0, MC, ME, NCA, NCD
      INTEGER PC
      LOGICAL ADD2QR, MOVE
      REAL C(PP, M), CN(MPP0), G(P), Q(P, P), R(LR), RCOND
      REAL S(P), W(P), X(P), Y(P)
      EXTERNAL  V2NRM,  C7COL,  R7MDC,  V7SCL, I7SHFT,  D7TPR
      EXTERNAL  L7SVX,  L7SVN,  L7ITV,  V7SCP, Q7RGS, V7CPY
      EXTERNAL V2AXY,  D7TP1,  Q7RS1
      INTEGER MCD, MCSAVE, I, J, K, L
      INTEGER IERR, JA, KA, MC0, MC1
      INTEGER ME1
      LOGICAL DRDONE, ADDTRY, DEGEN, SAMEB, XZERO
      REAL  V2NRM, ONE, EPS, SNI, NEGONE,  R7MDC
      REAL D7TPR, NPMEPS,  L7SVX,  L7SVN
      REAL T, ZETA, MEPS, ZERO, P1, AMIN1
      REAL AMAX1, CS, CX, GS, SI, GNORM
      REAL PMEPS, SNORM, GX0,  D7TP1
      INTEGER TEMP
      DATA NEGONE/-1.E+0/
      DATA ONE/1.E+0/
      DATA P1/0.1E+0/
      DATA ZERO/0.E+0/
      DATA MEPS/0.E+0/
C M0 = NO. OF CONSTRAINTS SATISFIED AS EQUALITIES BY S = 0
C MC = NO. OF CONSTRAINTS SATISFIED AS EQUALITIES BY RETURN VALUE OF S
C ME = NO. OF EQUALITY CONSTRAINTS
C MOVE IS SET TO TRUE IFF THE S RETURNED IS NONZERO
C *** LOCAL VARIABLES ***
C-------------------------------  BODY  --------------------------------
      CALL  V7SCP(P, X, ZERO)
      XZERO = .TRUE.
      DRDONE = .FALSE.
      ADDTRY = .TRUE.
      SAMEB = .FALSE.
      GNORM =  V2NRM(P, G)
      IF (GNORM .LE. ZERO) GOTO  440
      IF (MEPS .LE. ZERO) MEPS =  R7MDC(3)
      PMEPS = (FLOAT(P))*MEPS
      NPMEPS = -PMEPS
      ME1 = ME+1
      MC0 = P-PC
      MCD = MC0
      NCA = 0
      NCD = 0
      RCOND = ONE
      IF (MC0 .GT. 0) RCOND =  L7SVN(MC0, R, S, S)/ L7SVX(MC0, R, S, S)
      DEGEN = .FALSE.
  10  MOVE = .FALSE.
      MC0 = P-PC
      CALL  V7SCL(P, S, NEGONE, G)
      IF (MC0 .LE. 0) GOTO 30
         DO  20 I = 1, MC0
            W(I) =  D7TPR(P, Q(1, I), S)
            CALL V2AXY(P, S, -W(I), Q(1, I), S)
  20        CONTINUE
  30  SNORM =  V2NRM(P, S)
      IF (PC .LE. 0) GOTO  150
      IF (SNORM .LE. (GNORM/RCOND)*PMEPS) GOTO  150
      CALL  V7SCL(P, S, ONE/SNORM, S)
      GS =  D7TPR(P, G, S)
      IF (GS .GE. ZERO) GOTO  140
      IF (XZERO) GOTO 40
         IF ((GS-GX0)/GX0 .LE. PMEPS) GOTO  140
  40  MC1 = MC+1
      K = 0
      ZETA = ONE
      IF (.NOT. DRDONE) GOTO 50
         CS =  D7TP1(C, A(MC1), P, PP, S)
         IF (CS .LT. ZERO) GOTO  140
         MC1 = MC1+1
  50  SAMEB = .FALSE.
      IF (MC1 .GT. M0) GOTO 80
         L = MPP0
         DO  70 I = MC1, M0
            J = A(I)
            CS =  D7TP1(C, J, P, PP, S)
            IF (CS .GE. ZERO) GOTO  70
            CX = ZERO
            IF (.NOT. XZERO) CX = AMAX1(ZERO,  D7TP1(C, J, P, PP, X))
            T = CX-CS
            IF (T*ZETA .LT. CX) GOTO  70
            IF (.NOT. DEGEN) GOTO 60
               IF (L .LT. J) GOTO  70
               L = J
  60        ZETA = CX/T
            K = I
  70        CONTINUE
  80  DEGEN = ZETA .LT. P1
      IF (K .NE. 0) GOTO 90
         MOVE = .TRUE.
         CALL V7CPY(P, X, S)
         GX0 = GS
         XZERO = .FALSE.
         SAMEB = .TRUE.
         GOTO  150
  90  IF (ZETA .LE. PMEPS) GOTO 110
         DO  100 I = 1, P
            X(I) = X(I)+ZETA*(S(I)-X(I))
 100        CONTINUE
         CALL  V7SCL(P, X, ONE/ V2NRM(P, X), X)
         GX0 =  D7TPR(P, G, X)
 110  MCD = MC0
      J = A(K)
      MC1 = MC0+1
      ADDTRY = .TRUE.
      CALL  C7COL(C, J, M, P, PP, Q(1, MC1))
      CALL Q7RGS(IERR, A, MC1, P, P, P, MC1, Q, R, Y)
      MC = MC+1
      IF (.NOT. DRDONE) GOTO 120
         I = A(MC)
         A(MC) = J
         A(K) = A(MC+1)
         A(MC+1) = I
         GOTO  130
 120     A(K) = A(MC)
         A(MC) = J
 130  IF (IERR .NE. 0) GOTO  40
      IF ( D7TPR(P, Q(1, MC1), S) .GE. ZERO) GOTO  40
      T =  L7SVN(MC1, R, Y, Y)/ L7SVX(MC1, R, Y, Y)
      IF (T .LE. PMEPS) GOTO  40
      DRDONE = .FALSE.
      RCOND = T
      PC = PC-1
      NCA = NCA+1
      MCD = MCD+1
      A(MC) = A(MC1)
      A(MC1) = J
      CALL V2AXY(P, S, - D7TPR(P, S, Q(1, MC1)), Q(1, MC1), S)
      GOTO  10
 140  IF (ADDTRY) GOTO  150
      MC = MCSAVE
      PC = PC-1
      MC0 = P-PC
 150  K = 0
      IF (ME1 .GT. MCD) GOTO 240
         EPS = PMEPS/RCOND
         CALL  L7ITV(MC0, Y, R, W)
         SNI =  V2NRM(MC0, Y)
         IF (SNI .LE. ZERO) GOTO 230
            SNI = ONE/SNI
            IF (.NOT. DEGEN) GOTO 200
               DO  190 I = ME1, MCD
                  SI = Y(I)
                  IF (SNI*SI .LT. EPS) GOTO  190
                  JA = IABS(A(I))
                  IF (K .NE. 0) GOTO 160
                     K = I
                     KA = JA
                     GOTO  180
 160                 IF (KA .LE. JA) GOTO 170
                        K = I
                        KA = JA
 170              CONTINUE
 180              CONTINUE
 190              CONTINUE
               GOTO  220
 200           T = ZERO
               DO  210 I = ME1, MCD
                  SI = Y(I)*SNI
                  IF (SI .LT. EPS) GOTO  210
                  JA = IABS(A(I))
                  SI = SI/CN(JA)
                  IF (SI .LT. T) GOTO  210
                  T = SI
                  K = I
 210              CONTINUE
 220        CONTINUE
 230     CONTINUE
 240  IF (K .NE. 0) GOTO  250
      IF (MOVE) GOTO  460
      IF (XZERO) GOTO  450
      IF (SAMEB) GOTO  420
      GOTO  270
 250  IF (K .GE. MC0) GOTO 260
         CALL  Q7RS1(K, LR, MC0, P, Q, R, Y)
         CALL I7SHFT(MC0, K, A)
 260  MCD = MCD-1
      J = MC0*(MC0+1)/2
      K = IABS(A(MC0))
      IF (( D7TPR(P, Q(1, MC0), G)/GNORM)*(R(J)/CN(K)) .GE. NPMEPS)
     1   GOTO  150
      RCOND = ONE
      MCSAVE = MC
      MC = MC0-1
      IF (MC .GT. 0) RCOND =  L7SVN(MC, R, Y, Y)/ L7SVX(MC, R, Y, Y)
      PC = PC+1
      NCD = NCD+1
      ADDTRY = .FALSE.
      DRDONE = .TRUE.
      GOTO  10
C *** SPECIAL CASE OF FINISH WITH MOVE = FALSE AND XZERO = FALSE...
C ***  MAKE SURE THAT
C *** (1) S IS ORTHOGONAL TO CONSTRAINTS THE A ARRAY CALLS ACTIVE,
C *** (2) S DOES NOT MAKE A NEGATIVE INNER PRODUCT WITH CONSTRAINTS
C ***  THAT THE A ARRAY CALLS INACTIVE,
C *** (3) RCOND CORRESPONDS TO RETURNED R, AND
C *** (4) Y = VECTOR OF LAGRANGE MULTIPLIERS.
 270  MC0 = P-PC
      K = MC+1
      I = MC
         GOTO  290
 280     I = I-1
 290     IF (I .LE. 0) GOTO  340
         J = A(I)
         IF ( D7TP1(C, J, P, PP, X) .LE. ZERO) GOTO 330
            IF (I .GE. MC0) GOTO 300
               CALL  Q7RS1(I, LR, MC0, P, Q, R, Y)
               CALL I7SHFT(MC0, I, A)
               GOTO  310
 300           A(I) = A(MC)
               A(MC) = J
 310        IF (I .GT. MC0) GOTO 320
               MC0 = MC0-1
               PC = PC+1
 320        MC = MC-1
 330     CONTINUE
         GOTO  280
 340  IF (PC .LE. 0) GOTO 390
         GOTO  360
 350        K = K+1
 360        IF (K .GT. M0) GOTO  380
            J = A(K)
            IF ( D7TP1(C, J, P, PP, X) .GE. ZERO) GOTO 370
               MC1 = MC0+1
               CALL  C7COL(C, J, M, P, PP, Q(1, MC1))
               CALL Q7RGS(IERR, A, MC1, P, P, P, MC1, Q, R, Y)
               MC = MC+1
               A(K) = A(MC)
               A(MC) = J
               IF (IERR .NE. 0) GOTO  350
               T =  L7SVN(MC1, R, Y, Y)/ L7SVX(MC1, R, Y, Y)
               IF (T .LE. PMEPS) GOTO  350
               RCOND = T
               MC0 = MC1
               PC = PC-1
               IF (PC .LE. 0) GOTO  380
 370        CONTINUE
            GOTO  350
 380  CONTINUE
 390  IF (MC0 .LE. 0) GOTO 410
         CALL  V7SCL(P, S, NEGONE, G)
         DO  400 I = 1, MC0
            W(I) =  D7TPR(P, Q(1, I), S)
            CALL V2AXY(P, S, -W(I), Q(1, I), S)
 400        CONTINUE
         CALL  L7ITV(MC0, Y, R, W)
 410  CONTINUE
 420  CALL V7CPY(P, S, X)
      MOVE = .TRUE.
      TEMP = MC+1
      DO  430 I = TEMP, M0
         J = A(I)
         JA = IABS(J)
         T = AMIN1(T,  D7TP1(C, J, P, PP, X)/CN(JA))
 430     CONTINUE
      GOTO  480
 440  CALL  V7SCP(P, Y, ZERO)
 450  CALL  V7SCP(P, S, ZERO)
      MOVE = .FALSE.
      GOTO  480
 460  IF (.NOT. ADD2QR) GOTO 470
         MC0 = P-PC
         CALL V7CPY(P, Q(1, MC0+1), S)
         J = MC0*(MC0+1)/2+1
         K = J+MC0
         IF (MC0 .GT. 0) CALL V7CPY(MC0, R(J), W)
         R(K) = SNORM
 470  CONTINUE
 480  RETURN
      END
