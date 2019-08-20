      REAL FUNCTION  M7NSX(DX, FUNC, IRC, ITMX, P, P1, PP,
     1                                 S, TOL, X0, XE, XR, Y)
C
C *** NELDER-MEAD SIMPLEX METHOD (PATTERN SEARCH) FOR UNCONSTRAINED
C *** MINIMIZATION.
C
      INTEGER IRC, ITMX, P, P1, PP
      REAL DX(P), FUNC, S(PP,P1), TOL, X0(P), XE(P), XR(P),
     1                 Y(P1)
      EXTERNAL FUNC
C
C *** PARAMETERS ***
C
C DX   (SCRATCH) STEP DIRECTION VECTOR
C FUNC (INPUT)   EXTERNAL FUNCTION TO COMPUTE F(X)
C IRC  (OUTPUT)  RETURN CODE... 1 = CONVERGENCE TEST SATISFIED,
C                2 = ITERATION LIMIT EXCEEDED,
C                3 = INVALID INPUT, 4 = CONVERGENCE TEST SATISFIED,
C                BUT F IS LARGER AT THE CENTROID THAN AT ANY VERTEX.
C ITMX (INPUT)   MAX. NO. OF ITERATIONS ALLOWED
C P    (INPUT)   PROBLEM DIMENSION
C P1   (INPUT)   P+1, THE NUMBER OF COLUMNS OF S
C PP   (INPUT)   LEAD DIMENSION OF S, WHICH MUST BE AT LEAST P
C S    (IN/OUT)  ON INPUT, THE INITIAL SIMPLEX.  ON OUTPUT, THE FINAL
C                SIMPLEX
C TOL  (IN/OUT)  ON INPUT, THE CONVERGENCE TOLERANCE (ON THE STANDARD
C                DEVIATION OF THE FUNCTION VALUES AT THE SIMPLEX
C                VERTICES).  ON OUTPUT, TOL IS SET TO THIS STANDARD
C                DEVIATION.
C X0   (OUTPUT)  THE BEST X VALUE FOUND, CORRESPONDING TO THE RETURN
C                VALUE FROM  M7NSX
C XE   (SCRATCH)
C XR   (SCRATCH)
C Y    (OUTPUT)  THE FUNCTION VALUES AT THE CURRENT SIMPLEX VERTICES
C
C
C *** INTRINSIC FUNCTIONS ***
C/+
      REAL  SQRT
C/
C *** EXTERNAL FUNCTIONS AND SUBROUTINES ***
C
      REAL  V2NRM
      EXTERNAL  V2NRM, V2AXY, V7CPY,  V7SCL,  V7SCP
C
C *** LOCAL VARIABLES ***
C
      INTEGER I, ITER, J, KH, KL, KS
      REAL SD, T, YE, YH, YI, YL, YR, YS
C
      REAL P1INV, PINV
      REAL ALPHA, BETA, GAMMA, HALF, ONE, NEGONE, ZERO
      DATA ALPHA/1.0E+0/, BETA/0.5E+0/, GAMMA/2.E+0/, HALF/0.5E+0/,
     1     ONE/1.E+0/, NEGONE/-1.E+0/, ZERO/0.E+0/
C
C *** BODY ***
C
      IF (P1 .NE. P + 1) GO TO 190
      IF (P .LE. 0) GO TO 190
      IF (PP .LT. P) GO TO 190
C
      PINV = ONE / FLOAT(P)
      P1INV = ONE / FLOAT(P1)
C
      DO 10 I = 1, P1
 10     Y(I) = FUNC(P, S(1,I))
C
      IRC = 1
      DO 150 ITER = 1, ITMX
C
C *** CONVERGENCE TEST ***
C
         SD =  V2NRM(P1, Y)
         IF (SD .LE. ZERO) GO TO 160
         T = ZERO
         DO 20 I = 1, P1
 20         T = T + Y(I)
         T = P1INV * T / SD
         SD = SD *  SQRT( AMAX1(P1INV - T**2, ZERO) )
         IF (SD .LE. TOL) GO TO 160
C
C *** FIND XH, XL ***
C
         KH = 1
         KL = 1
         YH = Y(1)
         YL = YH
         DO 40 I = 2, P1
            YI = Y(I)
            IF (YI .LE. YH) GO TO 30
               YH = YI
               KH = I
 30         IF (YI .GE. YL) GO TO 40
               YL = YI
               KL = I
 40         CONTINUE
C
C *** COMPUTE SIMPLEX CENTROID X0 ***
C
         CALL  V7SCP(P, X0, ZERO)
         DO 50 I = 1, P1
            IF (I .NE. KH) CALL V2AXY(P, X0, ONE, X0, S(1,I))
 50         CONTINUE
         CALL  V7SCL(P, X0, PINV, X0)
C
C *** COMPUTE DIRECTION DX AND REFLECTION POINT XR ***
C
         CALL V2AXY(P, DX, NEGONE, S(1,KH), X0)
         CALL V2AXY(P, XR, ALPHA, DX, X0)
         YR = FUNC(P, XR)
         IF (YR .GE. YL) GO TO 60
C
C *** CONSIDER EXPANSION STEP ***
C
         CALL V2AXY(P, XE, GAMMA*ALPHA, DX, X0)
         YE = FUNC(P, XE)
         IF (YE .GE. YL) GO TO 90
C        *** ACCEPT EXPANSION STEP ***
         CALL V7CPY(P, S(1,KH), XE)
         Y(KH) = YE
         GO TO 150
C
C *** FIND SECOND HIGHEST FUNCTION VALUE ***
C
 60      YS = Y(1)
         KS = 1
         IF (KH .NE. 1) GO TO 70
            YS = Y(2)
            KS = 2
 70      DO 80 I = 2, P1
            IF (I .EQ. KH) GO TO 80
            YI = Y(I)
            IF (YI .LE. YS) GO TO 80
            YS = YI
            KS = I
 80         CONTINUE
C
         IF (YS .LT. YR) GO TO 100
C        *** ACCEPT REFLECTION STEP ***
 90      CALL V7CPY(P, S(1,KH), XR)
         Y(KH) = YR
         GO TO 150
C
C *** COMPUTE CONTRACTION STEP ***
C
 100     IF (YR .GE. YH) GO TO 110
            CALL V7CPY(P, S(1,KH), XR)
            Y(KH) = YR
            YH = YR
            CALL V2AXY(P, DX, NEGONE, XR, X0)
 110     CALL V2AXY(P, XE, -BETA, DX, X0)
         YE = FUNC(P, XE)
C
         IF (YE .GE. YH) GO TO 120
C        *** ACCEPT CONTRACTION STEP ***
         CALL V7CPY(P, S(1,KH), XE)
         Y(KH) = YE
         GO TO 150
C
C *** SHRINK THE SIMPLEX ***
C
 120     DO 140 I = 1, P1
            IF (I .EQ. KL) GO TO 140
            DO 130 J = 1, P
 130            S(J,I) = HALF * (S(J,I) + S(J,KL))
            Y(I) = FUNC(P, S(1,I))
 140        CONTINUE
C
 150     CONTINUE
C
C *** ITERATION LIMIT EXCEEDED ***
C
      IRC = 2
C
C *** CONVERGENCE -- RETURN BEST POINT IN X0 ***
C
 160  TOL = SD
      YL = Y(1)
      YH = YL
      KL = 1
      CALL  V7SCL(P, X0, P1INV, S)
      DO 180 I = 2, P1
         CALL V2AXY(P, X0, P1INV, S(1,I), X0)
         YI = Y(I)
         IF (YI .GE. YL) GO TO 170
            YL = YI
            KL = I
            GO TO 180
 170     IF (YH .LT. YI) YH = YI
 180     CONTINUE
       M7NSX = FUNC(P, X0)
      IF ( M7NSX .LE. YL) GO TO 999
      CALL V7CPY(P, X0, S(1,KL))
      T =  M7NSX
       M7NSX = YL
      IF (IRC .EQ. 2) GO TO 999
      IF (T .GT. YH + SD) IRC = 4
      GO TO 999
C
C *** INVALID INPUT ***
C
 190  IRC = 3
       M7NSX = ZERO
C
 999  RETURN
C *** LAST LINE OF  M7NSX FOLLOWS ***
      END
