      SUBROUTINE  SMNH( P, X, CALCF, CALCGH,   MXFCAL, ACC )
C
C ** SIMPLIED VERSION OF MNH
C
C INPUT PARAMETERS
C P    NUMBER OF UNKNOWNS
C X    APPROXIMATE SOLUTION
C CALCF SUBROUTINE TO EVALUATE FUNCTION
C CALCGH SUBROUTINE TO EVALUATE THE GRADIENTAND THE HESSIAN
C MXFCAL MAXIMUM NUMBER OF PERMITTED FUNCTION EVALUATIONS
C ACC   ACCURACY IN X
C OUTPUT PARAMETERS
C X     SOLUTION
      INTEGER  P, MXFCAL
      REAL X(P), ACC
      EXTERNAL CALCF, CALCGH
C
C
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER IV, LIV, LV, V1
      INTEGER IDI,IDM1,ID,J
      REAL DSTAK(1000)
      COMMON /CSTAK/ DSTAK
      INTEGER ISTAK(1000)
      EQUIVALENCE (DSTAK(1), ISTAK(1))
C
C  ***  BODY  ***
C
      CALL ENTER(0)
C/6S
C     IF (P.LT.1)
C    1CALL SETERR(14H SMNH - P.LT.1,14,1,2)
C     IF (MXFCAL.LT.1)
C    1CALL SETERR(19H SMNH - MXFCAL.LT.1,19,2,2)
C     IF (ACC.LT.0.0)
C    1CALL SETERR(18H SMNH -ACC .LT.0.0,18,3,2)
C/7S
      IF (P.LT.1)
     1CALL SETERR(' SMNH - P.LT.1',14,1,2)
      IF (MXFCAL.LT.1)
     1CALL SETERR(' SMNH - MXFCAL.LT.1',19,2,2)
      IF (ACC.LT.0.0)
     1CALL SETERR(' SMNH -ACC .LT.0.0',18,3,2)
C/
      LIV =59
      LV=78+P*(P+12)
      IV=ISTKGT(LIV,2)
      V1=ISTKGT(LV, 3)
      CALL IVSET(2,ISTAK(IV),LIV,LV,DSTAK(V1))
      ISTAK(IV+20)=0
      ISTAK(IV+16)=MXFCAL
      ISTAK(IV+17)=MXFCAL
      DSTAK(V1+32)=ACC
      DSTAK(V1+31)=ACC
      ID=ISTKGT(P, 3)
      IDM1=ID-1
      DO 10 I=1,P
         IDI=IDM1+I
         DSTAK(IDI)=1.0
         IF (X(I).NE.0.0)DSTAK(IDI)=1.0/ABS(X(I))
 10   CONTINUE
      CALL  J6MNH( P, DSTAK(ID),X,  CALCF,CALCGH,  ISTAK(IV), LIV, LV,
     1            DSTAK(V1))
      J=ISTAK(IV)
      IF(J.LT.7) GO TO 20
C/6S
C     IF (J.EQ.7)CALL SETERR(27H SMNH -SINGULAR CONVERGENCE,27,4,1)
C     IF(J.EQ.8)CALL SETERR(24H SMNH -FALSE CONVERGENCE,24,5,1)
C     IF(J.EQ.9)CALL SETERR(32H SMNH -FUNCTION EVALUATION LIMIT,32,6,1)
C     IF (J.EQ.63)
C    1CALL SETERR(43H SMNH -F(X) CANNOT BE COMPUTED AT INITIAL X,43,7,1)
C     IF (J.EQ.65)
C    1CALL SETERR(47H SMNH -GRADIENT CANNOT BE COMPUTED AT INITIAL X,
C    247,8,1)
C/7S
      IF (J.EQ.7)CALL SETERR(' SMNH -SINGULAR CONVERGENCE',27,4,1)
      IF(J.EQ.8)CALL SETERR(' SMNH -FALSE CONVERGENCE',24,5,1)
      IF(J.EQ.9)CALL SETERR(' SMNH -FUNCTION EVALUATION LIMIT',32,6,1)
      IF (J.EQ.63)
     1CALL SETERR(' SMNH -F(X) CANNOT BE COMPUTED AT INITIAL X',43,7,1)
      IF (J.EQ.65)
     1CALL SETERR(' SMNH -GRADIENT CANNOT BE COMPUTED AT INITIAL X',
     247,8,1)
C/
 20   CALL LEAVE
C
      RETURN
C  *** LAST LINE OF  SMNH  FOLLOWS  ***
      END
