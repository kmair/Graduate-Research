      SUBROUTINE  DSMNGB( P, X,B, CALCF,CALCG,   MXFCAL, ACC )
C
C ** SIMPLIED VERSION OF DMNGB
C
C INPUT PARAMETERS
C P    NUMBER OF UNKNOWNS
C X    APPROXIMATE SOLUTION
C B    FIRST ROW OF B GIVES LOWER BOUNDS ON X AND SECOND GIVES UPPER
C      BOUNDS
C CALCF SUBROUTINE TO EVALUATE FUNCTION
C CALCG SUBROUTINE TO EVALUATE DERIVATIVE
C MXFCAL MAXIMUM NUMBER OF PERMITTED FUNCTION EVALUATIONS
C ACC   ACCURACY IN X
C OUTPUT PARAMETERS
C X     SOLUTION
      INTEGER  P, MXFCAL
      DOUBLE PRECISION X(P), ACC ,B(2,P)
      EXTERNAL CALCF,   CALCG
C
C
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER IV, LIV, LV, V1
      INTEGER IDI,IDM1,ID,J
      DOUBLE PRECISION DSTAK(500)
      COMMON /CSTAK/ DSTAK
      INTEGER ISTAK(1000)
      EQUIVALENCE (DSTAK(1), ISTAK(1))
C
C  ***  BODY  ***
C
      CALL ENTER(0)
C/6S
C     IF (P.LT.1)
C    1CALL SETERR(14HDSMNGB- P.LT.1,14,1,2)
C     IF (MXFCAL.LT.1)
C    1CALL SETERR(19HDSMNGB- MXFCAL.LT.1,19,2,2)
C     IF (ACC.LT.0.0D0)
C    1CALL SETERR(18HDSMNGB-ACC .LT.0.0,18,3,2)
C/7S
      IF (P.LT.1)
     1CALL SETERR('DSMNGB- P.LT.1',14,1,2)
      IF (MXFCAL.LT.1)
     1CALL SETERR('DSMNGB- MXFCAL.LT.1',19,2,2)
      IF (ACC.LT.0.0D0)
     1CALL SETERR('DSMNGB-ACC .LT.0.0',18,3,2)
C/
      LIV =59+P
      LV=71+P*(P+21)/2
      IV=ISTKGT(LIV,2)
      V1=ISTKGT(LV, 4)
      CALL DIVSET(2,ISTAK(IV),LIV,LV,DSTAK(V1))
      ISTAK(IV+20)=0
      ISTAK(IV+16)=MXFCAL
      ISTAK(IV+17)=MXFCAL
      DSTAK(V1+32)=ACC
      DSTAK(V1+31)=ACC
      ID=ISTKGT(P, 4)
      IDM1=ID-1
      DO 10 I=1,P
         IDI=IDM1+I
         DSTAK(IDI)=1.0
         IF (X(I).NE.0.0)DSTAK(IDI)=1.0/DABS(X(I))
 10   CONTINUE
      CALL DM6NGB( P, DSTAK(ID),X,B,  CALCF,CALCG,  ISTAK(IV), LIV, LV,
     1            DSTAK(V1))
      J=ISTAK(IV)
      IF(J.LT.7) GO TO 20
C/6S
C     IF (J.EQ.82)CALL SETERR(26HDSMNGB-INCONSISTENT BOUNDS,26,4,1)
C     IF (J.EQ.7)CALL SETERR(27HDSMNGB-SINGULAR CONVERGENCE,27,5,1)
C     IF(J.EQ.8)CALL SETERR(24HDSMNGB-FALSE CONVERGENCE,24,6,1)
C     IF(J.EQ.9)CALL SETERR(32HDSMNGB-FUNCTION EVALUATION LIMIT,32,7,1)
C     IF (J.EQ.63)
C    1CALL SETERR(43HDSMNGB-F(X) CANNOT BE COMPUTED AT INITIAL X,43,8,1)
C     IF(J.EQ.65)
C    1CALL SETERR(47HDSMNGB-JACOBIAN CANNOT BE COMPUTED AT INITIAL X,
C    247,9,1)
C/7S
      IF (J.EQ.82)CALL SETERR('DSMNGB-INCONSISTENT BOUNDS',26,4,1)
      IF (J.EQ.7)CALL SETERR('DSMNGB-SINGULAR CONVERGENCE',27,5,1)
      IF(J.EQ.8)CALL SETERR('DSMNGB-FALSE CONVERGENCE',24,6,1)
      IF(J.EQ.9)CALL SETERR('DSMNGB-FUNCTION EVALUATION LIMIT',32,7,1)
      IF (J.EQ.63)
     1CALL SETERR('DSMNGB-F(X) CANNOT BE COMPUTED AT INITIAL X',43,8,1)
      IF(J.EQ.65)
     1CALL SETERR('DSMNGB-JACOBIAN CANNOT BE COMPUTED AT INITIAL X',
     247,9,1)
C/
 20   CALL LEAVE
C
      RETURN
C  *** LAST LINE OF  DSMNGB FOLLOWS  ***
      END
