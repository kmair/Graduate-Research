      SUBROUTINE  DSN2F(N, P, X, CALCR,   MXFCAL, ACC )
C
C ** SIMPLIED VERSION OF DN2F
C
C INPUT PARAMETERS
C N    NUMBER OF OBSERVATIONS
C P    NUMBER OF UNKNOWNS
C X    APPROXIMATE SOLUTION
C CALCR SUBROUTINE TO EVALUATE RESIDUAL
C MXFCAL MAXIMUM NUMBER OF PERMITTED FUNCTION EVALUATIONS
C ACC   ACCURACY IN X
C OUTPUT PARAMETERS
C X     SOLUTION
      INTEGER N, P, MXFCAL
      DOUBLE PRECISION X(N), ACC
      EXTERNAL CALCR,   DC6LCR
C
C
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER IV, LIV, LV, V1
      DOUBLE PRECISION UR
      COMMON /CSTAK/ DSTAK
      DOUBLE PRECISION DSTAK(500)
      INTEGER ISTAK(1000)
      EQUIVALENCE (DSTAK(1), ISTAK(1))
C
C  ***  BODY  ***
C
      CALL ENTER(0)
C/6S
C     IF (N.LT.1.OR.P.LT.1)
C    1CALL SETERR(24HDSN2F - N.LT.1 OR P.LT.1,24,1,2)
C     IF (MXFCAL.LT.1)
C    1CALL SETERR(19HDSN2F - MXFCAL.LT.1,19,2,2)
C     IF (ACC.LT.0.0D0)
C    1CALL SETERR(18HDSN2F -ACC .LT.0.0,18,3,2)
C/7S
      IF (N.LT.1.OR.P.LT.1)
     1CALL SETERR('DSN2F - N.LT.1 OR P.LT.1',24,1,2)
      IF (MXFCAL.LT.1)
     1CALL SETERR('DSN2F - MXFCAL.LT.1',19,2,2)
      IF (ACC.LT.0.0D0)
     1CALL SETERR('DSN2F -ACC .LT.0.0',18,3,2)
C/
      LIV = P + 82
      LV = P*(N + 2*P + 17) + 2*N + 105
      IV=ISTKGT(LIV,2)
      V1=ISTKGT(LV, 4)
      CALL DIVSET(1,ISTAK(IV),LIV,LV,DSTAK(V1))
      ISTAK(IV+20)=0
      ISTAK(IV+16)=MXFCAL
      ISTAK(IV+17)=MXFCAL
      ISTAK(IV+56)=0
      DSTAK(V1+32)=ACC
      DSTAK(V1+31)=ACC
      CALL  DN2F(N, P, X,  DC6LCR,  ISTAK(IV), LIV, LV,
     1            DSTAK(V1), IU, UR, CALCR)
      J=ISTAK(IV)
      IF(J.LT.7) GO TO 20
C/6S
C     IF (J.EQ.7)CALL SETERR(27HDSN2F -SINGULAR CONVERGENCE,27,4,1)
C     IF(J.EQ.8)CALL SETERR(24HDSN2F -FALSE CONVERGENCE,24,5,1)
C     IF(J.EQ.9)CALL SETERR(32HDSN2F -FUNCTION EVALUATION LIMIT,32,6,1)
C     IF (J.EQ.63)
C    1CALL SETERR(43HDSN2F -F(X) CANNOT BE COMPUTED AT INITIAL X,43,7,1)
C/7S
      IF (J.EQ.7)CALL SETERR('DSN2F -SINGULAR CONVERGENCE',27,4,1)
      IF(J.EQ.8)CALL SETERR('DSN2F -FALSE CONVERGENCE',24,5,1)
      IF(J.EQ.9)CALL SETERR('DSN2F -FUNCTION EVALUATION LIMIT',32,6,1)
      IF (J.EQ.63)
     1CALL SETERR('DSN2F -F(X) CANNOT BE COMPUTED AT INITIAL X',43,7,1)
C/
 20   CALL LEAVE
C
      RETURN
C  *** LAST LINE OF  DSN2F  FOLLOWS  ***
      END
