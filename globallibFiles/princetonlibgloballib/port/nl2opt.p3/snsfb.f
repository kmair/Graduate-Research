      SUBROUTINE  SNSFB(N, P,L, X,B,C,Y, CALCA,INC,IINC,MXFCAL, ACC )
C
C ** SIMPLIED VERSION OF  NSFB
C
C INPUT PARAMETERS
C N    NUMBER OF OBSERVATIONS
C P    NUMBER OF NONLINEAR UNKNOWNS
C L    NUMBER OF LINEAR UNKNOWNS
C X    APPROXIMATE SOLUTION NONLINEAR PARAMETERS
C B    FIRST ROW GIVES LOWER BOUNDS ON X, SECOND ROW GIVES UPPOER BOUNDS
C Y    OBSERVATIONS
C CALCA SUBROUTINE TO EVALUATE RESIDUAL
C INC  INCIDENCE ARRAY IF INC(I,J)=1 THEN COLUMN I DEPENDS ON X(J)
C IINC ROW DIMENSIONOF INC MUST BE AT LEAST L+1
C MXFCAL MAXIMUM NUMBER OF PERMITTED FUNCTION EVALUATIONS
C ACC   ACCURACY IN X
C OUTPUT PARAMETERS
C X     SOLUTION
C C     LINEAR PARAMETERS
      INTEGER N, P, MXFCAL, L, IINC
      REAL X(P), ACC , B(2,P), C(L), Y(N)
      INTEGER INC(IINC,P)
      EXTERNAL CALCA,   C6LCA
C
C
C
C  ***  LOCAL VARIABLES  ***
C
      INTEGER IV, LIV, LV, V1
      REAL DSTAK(1000)
      INTEGER ISTAK(1000)
      COMMON /CSTAK/ DSTAK
      EQUIVALENCE (DSTAK(1), ISTAK(1))
C
C  ***  BODY  ***
C
      CALL ENTER(0)
C/6S
C     IF (N.LT.1.OR.P.LT.0.OR.L.LT.0)
C    1CALL SETERR(34H SNSFB- N.LT.1 OR P.LT.0 OR L.LT.0,34,1,2)
C     IF(IINC.LE.L)
C    1CALL SETERR(16H SNSFB-IINC.LE.L,16,2,2)
C     IF (MXFCAL.LT.1)
C    1CALLSETERR(19H SNSFB- MXFCAL.LT.1,19,4,2)
C     IF (ACC.LT.0.0)
C    1CALL SETERR(18H SNSFB-ACC .LT.0.0,18,5,2)
C/7S
      IF (N.LT.1.OR.P.LT.0.OR.L.LT.0)
     1CALL SETERR(' SNSFB- N.LT.1 OR P.LT.0 OR L.LT.0',34,1,2)
      IF(IINC.LE.L)
     1CALL SETERR(' SNSFB-IINC.LE.L',16,2,2)
      IF (MXFCAL.LT.1)
     1CALLSETERR(' SNSFB- MXFCAL.LT.1',19,4,2)
      IF (ACC.LT.0.0)
     1CALL SETERR(' SNSFB-ACC .LT.0.0',18,5,2)
C/
      LP1=L+1
C  ***  CHECK INC, COUNT ITS NONZEROS
C
      L1 = 0
      M = 0
      DO 40 I = 1, P
         M0 = M
         IF (L .EQ. 0) GO TO 20
         DO 10 K = 1, L
            IF (INC(K,I) .LT. 0 .OR. INC(K,I) .GT. 1) GO TO 50
            IF (INC(K,I) .EQ. 1) M = M + 1
 10         CONTINUE
 20      IF (INC(LP1,I) .NE. 1) GO TO 30
            M = M + 1
            L1 = 1
 30      IF (M .EQ. M0 .OR. INC(LP1,I) .LT. 0
     1                 .OR. INC(LP1,I) .GT. 1) GO TO 50
 40      CONTINUE
      GO TO 60
C/6S
C50   CALL SETERR(33H SNSFB- INCIDENCE ARRAY INCORRECT,33,3,2)
C/7S
 50   CALL SETERR(' SNSFB- INCIDENCE ARRAY INCORRECT',33,3,2)
C/
 60   CONTINUE
      LIV = 122+2*M+4*P+2*L+MAX0(LP1,6*P)+3*P
      LV = 105+P*(2*P+22)+2*N*(L+3)+L*(L+3)/2+N*P
      IV=ISTKGT(LIV,2)
      V1=ISTKGT(LV, 3)
      CALL IVSET(1,ISTAK(IV),LIV,LV,DSTAK(V1))
      ISTAK(IV+20)=0
      ISTAK(IV+16)=MXFCAL
      ISTAK(IV+17)=MXFCAL
      ISTAK(IV+56)=0
      DSTAK(V1+32)=ACC
      DSTAK(V1+31)=ACC
      CALL  NSFB(N, P,L, X,B,C,Y,  C6LCA,INC,IINC,
     1  ISTAK(IV), LIV, LV,
     1            DSTAK(V1), IU, UR, CALCA)
      J=ISTAK(IV)
      IF(J.LT.7) GO TO 70
C/6S
C     IF (J.EQ.82)CALL SETERR(26H SNSFB-INCONSISTENT BOUNDS,26,6,1)
C     IF (J.EQ.7)CALL SETERR(27H SNSFB-SINGULAR CONVERGENCE,27,7,1)
C     IF(J.EQ.8)CALL SETERR(24H SNSFB-FALSE CONVERGENCE,24,8,1)
C     IF(J.EQ.9)CALL SETERR(32H SNSFB-FUNCTION EVALUATION LIMIT,32,9,1)
C     IF (J.EQ.63)
C    1CALL SETERR(43H SNSFB-F(X) CANNOT BE COMPUTED AT INITIAL X,43,10,
C    21)
C/7S
      IF (J.EQ.82)CALL SETERR(' SNSFB-INCONSISTENT BOUNDS',26,6,1)
      IF (J.EQ.7)CALL SETERR(' SNSFB-SINGULAR CONVERGENCE',27,7,1)
      IF(J.EQ.8)CALL SETERR(' SNSFB-FALSE CONVERGENCE',24,8,1)
      IF(J.EQ.9)CALL SETERR(' SNSFB-FUNCTION EVALUATION LIMIT',32,9,1)
      IF (J.EQ.63)
     1CALL SETERR(' SNSFB-F(X) CANNOT BE COMPUTED AT INITIAL X',43,10,
     21)
C/
 70    CALL LEAVE
C
      RETURN
C  *** LAST LINE OF  SNSFB FOLLOWS  ***
      END
