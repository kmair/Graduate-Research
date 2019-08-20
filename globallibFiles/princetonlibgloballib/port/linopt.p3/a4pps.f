      SUBROUTINE A4PPS(A, M, N,IA, KK, S, Q,IQ, R, AS, AG, E, IPTS,
     1   DVECS,X,SIMP,ISIMP,TOLL,TOLU,EPS,TEMP,IPRINT,RHS,INDX2,P,
     2   IPHAS, JDEPS,IHIT,INVHIT,ISTRIK)
C
C THIS SUBROUTINE TESTS IF ANY OF THE SIMPLE CONSTRAINTS
C SHOULD BE ADDED TO THE ACTIVE SET AND THEN UPDATES
C THE VECTORS AND LQ DECOMPOSTION
C
      INTEGER M, S, DVECS(1), IA(1),AGPE,AGEP1,IPHAS
      INTEGER N, KK, AS, AG, E, IPTS(1), JDEPS
      INTEGER ISIMP(1)
      REAL A(1),Q(IQ, 1), R( 1), X(1), SIMP(1), TOLL
      REAL RHS(N), P(N)
      REAL TOLU, EPS,PP
      INTEGER NMS, ASP1, I, J, K
      INTEGER IABS, II,  INDEX, NMSGE, ITEMP
      REAL L, PNRM, SNRM2, TEMP(1),R1MACH,SL,SU,EPA
      INTEGER IHIT(N),INVHIT(S)
      COMMON /CSTAK/ISTAK(1000)
      AGPE=AG+E
      ASP1 = AS+1
      NH=0
      NMS = N - AS
      AGEP1=AG+E+1
      II = JDEPS+1
      EPA=SNRM2(N,X,1)*R1MACH(4)+R1MACH(1)
      IF (INDX2.GT.0)II=ASP1+1
      DO  5 I=1,S
 5     INVHIT(I)=0
C
C FIND OUT IF ANY CONSTRAINT HAS BEEN HIT
C
         GOTO  20
 10      II = II+1
 20      IF (II .GT. S) GOTO  46
         K = ISIMP(II)
         L = SIMP(II)
         I = IABS(K)
          SU=L*TOLU
          SL=L*TOLL
          IF (L.LT.0.0E0)SU=L*TOLL
          IF(L.LT.0.E0)SL=L*TOLU
          IF(SU.EQ.0.E0)SU=-EPA
          IF(SL.EQ.0.E0)SL=EPA
          IF (K .GT. 0) GOTO 30
             IF (X(I) .LT. SU.OR.(IPHAS.EQ.1.AND. 
     1   X(I).GT.SL)) GOTO 10
             X(I)=L
             IF (P(I).LE.0.0E0.AND.ISTRIK.NE.II) GO TO 10
             GOTO 40
 30      CONTINUE
         IF  (X(I) .GT. SL.OR.(IPHAS.EQ.1.AND.
     1 X(I).LT.SU)) GOTO 10
             X(I)=L
             IF (P(I).GE.0.0E0.AND.ISTRIK.NE.II) GO TO 10
 40      CONTINUE
         JH=0
         PP=ABS(P(I))
 41      JH=JH+1
         IF(JH.GT.NH) GO TO 45
           J2=IHIT(JH)
           I2=IABS(ISIMP(J2))
           IF (PP.LT.ABS(P(I2)))GO TO 41
           MNH=NH+1
           DO 43 MH=JH,NH
             IHIT(MNH)=IHIT(MNH-1)
             MNH=MNH-1
  43       CONTINUE
           NH=NH+1
           IHIT(JH)=II
           GO TO 10
  45       NH=NH+1
           IHIT(NH)=II
            GO TO 10
C
C DETERMINE WHO IT BELONGS TO
C
  46       IF(NH.EQ.0) GO TO 110
         DO 47 I2=1,NH
             I3=IHIT(I2)
 47       INVHIT(I3)=I2
           DO 105 I2=1,NH
              II=IHIT(I2)
              K=ISIMP(II)
              I=IABS(K)
               L=SIMP(II)
         DO  60 J = ASP1, N
            IF (IPTS(J) .NE. I) GOTO 50
            IP = IPTS(J)
            NMSGE = NMS - AG - E
       JMAS = J-AS
C
C TEST FOR LINEAR INDEPENDENCE
C
            PNRM = SNRM2 (NMSGE, Q(AGEP1,JMAS),1)
               ITEMP2 = DVECS(II)
               JDEPS1=JDEPS+1
               IN1=INVHIT(JDEPS1)
               IN2=INVHIT(ASP1)
               IF (IN2.NE.0)IHIT(IN2)=JDEPS1
               IF(IN1.NE.0)IHIT(IN1)=II
               INVHIT(JDEPS1)=IN2
               INVHIT(II)=IN1
               ISIMP(II)=ISIMP(JDEPS1)
               SIMP(II)=SIMP(JDEPS1)
               DVECS(II)=DVECS(JDEPS1)
C
               ISIMP(JDEPS1)=ISIMP(ASP1)
               SIMP(JDEPS1)=SIMP(ASP1)
                DVECS(JDEPS1)=DVECS(ASP1)
               ISIMP(ASP1) = K
               SIMP(ASP1) = L
               DVECS(ASP1) = ITEMP2
               JDEPS=JDEPS+1
               IF(PNRM.LE.EPS) GO TO 104
C IT IS INDEPENDENT SO CHANGE VECTORS
C
               GOTO  70
 50         CONTINUE
 60         CONTINUE
            GO TO 104
 70      CONTINUE
C
C UPDATE LQ DECOMPOSTION
            INDEX = J - AS
         IF (AGPE.EQ.0) GO TO 80
            ITEMP=IPTS(N)
            CALL MOVEBI(NMS-1,IPTS(ASP1),IPTS(AS+2))
            IF(J.NE.N)IPTS(J+1)=ITEMP
         CALL A4SQR(IQ,NMS,AGPE,Q,R,INDEX,TEMP,RHS)
         GO TO 100
 80       CONTINUE
         CALL MOVEBI(INDEX-1,IPTS(ASP1),IPTS(AS+2))
         NMS=NMS-1
         IF (INDEX.EQ.NMS+1) GO TO 100
          DO 90 IJ=INDEX,NMS
             RHS(IJ)=RHS(IJ+1)
 90       CONTINUE
 100     CONTINUE
         IPTS(ASP1)=I
         AS = AS+1
         ASP1 = ASP1 + 1
         KK = KK+1
  104       CONTINUE
 105     CONTINUE
 110  RETURN
      END
