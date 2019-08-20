      SUBROUTINE C4ONS(A, M, N, IA, S, P, V, W, AG, JDEPS,AS, PNRM,
     1   IPTS,IPTG,SIMP,ISIMP,AMAN,EPS,SCALE,X,THETA,T,IHIT,IERC)
C
C THIS SUBROUTINE LEAVES IN THETA THE DISTANCE ONE
C CAN TRAVEL ALONG THE DIRECTION P BEFORE HITTING
C THE INACTIVE CONSTRAINTS
C
C IF IHIT IS LESS THAN M, THE IHITTH GENERAL CONSTRAINT HAS BEEN
C HIT. IF GREATER THAN M, THEN THE IHITTH-M SIMPLE CONSTRAINT HAS BEEN
C HIT.
C
      INTEGER M, N, S, IA(1), ISIMP(1)
      EXTERNAL AMAN
      INTEGER AG, AS, IPTS(1), IPTG(1)
      REAL A(1), P(1), V(1), W(1), PNRM, EPS,THETA2
      REAL SCALE(1), X(1), FLOAT
      INTEGER NMS, AGP1, ASP1, I, K, II
      REAL T(1), TEMP1, THETA, SIMP(1), EPP, ET, VV, WW
      LOGICAL UNBNDD
      INTEGER IT1
      IERC=0
      EPP=PNRM*EPS
      UNBNDD = .TRUE.
      AGP1 = AG+1
      ASP1 = AS+1
      THETA2=0.0E0
      IHIT2=0
      IF (M .LT. AGP1) GOTO  50
C
C DETERMINE THE DISTANCE TO THE NEAREST GENERAL CONSTRAINT
      DO  40 IJ = AGP1, M
         I = IPTG(IJ)
         CALL AMAN(.FALSE., A, IA, N, I, T, TEMP1)
         V(I) = 0.
      NMS=N-AS
         DO  10 II = 1, NMS
            IT1 = AS+II
            K = IPTS(IT1)
            V(I) = V(I)+T(K)*P(K)
 10         CONTINUE
        IF (W(I).LT.0.0E0) GO TO 20
           IF (V(I).LE.EPP*SCALE(I))GO TO 40
           IF (V(I)*THETA2.GE.W(I)) GO TO 40
                IHIT2=I
                THETA2=W(I)/V(I)
                GO TO 40
 20     CONTINUE
         IF (V(I) .GE. (-EPP)*SCALE(I)) GOTO 40
C
C WE WILL HIT THE CONSTRAINT BE GOING IN THE DIRECTION P
C
            IF (UNBNDD) GOTO 30
               IF (V(I)*THETA .GE. W(I)) GO TO 40
                   THETA=W(I)/V(I)
               IHIT=I
               GOTO  40
 30            UNBNDD = .FALSE.
C
C THIS IS THE FIRST CONSTRAINT ENCOUNTERED THAT EVEN
C IS IN THE RIGHT DIRECTION
C
               THETA = W(I)/V(I)
               IHIT=I
 40      CONTINUE
 50      CONTINUE
      JDEPS1=JDEPS+1
      IF (S .LT. JDEPS1) GOTO  90
C
C DETERMINE THE DISTANCE TO THE NEAREST SIMPLE CONSTRAINT
C
      DO  80 II = JDEPS1, S
       ET = SIGN(1.0E0, FLOAT(ISIMP(II)))
       I1 = IABS(ISIMP(II))
       WW = ET*(SIMP(II)-X(I1))
       VV = ET * P(I1)
       IF (WW.LE.0.0E0) GO TO 60
          IF (VV.LE.EPS.OR.VV*THETA2.GE.WW) GO TO 80
              THETA2=WW/VV
              IHIT2=II+M
 60    CONTINUE
       IF (VV .GE. (-EPS)) GOTO 80
       IF (UNBNDD) GOTO 70
       IF (THETA*VV .GE. WW) GO TO 80
           THETA=WW/VV
         IHIT=II+M
         GOTO 80
 70     UNBNDD = .FALSE.
        THETA = WW/VV
        IHIT=II+M
 80      CONTINUE
 90      CONTINUE
      IF (.NOT.UNBNDD)RETURN
         THETA=THETA2
         IHIT=IHIT2
          IF (IHIT2.EQ.0)IERC=7
 100  RETURN
      END
