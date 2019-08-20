      SUBROUTINE G4ETC(N, M, S, AS, AGPE, A, IA, AMAN, X, CC, RHS
     1   , W, SCALE, Q,IQ,IPTS, ISIMP, SIMP, IPTG, DONE, CTX,
     2    CNRM,IST,FIRST)
      INTEGER N
      EXTERNAL AMAN
       REAL DSTACK(1000)
       COMMON /CSTAK/DSTACK
      INTEGER M, S, AS, AGPE, IA(1), IPTS(1)
      INTEGER ISIMP(1), IPTG(1), IST,ISP1
      REAL A(1), X(N), CC(N), RHS(N), W(1), SCALE(1),CTX
      REAL CNRM,SDOT
      REAL Q(IQ, N), SIMP(1)
      LOGICAL DONE, FIRST
      REAL SNRM2,EPS
      INTEGER II, IIAS, IK, IABS, I, K
      INTEGER NMS, I1, ASP1
      REAL ET, SC, TEMP, FLOAT, R1MACH, EP1, SI, EP2
C THIS PROCEDURE OBTAINS THE GRADIENT AND THE PROJECT GRADIENT
C TO PUSH INFEASIBLE POINTS TO BE MORE FEASIBLE
      EP1=R1MACH(4)*FLOAT(N)*10.0E0
      EPS=SNRM2(N,X,1)*EP1
      IS=IST
      ASP1=AS+1
      DO  1 I = 1, N
         CC(I) = 0.0E0
   1     CONTINUE
C FIND VIOLATED GENERAL CONSTRAINTS AND ADJUST GRADIENT
 11   DONE=.TRUE.
      IF (IST.LT.0)GO TO 4
      ISP1=IST+1
      IF (M .LT. ISP1) GOTO 34
         DO  3 I = ISP1, M
            K = IPTG(I)
            IF (W(K) .LE. EPS*SCALE(K)) GOTO 2
            IF (DONE.AND..NOT.FIRST)IST=I
               DONE = .FALSE.
               SC = 1.0/SCALE(K)
               CALL AMAN(.FALSE., A, IA, N, K, RHS, TEMP)
               CALL SAXPY(N, SC, RHS, 1, CC, 1)
               IF (.NOT.FIRST)GO TO 77
   2        CONTINUE
   3        CONTINUE
C LOOK NOW AT SIMPLE CONSTRAINTS TO FIND THOSE THAT
C ARE VIOLATED
 34   IS=-AS
   4  ISP1=-IS+1
      IF (S .LT. ISP1) GOTO 77
         DO  6 I = ISP1, S
            ET = 1.0
            IF (ISIMP(I) .LT. 0) ET = -1.0
            I1 = IABS(ISIMP(I))
            EP2=EP1
            IF (SIMP(I).LT.0.0E0)EP2=-EP1
            SI=ET*SIMP(I)-EP2*SIMP(I)
            IF (SI.EQ.0.0)SI=-EPS
            IF (ET*X(I1) .GE. SI) GOTO 5
                 IF (DONE.AND..NOT.FIRST)IST=-I
               DONE = .FALSE.
               CC(I1) = CC(I1)+ET
              IF (.NOT.FIRST)GO TO 77
   5        CONTINUE
   6        CONTINUE
           IF (DONE)RETURN
           CNRM=SNRM2(N,CC,1)
           IF (CNRM.GT.EP1)GO TO 77
               FIRST=.FALSE.
               GO TO 11
 77       CONTINUE
            CTX=SDOT(N,X,1,CC,1)
C PROJECT GRADIENT
   7  DO  8 II = ASP1, N
         IK = IPTS(II)
         IIAS = II-AS
         RHS(IIAS) = CC(IK)
   8     CONTINUE
      NMS = N-AS
      CALL M5TOP(IQ,N,Q,1,NMS,1,NMS,RHS,1,RHS)
      RETURN
      END
