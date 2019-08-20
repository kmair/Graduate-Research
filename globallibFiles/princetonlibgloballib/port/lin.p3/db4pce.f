      SUBROUTINE DB4PCE(N,ML,G,IG,ANORM,COND,Z)
      INTEGER N,ML,IG
      DOUBLE PRECISION G(IG,N), Z(N), ANORM, COND
      DOUBLE PRECISION YNORM,S,T,DASUM,DDOT,SM,WKM,WK,EK
      INTEGER ML1,J,K,KP1,JEND,JK,NUMELE,NM1
      DOUBLE PRECISION D1MACH,GREAT,BIG,BIG1,DSQRT
C SOLVE A(TRANSPOSE)W = E
C WHERE E IS CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH
C IN THE COMPONENTS OF W
      ML1 = ML - 1
      BIG1=DSQRT(D1MACH(2))/FLOAT(N)
      BIG=BIG1
      IF (ANORM.GT.1.D0)BIG=BIG1/ANORM
      IF (BIG.LT.1.D0)BIG=1.D0
      NM1=N-1
      EK = 1.D0
      DO  2 J = 1, N
         Z(J) = 0.D0
  2   CONTINUE
      DO 30 K=1,N
        IF (DABS(Z(K)) .NE. 0.D0) EK=DSIGN(EK,-Z(K))
        IF (DABS(EK-Z(K)) .LE. G(1,K)) GO TO 20
           S=G(1,K)/DABS(EK-Z(K))
           CALL DSCAL(N,S,Z,1)
           EK=S*EK
 20     CONTINUE
        WK=EK - Z(K)
        WKM=-EK-Z(K)
        S = DABS(WK)
        SM = DABS(WKM)
        KP1=K+1
        JEND= MIN0(K+ML1, N)
        IF (KP1.GT.JEND) GO TO 28
        JK = 2
        DO 24 J=KP1,JEND
          SM = SM + DABS(Z(J) + WKM*G(JK,K))
          Z(J) = Z(J) + WK * G(JK,K)
          S = S + DABS(Z(J))
          JK = JK + 1
 24    CONTINUE
       IF ( S .GE. SM) GO TO 28
          T= WKM - WK
          WK = WKM
          JK = 2
          DO 26 J=KP1,JEND
             Z(J) = Z(J) + T*G(JK,K)
             JK = JK + 1
 26       CONTINUE
 28    CONTINUE
       Z(K)=1.D0
       IF(G(1,K).NE.0.D0) Z(K) = WK/G(1,K)
 30    CONTINUE
       S= 1.D0/DASUM(N,Z,1)
       CALL DSCAL(N,S,Z,1)
C
C SOLVE W=L(TRANSPOSE)Y FOR Y
C
      IF (ML.EQ.1) GO TO 100
      DO  12 KB = 1, N
         K = N+1-KB
         NUMELE = MIN0(ML1,N-K)
         IF(NUMELE.GT.0)Z(K) = Z(K) - DDOT(NUMELE,G(2,K),1,Z(K+1),1)
         S = DABS(Z(K))
         IF (S .LE. BIG) GOTO 11
         S = 1.D0/S
         CALL DSCAL(N,S,Z,1)
 11      CONTINUE
 12      CONTINUE
         S=1.D0/DASUM(N,Z,1)
         CALL DSCAL(N,S,Z,1)
 100  YNORM = 1.D0
C
C   SOLVE LW=Y FOR W
C
       IF (ML.EQ.1) GO TO 16
       DO 40 K=1,NM1
          T=-Z(K)
          IF (DABS(T).LT.BIG) GO TO 29
            S=1.D0/DABS(T)
            YNORM=YNORM*S
            CALL DSCAL(N,S,Z,1)
  29      CONTINUE
          NUMELE=MIN0(ML1,N-K)
          CALL DAXPY(NUMELE,T,G(2,K),1,Z(K+1),1)
  40   CONTINUE
       S = 1.D0/DASUM(N,Z,1)
       IF (S.GT.1.D0) GO TO 16
       CALL DSCAL(N,S,Z,1)
       YNORM = YNORM*S
C
C   SOLVEL(TRANSPOSE)* Z = W
C
 16    DO  50 KB = 1, N
         K = N+1-KB
         IF (G(1,K).GE.DABS(Z(K))) GO TO 48
              S=DABS(G(1,K)/Z(K))
              CALL DSCAL(N,S,Z,1)
              YNORM=YNORM*S
 48       T=1.D0
         IF (G(1,K).NE.0.D0)T=Z(K)/G(1,K)
         NUMELE = MIN0(ML1,N-K)
         Z(K)=T
         IF (NUMELE.GT.0)Z(K)=Z(K)-DDOT(NUMELE,G(2,K),1,Z(K+1),1)
 50      CONTINUE
C
C    MAKE ZNORM = 1.D0
C
       S= 1.D0/DASUM(N,Z,1)
       CALL DSCAL(N,S,Z,1)
       YNORM = YNORM*S
C
C    CALCULATE FINAL VALUE OF CONDITION NUMBER
C
       GREAT=D1MACH(2)
       IF (YNORM.GT.1.D0) GO TO 60
       IF (ANORM.LE.YNORM*GREAT) GO TO 60
       COND=GREAT
       RETURN
 60    COND=ANORM/YNORM
       RETURN
       END
