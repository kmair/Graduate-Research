      SUBROUTINE CB4PCE(N,ML,G,IG,ANORM,COND,Z)
      INTEGER N,ML,IG
      COMPLEX G(IG,N), Z(N)
      REAL ANORM,COND
      REAL YNORM,SCASUM,S,SM
      COMPLEX T,CDOTU,WKM,WK,EK,CSIGN1,Z1,Z2
      INTEGER ML1,J,K,KP1,JEND,JK,NUMELE,NM1
      REAL R1MACH,GREAT,BIG,BIG1
      CSIGN1(Z1,Z2)=CABS1(Z1)*(Z2/CABS1(Z2))
C SOLVE A(TRANSPOSE)W = E
C WHERE E IS CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH
C IN THE COMPONENTS OF W
      ML1 = ML - 1
      BIG1=SQRT(R1MACH(2))/FLOAT(N)
      BIG=BIG1
      IF (ANORM.GT.1.0)BIG=BIG1/ANORM
      IF (BIG.LT.1.0)BIG=1.0
      NM1=N-1
      EK = CMPLX(1.0,0.0)
      DO  2 J = 1, N
         Z(J) = (0.0,0.0)
  2   CONTINUE
      DO 30 K=1,N
        IF (CABS1(Z(K)) .NE. 0.0) EK=CSIGN1(EK,-Z(K))
        IF (CABS1(EK-Z(K)) .LE. REAL(G(1,K))) GO TO 20
           S=REAL(G(1,K))/CABS1(EK-Z(K))
           CALL CSSCAL(N,S,Z,1)
           EK=CMPLX(S,0.0E0)*EK
 20     CONTINUE
        WK=EK - Z(K)
        WKM=-EK-Z(K)
        S = CABS1(WK)
        SM = CABS1(WKM)
        KP1=K+1
        JEND= MIN0(K+ML1, N)
        IF (KP1.GT.JEND) GO TO 28
        JK = 2
        DO 24 J=KP1,JEND
          SM = SM + CABS1(Z(J) + WKM*CONJG(G(JK,K)))
          Z(J) = Z(J) + WK * CONJG(G(JK,K))
          S = S + CABS1(Z(J))
          JK = JK + 1
 24    CONTINUE
       IF ( S .GE. SM) GO TO 28
          T= WKM - WK
          WK = WKM
          JK = 2
          DO 26 J=KP1,JEND
             Z(J) = Z(J) + T*CONJG(G(JK,K))
             JK = JK + 1
 26       CONTINUE
 28    CONTINUE
       Z(K)=(1.0,0.0)
       IF(REAL(G(1,K)).NE.0.0) Z(K) = WK/G(1,K)
 30    CONTINUE
       S= 1.0/SCASUM(N,Z,1)
       CALL CSSCAL(N,S,Z,1)
C
C SOLVE W=L(TRANSPOSE)Y FOR Y
C
      IF (ML.EQ.1) GO TO 100
      DO  12 KB = 1, N
         K = N+1-KB
         NUMELE = MIN0(ML1,N-K)
         IF(NUMELE.GT.0)Z(K) = Z(K) - CDOTU(NUMELE,G(2,K),1,Z(K+1),1)
         S = CABS1(Z(K))
         IF (S .LE. BIG) GOTO 11
         S = 1.0/S
         CALL CSSCAL(N,S,Z,1)
 11      CONTINUE
 12      CONTINUE
         S=1.0/SCASUM(N,Z,1)
         CALL CSSCAL(N,S,Z,1)
 100  YNORM = 1.0
C
C   SOLVE LW=Y FOR W
C
       IF (ML.EQ.1) GO TO 16
       NP1=N+1
       DO 40 K=1,NM1
          T=-Z(K)
          IF (CABS1(T).LT.BIG) GO TO 29
            S=1.0/CABS1(T)
            YNORM=YNORM*S
            CALL CSSCAL(N,S,Z,1)
  29      CONTINUE
          IEND=MIN0(NP1-K,ML)
          L=K+1
          IF(IEND.LT.2) GO TO 40
          DO 31 I=2,IEND
             Z(L)=Z(L)+T*CONJG(G(I,K))
             L=L+1
  31      CONTINUE
  40   CONTINUE
       S = 1.0/SCASUM(N,Z,1)
       IF (S.GT.1.0) GO TO 16
       CALL CSSCAL(N,S,Z,1)
       YNORM = YNORM*S
C
C   SOLVEL(TRANSPOSE)* Z = W
C
 16    DO  50 KB = 1, N
         K = N+1-KB
         IF (REAL(G(1,K)).GE.CABS1(Z(K))) GO TO 48
              S=CABS1(G(1,K)/Z(K))
              CALL CSSCAL(N,S,Z,1)
              YNORM=YNORM*S
 48       T=(1.0,0.0)
         IF (REAL(G(1,K)).NE.0.0)T=Z(K)/G(1,K)
         NUMELE = MIN0(ML1,N-K)
         Z(K)=T
         IF (NUMELE.GT.0)Z(K)=T-CDOTU(NUMELE,G(2,K),1,Z(K+1),1)
 50      CONTINUE
C
C    MAKE ZNORM = 1.0
C
       S= 1.0/SCASUM(N,Z,1)
       CALL CSSCAL(N,S,Z,1)
       YNORM = YNORM*S
C
C    CALCULATE FINAL VALUE OF CONDITION NUMBER
C
       GREAT=R1MACH(2)
       IF (YNORM.GT.1.0) GO TO 60
       IF (ANORM.LE.YNORM*GREAT) GO TO 60
       COND=GREAT
       RETURN
 60    COND=ANORM/YNORM
       RETURN
       END
