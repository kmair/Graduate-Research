      SUBROUTINE B4DCE(N, ML, G, IG, GL, IGL, INTER, COND, MU,
     1   ANORM, Z)
      INTEGER IG, N, IGL
      INTEGER ML, INTER(N), MU
      REAL G(IG, N), GL(IGL, N), COND, ANORM, Z(N)
      INTEGER IB, KB, JEND, JJ, IEND, KP1
      INTEGER NP1, I, J, K, L, IMK
      INTEGER M1, MIN0
      REAL FLOAT
      REAL  EK, ABS, SM, WK, ZK
      REAL T, S, SIGN, WKM, SASUM, SUM
      REAL BOUND, R1MACH, SQRT, YNORM
      REAL ARGE
      M1 = ML-1
      BOUND = SQRT(R1MACH(2))/(FLOAT(N))
C SOLVE U(TRANSPOSE)Z=E,WHERE ELEMENTS OF E ARE +1 OR
C-1
      EK = 1.E0
      M1=ML-1
      MUM1=MU-1
      DO  1 I = 1, N
         Z(I) = 0.E0
   1     CONTINUE
      DO  9 K = 1, N
         ZK = Z(K)
         IF (ZK .NE. 0.E0) EK = SIGN(EK,- ZK)
            JEND = MIN0(K+MUM1, N)
         T = EK-ZK
         IF (ABS(G(1, K)) .GE. ABS(T)) GOTO 4
            S=ABS(G(1,K))/ABS(T)
            DO  3 I = 1, JEND
               Z(I) = S*Z(I)
   3           CONTINUE
            EK = S*EK
 4       WK=EK-Z(K)
         WKM=-EK-Z(K)
         S=ABS(WK)
         SM=ABS(WKM)
         IF (ABS(G(1,K)).EQ.0.0) GO TO 101
             WK=WK/G(1,K)
             WKM=WKM/G(1,K)
             GO TO 102
 101         WK=1.0
            WKM=1.0
 102     CONTINUE
            KP1 = K+1
            IF (KP1.GT.JEND) GO TO 103
            JJ = 2
            DO  5 J = KP1, JEND
                SM=SM+ABS(Z(J)+WKM*G(JJ,K))
               Z(J) = Z(J)+G(JJ,K)*WK
               S = S+ABS(Z(J))
               JJ = JJ+1
   5           CONTINUE
 103        CONTINUE
            IF (S .GE. SM) GOTO 7
               T = WKM-WK
               WK=WKM
               IF (KP1.GT.JEND) GO TO 7
               JJ = 2
               DO  6 J = KP1, JEND
                  Z(J) = Z(J)+T*G(JJ, K)
                  JJ = JJ+1
   6              CONTINUE
   7        CONTINUE
            Z(K)=WK
   9     CONTINUE
C SOLVE L(TRANSPOSE)Y=W
      S=1.0/SASUM(N,Z,1)
      CALL SSCAL(N,S,Z,1)
      NP1 = N+1
      IF(ML.EQ.1) GO TO 135
      DO  13 KB = 1, N
         K = NP1-KB
         IF (K .GE. N) GOTO 13
            SUM = 0.E0
            IEND = MIN0(KB-1, M1)
            J = K+1
            DO  10 I = 1, IEND
               SUM = SUM+GL(I, K)*Z(J)
               J = J+1
  10           CONTINUE
            Z(K) = Z(K)+SUM
           IF (ABS(Z(K)).LT.BOUND) GO TO 11
              S=1.0/ABS(Z(K))
              CALL SSCAL(N,S,Z,1)
 11       CONTINUE
         L = INTER(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
  13     CONTINUE
 135   S=1.E0/SASUM(N,Z,1)
       CALL SSCAL(N,S,Z,1)
       YNORM=1.E0
C SOLVELW=Y
 14    IF(ML.EQ.1.OR.N.EQ.1) GO TO 18
      NM1=N-1
      DO 16 K=1,NM1
         I=INTER(K)
         KP1=K+1
         T=Z(I)
         Z(I)=Z(K)
         Z(K)=T
         IEND=MIN0(M1+K,N)
         IMK=0
         DO 15 II=KP1,IEND
            IMK=IMK+1
            Z(II)=Z(II)+GL(IMK,K)*T
 15      CONTINUE
         IF (ABS(Z(K)).LT.BOUND) GO TO 16
            S=1.E0/ABS(Z(K))
            CALL SSCAL(N,S,Z,1)
            YNORM=YNORM*S
 16      CONTINUE
  17     CONTINUE
      S=1.0/SASUM(N,Z,1)
       IF (S.GT.1.E0) GO TO 18
         CALL SSCAL(N, S, Z, 1)
         YNORM = YNORM*S
C SOLVE UZ=W
  18  L = 1
      DO  23 KB = 1, N
         K = NP1-KB
         SUM = Z(K)
         IF (L .LE. 1) GOTO 20
            IB = K
            DO  19 I = 2, L
               IB = IB+1
               SUM = SUM-G(I, K)*Z(IB)
  19           CONTINUE
  20     L = MIN0(L+1, MU)
         Z(K)=SUM
         IF (ABS(G(1, K)) .GE. ABS(SUM)) GOTO 210
  21        T = 0.E0
            IF (ABS(SUM) .NE. 0.E0) T = G(1, K)/SUM
            S = ABS(T)
            CALL SSCAL(N, S, Z, 1)
            YNORM = YNORM*S
 210     IF (G(1,K).NE.0.E0) Z(K)=Z(K)/G(1,K)
         IF (G(1,K).EQ.0.E0)Z(K)=1.E0
  22     CONTINUE
  23     CONTINUE
       S=1.0/SASUM(N,Z,1)
       YNORM=S*YNORM
      ARGE=R1MACH(2)
      IF (YNORM.GT.1.E0) GO TO 50
      IF (ANORM.LE.YNORM*ARGE) GO TO 50
         COND=ARGE
         RETURN
 50   COND=ANORM/YNORM
      RETURN
      END