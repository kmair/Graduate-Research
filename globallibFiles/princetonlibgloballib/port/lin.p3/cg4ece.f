      SUBROUTINE CG4ECE(N,A,IA,ANORM,COND,IPVT,Z)
      INTEGER IA,N,IPVT(N)
      COMPLEX A(IA,N),Z(N)
      REAL ANORM,COND
      INTEGER KB,  KP1,  J, K
      INTEGER L
      REAL  GREAT, SM,    CABS1
      COMPLEX EK
      REAL S,   SCASUM, YNORM
      COMPLEX WK,WKM,T,CDOTC
      REAL R1MACH,BIG
      COMPLEX CSIGN1, Z1,Z2
      CSIGN1(Z1,Z2)=CABS1(Z1)*(Z2/CABS1(Z2))
C THIS SUBROUTINE DETERMINES A LOWER BOUND ON THE CONDITION NUMBER
C OF THE DECOMPOSED MATRIX A VIA THE ALGORITHM USED IN LINPACK
C
C
C SOLVE A(TRANSPOSE)W = E
C WHERE E IS CHOSEN TO CAUSE MAXIMUM LOCAL GROWTH
C IN THE COMPONENTS OF W
C     SOLVE CTRANS(U)*W = E
C
 1    EK = (1.0E0,0.0E0)
      BIG=(R1MACH(2))/FLOAT(N)
      DO 20 J = 1, N
         Z(J) = (0.0E0,0.0E0)
   20 CONTINUE
      DO 100 K = 1, N
         IF (CABS1(Z(K)) .NE. 0.0E0) EK = CSIGN1(EK,-Z(K))
         IF (CABS1(EK-Z(K)) .LE. CABS1(A(K,K))) GO TO 30
            S = CABS1(A(K,K))/CABS1(EK-Z(K))
            CALL CSSCAL(N,S,Z,1)
            EK = CMPLX(S,0.0E0)*EK
   30    CONTINUE
         WK = EK - Z(K)
         WKM = -EK - Z(K)
         S = CABS1(WK)
         SM = CABS1(WKM)
         IF (CABS1(A(K,K)) .EQ. 0.0E0) GO TO 40
            WK = WK/CONJG(A(K,K))
            WKM = WKM/CONJG(A(K,K))
         GO TO 50
   40    CONTINUE
            WK = (1.0E0,0.0E0)
            WKM = (1.0E0,0.0E0)
   50    CONTINUE
         KP1 = K + 1
         IF (KP1 .GT. N) GO TO 90
            DO 60 J = KP1, N
               SM = SM + CABS1(Z(J)+WKM*CONJG(A(K,J)))
               Z(J) = Z(J) + WK*CONJG(A(K,J))
               S = S + CABS1(Z(J))
   60       CONTINUE
            IF (S .GE. SM) GO TO 80
               T = WKM - WK
               WK = WKM
               DO 70 J = KP1, N
                  Z(J) = Z(J) + T*CONJG(A(K,J))
   70          CONTINUE
   80       CONTINUE
   90    CONTINUE
         Z(K) = WK
  100 CONTINUE
      S = 1.0E0/SCASUM(N,Z,1)
      CALL CSSCAL(N,S,Z,1)
C
C     SOLVE CTRANS(L)*Y = W
C
      NM1=N-1
      IF (N.EQ.1) GO TO 121
      DO 120 KB = 1, NM1
         K = N  - KB
         Z(K) = Z(K) + CDOTC(N-K,A(K+1,K),1,Z(K+1),1)
         IF (CABS1(Z(K)) .LE. BIG) GO TO 110
            S = 1.0E0/CABS1(Z(K))
            CALL CSSCAL(N,S,Z,1)
  110    CONTINUE
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
  120 CONTINUE
      S = 1.0E0/SCASUM(N,Z,1)
      CALL CSSCAL(N,S,Z,1)
C
 121  YNORM = 1.0E0
C
C     SOLVE L*V = Y
C
      IF (N.EQ.1) GO TO 141
      DO 140 K = 1, NM1
         L = IPVT(K)
         T = Z(L)
         Z(L) = Z(K)
         Z(K) = T
         CALL CAXPY(N-K,T,A(K+1,K),1,Z(K+1),1)
         IF (CABS1(Z(K)) .LE. BIG) GO TO 130
            S = 1.0E0/CABS1(Z(K))
            CALL CSSCAL(N,S,Z,1)
            YNORM = S*YNORM
  130    CONTINUE
  140 CONTINUE
      S = 1.0E0/SCASUM(N,Z,1)
      CALL CSSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
C     SOLVE  U*Z = V
C
 141  DO 160 KB = 1, N
         K = N + 1 - KB
         IF (CABS1(Z(K)) .LE. CABS1(A(K,K))) GO TO 150
            S = CABS1(A(K,K))/CABS1(Z(K))
            CALL CSSCAL(N,S,Z,1)
            YNORM = S*YNORM
  150    CONTINUE
         IF (CABS1(A(K,K)) .NE. 0.0E0) Z(K) = Z(K)/A(K,K)
         IF (CABS1(A(K,K)) .EQ. 0.0E0) Z(K) = (1.0E0,0.0E0)
         T = -Z(K)
         CALL CAXPY(K-1,T,A(1,K),1,Z(1),1)
  160 CONTINUE
C     MAKE ZNORM = 1.0
      S = 1.0E0/SCASUM(N,Z,1)
      CALL CSSCAL(N,S,Z,1)
      YNORM = S*YNORM
C
C
C SET COND = ESTIMATE OF THE CONDITION NUMBER OF A
C
        GREAT=R1MACH(2)
        IF (YNORM.GT.1.0) GO TO 170
        IF (ANORM.NE.0.0.AND.ANORM.LE.YNORM*GREAT) GO TO 170
            COND=GREAT
            RETURN
 170    COND=ANORM/YNORM
        RETURN
        END
