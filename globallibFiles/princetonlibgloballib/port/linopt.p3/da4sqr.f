      SUBROUTINE DA4SQR(K, M, N, Q, R, INDEX, TEMP, RHS)
C
C THIS SUBROUTINE UPDATES THE QR DECOMPOSITION WHEN
C THE INDEXTH ROW IS DELETED FROM THE MATRIX
      INTEGER M, N, INDEX, K
      DOUBLE PRECISION TEMP(1), Q(K, 1), R( 1), RHS(K)
      INTEGER I, J
      DOUBLE PRECISION BETA, ALPHA, F,FF, DDOT, X,Y
C/6S
C     IF (INDEX .LE. 0 .OR. M+1 .LT. INDEX) CALL SETERR(
C    1   22HDA4SQR - INVALID INDEX, 21, 2, 2)
C/7S
      IF (INDEX .LE. 0 .OR. M+1 .LT. INDEX) CALL SETERR(
     1   'DA4SQR - INVALID INDEX', 21, 2, 2)
C/
       CALL DCOPY (M,Q(1,INDEX),1,TEMP,1)
C
C INTERCHANGE LAST COLUMN AND COLUMN INDEX
C
       IF (INDEX.NE.M)CALL DCOPY(M,Q(1,M),1,Q(1,INDEX),1)
         MMN=M-N
C
C
         KK=M
         IF (INDEX.GT.N)KK=INDEX
         X=RHS(KK)
         MM1=M-1
         NP1=N+1
      IF (M.LE.N+1) GO TO 20
         KKMN=KK-N
         CALL DH4HG(MMN,TEMP(N+1),ALPHA,BETA,KKMN)
         DO 10 I=1,MM1
            F=DDOT(MMN,TEMP(NP1),1,Q(NP1,I),1)/ALPHA
            CALL DAXPY(MMN,F,TEMP(NP1),1,Q(NP1,I),1)
 10      CONTINUE
         FF=DDOT(MMN,TEMP(NP1),1,RHS(NP1),1)/ALPHA
         CALL DAXPY(MMN,FF,TEMP(NP1),1,RHS(NP1),1)
         TEMP(KK)=BETA
         X=RHS(KK)
         IF (INDEX.LE.N)GO TO 20
            RHS(INDEX)=RHS(M)
            CALL DSWAP(M-1,Q(INDEX,1),K,Q(M,1),K)
 20      CONTINUE
         I=N
C
C ELIMINATE UNWANTED ELEMENTS
C
             IS=(N*(N+1))/2
         GOTO  40
 30      I = I-1
 40      IF (I .LT. 1) GOTO  70
         IIJ=I
         IF (I.LE.N)Q(I,M)=0.0D0
         IF (I.NE.INDEX)GO TO 60
          CALL DSWAP(M-1,Q(I,1),K,Q(M,1),K)
C SWAP RHS
         Y=RHS(I)
         RHS(I)=X
         X=Y
         IIJ=M
         KK=INDEX
C SWAP R
         IF (I.GT.N) GO TO 60
           IS2=IS
           DO 50 J=I,N
              Y=Q(J,M)
              Q(J,M)=R(IS2)
              R(IS2)=Y
              IS2=IS2+J
 50        CONTINUE
 60    CONTINUE
            CALL DROTG(TEMP(KK), TEMP(IIJ),ALPHA,BETA)
            CALL DROT (M-1,Q(M, 1),K,Q(I,1),K,ALPHA,BETA)
            Y=RHS(I)
            RHS(I)=-BETA*X+ALPHA*Y
            X=ALPHA*X+BETA*Y
            IF (I.GT.N)GO TO 30
            NMI=N-I
            CALL DS4ROT(NMI+1,Q(I,M),-1,R(IS),I,ALPHA,BETA)
            IS=IS-I
            GOTO  30
 70       M=M-1
      RETURN
      END
