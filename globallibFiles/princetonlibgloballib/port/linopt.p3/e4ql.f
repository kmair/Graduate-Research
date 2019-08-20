       SUBROUTINE E4QL(A,N,AMAN,IA,KK,G,IG,DU,B,X,RHS,V,R)
       INTEGER N,IA(1),KK
       REAL A(1),G(IG,1),DU(1),X(N),RHS(N),B(N),V(1),R(1)
       REAL SDOT,SUM2,SUM,TAU,TEMP1
       EXTERNAL AMAN
C
C GET THE QR DECOMPOSITION AND GENERATE THE RIGHT HAND SIDE
C
       DO 10   I = 1,KK
          CALL AMAN (.FALSE.,A,IA,N,I,G(1,I),TEMP1)
          RHS(I)=B(I)-SDOT(N,X,1,G(1,I),1)
 10       CONTINUE
          CALL LST2D(N,N,KK,G,DU)
C
C SOLVE THE LEAST SQUARES PROBLEM
C
         DO 40 I=1,KK
            SUM=RHS(I)
            IM1=I-1
            IF (I.EQ.1) GO TO 30
            DO 20 J=1,IM1
               SUM=SUM-RHS(J)*G(J,I)
 20         CONTINUE
 30         RHS(I)=SUM/DU(I)
 40      CONTINUE
C
C PUT THE R FROM THE UPPER TRIANGULAR FORM IN R
C
        L=1
        DO 70 I=1,KK
           IF (I.EQ.1)GO TO 60
           IM1=I-1
           DO 50 J=1,IM1
              R(L)=G(J,I)
              L=L+1
 50        CONTINUE
 60        R(L)=DU(I)
           L=L+1
 70      CONTINUE
C
C FORM THE Q INTO G
C
        KKP1=KK+1
        DO 100 IB=1,KK
           I=KKP1-IB
           TAU=DU(I)*G(I,I)
           NMI=N-I
           SUM2=1.0/DU(I)
           IP1=I+1
           IF (I.EQ.N) GO TO 90
           DO 80 K=IP1,N
              G(I,K)=0.0
              SUM=SDOT(NMI,G(IP1,I),1,G(IP1,K),1)/TAU
              CALL SAXPY(NMI+1,SUM,G(I,I),1,G(I,K),1)
 80        CONTINUE
           CALL SSCAL(NMI,SUM2,G(IP1,I),1)
 90     CONTINUE
        G(I,I)=1.0+SUM2*G(I,I)
 100    CONTINUE
       DO 110 I=1,N
          NMI=N-I
          IF (I.NE.N)CALL SSWAP(NMI,G(I+1,I),1,G(I,I+1),IG)
 110   CONTINUE
         CALL M5TOP(IG,N,G,1,KK,1,N,RHS,2,RHS)
         DO 120 I=1,N
            X(I)=X(I)+RHS(I)
 120     CONTINUE
          RETURN
        END
