        SUBROUTINE CHEFBS(N,A,B,IB,NB,INTER)
C
C THIS SUBROUTINE SOLVES THE LINEAR SYSTEM AX=B WHERE A IS A COMPLEX
C SYMMETRIC MATRIX OF ORDER N WHOSE DECOMPOSITION HAS BEEN COMPUTED
C BY THE SUBROUTINE CHEMD AND LEFT IN THE VECTOR A,  AND B IS AN N
C VECTOR. THE SOLUTION X IS RETURNED IN THE VECTOR B. THE VECTOR
C INTER OF LENGTH N IS GENERATED BY CHEMD AND CONTAINS INFORMATION
C ABOUT THE PERMUTATIONS PERFORMED ON THE A MATRIX IN CHEMD.
C
C INPUT PARAMETERS
C   N    ORDER OF THE PROBLEM
C   A    THE COMPLEX VECTOR CONTAINING THE DECOMPOSITION COMPUTED
C        BY CHEMD
C  B     THE RIGHT HAND SIDE, WILL BE DESTROYED ON INPUT
C  IB    LEADING DIMENSION OF B
C  NB    NUMBER OF COLUMNS OF B
C INTER RECORD OF INTERCHANGES PERFORMED BY CHEMD
C OUTPUT PARAMETERS
C  B      THE SOLUTION MATRIX
C ERROR CONDITIONS
C   1       N<1    FATAL
C   2       IB<N   FATAL
C   3       NB<1   FATAL
C 10+K     SINGULAR MATRIX RANK K   RECOVERABLE
        COMPLEX A(1),B(IB,NB),TEMP,SAVE,DENOM
        INTEGER INTER(N)
        CALL ENTER(1)
C/6S
C       IF (N.LT.1) CALL SETERR(13HCHEFBS-N.LT.1,13,1,2)
C       IF (IB.LT.N) CALL SETERR(14HCHEFBS-IB.LT.N,14,2,2)
C       IF (NB.LT.1) CALL SETERR(14HCHEFBS-NB.LT.1,14,3,2)
C/7S
        IF (N.LT.1) CALL SETERR('CHEFBS-N.LT.1',13,1,2)
        IF (IB.LT.N) CALL SETERR('CHEFBS-IB.LT.N',14,2,2)
        IF (NB.LT.1) CALL SETERR('CHEFBS-NB.LT.1',14,3,2)
C/
C
C SOLVE MDY=B AND STORE Y IN THE VECTOR B
C SOLVE MC=B AND STOREC IN B
C
         DO 400 ICOL=1,NB
        I=1
        JI=1
 10     IF (I.GE.N) GO TO 200
        ICH=INTER(I)
        SAVE=B(ICH,ICOL)
        IP1=I+1
        IF(INTER(IP1).LT.0) GO TO 30
        B(ICH,ICOL)=B(I,ICOL)
        IF (CABS1(A(JI)).EQ.0.0) GO TO 300
 15     B(I,ICOL)=SAVE/A(JI)
        JI=JI+1
        DO 20 J=IP1,N
           B(J,ICOL)=B(J,ICOL)+CONJG(A(JI))*SAVE
           JI=JI+1
 20     CONTINUE
        I=IP1
        GO TO 10
 30     TEMP=B(I,ICOL)
        B(ICH,ICOL)=B(IP1,ICOL)
 130    JIP1=JI+1
        JI1NI=JIP1+N-I
        DENOM=A(JI1NI)*A(JI)/A(JIP1)-CONJG(A(JIP1))
        B(IP1,ICOL)=(SAVE*A(JI)/A(JIP1)-TEMP)/DENOM
        B(I,ICOL)=(SAVE-B(IP1,ICOL)*A(JI1NI))/A(JIP1)
        IF (I+2.GT.N) GO TO 41
        IP2=I+2
        NMI=N-I
        JI=JI+2
        DO 40 J=IP2,N
           JIPNMI=JI+NMI
           B(J,ICOL)=B(J,ICOL)+CONJG(A(JI))*TEMP+CONJG(A(JIPNMI))*SAVE
           JI=JI+1
 40     CONTINUE
 41     I=I+2
        JI=JI+NMI
        GO TO 10
 200    IF (I.NE.N) GO TO 202
        IF (CABS1(A(JI)).EQ.0.0) GO TO 300
 201    B(I,ICOL)=B(I,ICOL)/A(JI)
C
C NOW SOLVE M(TRANSPOSE) X= Y FOR X,WHERE Y IS STORED
C IN THE VECTOR B AND STORE X IN B
C
 202    JK=(N*(N+1))/2-1
        IF (I.GT.N) JK=JK-3
        I=I-1
 210    IF(I.LE.0) GO TO 400
        KEND=1
        IF(INTER(I).LE.0) KEND =2
        IP1=I+1
        IF(IP1.GT.N) GO TO 251
        DO 250 KK=1,KEND
           K=IP1-KK
           SAVE=B(K,ICOL)
           DO 240 J=IP1,N
              SAVE=SAVE+A(JK)*B(J,ICOL)
              JK=JK+1
 240       CONTINUE
           B(K,ICOL)=SAVE
           JK=JK-2*(N-K)-1
 250    CONTINUE
        JK=JK-1
 251    SAVE=B(I,ICOL)
        ITEMP=IP1-KEND
        ICH=INTER(ITEMP)
        B(I,ICOL)=B(ICH,ICOL)
        B(ICH,ICOL)=SAVE
        I=I-KEND
        GO TO 210
 400     CONTINUE
         GO TO 301
C/6S
C300   CALL SETERR(22HCHEFBS-SINGULAR MATRIX,22,9+I,1)
C/7S
 300   CALL SETERR('CHEFBS-SINGULAR MATRIX',22,9+I,1)
C/
 301   CALL LEAVE
       RETURN
       END