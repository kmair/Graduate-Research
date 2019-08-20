        SUBROUTINE  CSYFBS(N,A,B,IB,NB,INTER)
C
C THIS SUBROUTINE SOLVES THE LINEAR CSYSTEM AX=B WHERE A IS A D.P.
C SYMMETRIC MATRIX OF ORDER N WHOSE DECOMPOSITION HAS BEEN COMPUTED
C BY THE SUBROUTINE  CSYMD AND LEFT IN THE VECTOR A
C INPUT PARAMETERS
C     N     THE ORDER OF THE PROBLEM
C     A     A D.P. VECTOR CONTAINING THE DECOMPOSITION COMPUTED
C           BY  CSYMD
C     B     THE RIGHT HAND SIDE,WILL BE DESTROYED ON OUTPUT
C     INTER A RECORD OF THE PERMUTATIONS PERFORMED BY  CSYMD
C OUTPUT PARAMETERS
C     B     THE SOLUTION TO THE PROBLEM
C ERROR CONDITIONS
C     1    N IS LESS THAN 1    FATAL
C     2    IB LESS THAN N      FATAL
C     3    NB LESS THAN 1      FATAL
C    10+K       SINGULAR MATRIX  OF RANK K RECOVERABLE
        COMPLEX A(N),B(IB,NB),TEMP,SAVE,DENOM
        INTEGER INTER(N)
        CALL ENTER(1)
C/6S
C       IF (N.LT.1)CALL SETERR(13HCSYFBS-N.LT.1,13,1,2)
C       IF (IB.LT.N) CALL SETERR(14HCSYFBS-IB.LT.N,14,2,2)
C       IF (NB.LT.1) CALL SETERR(14HCSYFBS-NB.LT.1,14,3,2)
C/7S
        IF (N.LT.1)CALL SETERR('CSYFBS-N.LT.1',13,1,2)
        IF (IB.LT.N) CALL SETERR('CSYFBS-IB.LT.N',14,2,2)
        IF (NB.LT.1) CALL SETERR('CSYFBS-NB.LT.1',14,3,2)
C/
C
C SOLVE MDY=B AND STORE Y IN THE VECTOR B
C
        DO 400 ICOL=1,NB
        I=1
        JI=1
 10     IF (I.GE.N) GO TO 200
        ICH=INTER(I)
        SAVE=B(ICH,ICOL)
        IP1=I+1
        IF(INTER(IP1).LT.0) GO TO 30
C HANDLE A 1 X 1 PIVOT
        B(ICH,ICOL)=B(I,ICOL)
         IF(CABS1(A(JI)).EQ.0.E0) GO TO 300
 15     B(I,ICOL)=SAVE/A(JI)
        JI=JI+1
        DO 20 J=IP1,N
           B(J,ICOL)=B(J,ICOL)+A(JI)*SAVE
           JI=JI+1
 20     CONTINUE
        I=IP1
        GO TO 10
C HANDLE A 2 X 2 PIVOT
 30     TEMP=B(I,ICOL)
        B(ICH,ICOL)=B(IP1,ICOL)
 130    JIP1=JI+1
        NMI=N-I
        JNMI=JIP1+NMI
        DENOM=A(JNMI)*A(JI)/A(JIP1)-A(JIP1)
        B(IP1,ICOL)=(SAVE*A(JI)/A(JIP1)-TEMP)/DENOM
        B(I,ICOL)=(SAVE-B(IP1,ICOL)*A(JNMI))/A(JIP1)
        IF (I+2.GT.N) GO TO 41
        IP2=I+2
        JI=JI+2
        DO 40 J=IP2,N
           JIPNMI=JI+NMI
           B(J,ICOL)=B(J,ICOL)+A(JI)*TEMP+A(JIPNMI)*SAVE
           JI=JI+1
 40     CONTINUE
 41     I=I+2
        JI=JI+NMI
        GO TO 10
 200    IF (I.NE.N) GO TO 202
        IF (CABS1(A(JI)).EQ.0.E0) GO TO 300
 201    B(I,ICOL)=B(I,ICOL)/A(JI)
        JK=N*(N+1)/2-1
        I=N-1
        GO TO 210
C
C NOW SOLVE M(TRANSPOSE) X= Y FOR X,WHERE Y IS STORED
C IN THE VECTOR B AND STORE X IN B
C
 202    JK=(N*(N+1))/2-4
        I=N-2
 210    IF(I.LE.0) GO TO 400
        KEND=1
        IF(INTER(I).LE.0) KEND =2
        IP1=I+1
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
C INTERCHANGE THE ELEMENTS OF THE SOLUTION
        IKEND=IP1-KEND
        ICH=INTER(IKEND)
        B(I,ICOL)=B(ICH,ICOL)
        B(ICH,ICOL)=SAVE
        I=I-KEND
        GO TO 210
 400     CONTINUE
       GO TO 301
C/6S
C300   CALL SETERR(22HCSYFBS-SINGULAR MATRIX,22,9+I,1)
C/7S
 300   CALL SETERR('CSYFBS-SINGULAR MATRIX',22,9+I,1)
C/
 301   CALL LEAVE
       RETURN
        END
