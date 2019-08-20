        SUBROUTINE CHEMD(N,A,INTER,EPS)
C
C MNEMONIC -HERMETIAN MDM(TRANSPOSE) DECOMPOSITION
C GIVEN A COMPLEX HERMETIAN MATRIX OF ORDER N STORED IN
C THE VECTOR A BY COLUMNS, THIS SUBROUTINE DETERMINES
C ITS DECOMPOSITION INTO PMDM(TRANSPOSE)P(TRANSPOSE) WHERE
C P IS A PERMUTATION MATRRIX,M IS A UNIT LOWER TRIANGULAR
C MATRIX, AD D IS A BLOCK DIAGONAL MATRIX WITH BLOCKS OF ORDER
C 1 AND 2 WHERE D(I+1,I) IS NONZERO, WHENEVER M(I+1,I) IS 0.
C
C INPUT PARAMETERS
C  N    THE ORDER OF THE PROBLEM
C  A    VECTOR OF LENGTH N*(N+1)/2 CONTAINING THE MATRIX
C       STORED IN THE ORDER 1,1; 2,1; 3,1; ETC.
C       THE MATRIX IS DESTROYED BY THE SUBROUTINE
C  EPS    IF 1 X 1 PIVOT IS LESS THAN EPS IN ABSOLUTE VALUE
C         THE MATRIX IS CONSIDERED SINGULAR
C OUTPUT PARAMETERS
C  A    M AND D OF THE DECOMPOSTION
C  INTER AN INTEGER VECTOR OF LENGTH N RECORDING THE INTERCHANGES
C       GENERATED
C ERROR CONDITIONS
C   1      N<1      FATAL
C 10 +K    SINGULAR MATRIX OF RANK K     RECOVERABLE
C
        REAL EPS
        COMPLEX A(1),TEMP,SAVE,DENOM,AII,AIP1,AIP1I
        REAL ALPHA,ALFLAM,AAII,LAMBDA
        INTEGER INTER(N)
C/6S
C       IF (N.LT.1) CALL SETERR(12HCHEMD-N.LT.1,12,1,2)
C/7S
        IF (N.LT.1) CALL SETERR('CHEMD-N.LT.1',12,1,2)
C/
        INTER(N)=N
        ALPHA=(1.0+SQRT(17.0))/8.0
        I=1
        II=1
        NI=N
       CALL ENTER(1)
       IERR=0
 10     IF(I.GE.N) GO TO 100
        AAII=CABS1(A(II))
        INTER(I)=I
C
C FIND THE LARGEST OFF DIAGONAL ELEMENT IN THE ITH COLUMN
C
        IIP1=II+1
        LAMBDA=CABS1(A(IIP1))
        IP1=I+1
        IP2=I+2
        NMI=N-I
        JI=IIP1
        IF(IP2.GT.N) GO TO 16
        DO 15 KI=IIP1,NI
           IF(CABS1(A(KI)).LE.LAMBDA) GO TO 15
           LAMBDA=CABS1(A(KI))
           JI=KI
 15     CONTINUE
 16     ALFLAM=ALPHA*LAMBDA
        IF (AAII.GE.ALFLAM) GO TO 60
C
C FIND THE LARGEST OFFDIAGONAL ELEMENT IN THE JTH COLUMN
C
        SGMA=LAMBDA
        J=I+JI-II
        NMJP1=N-J+1
        NJ=(J*(N+NMJP1))/2
        JJ=NJ-(N-J)
        JJP1=JJ+1
        IF(JJP1.GT.NJ) GO TO 21
        DO 20 KJ=JJP1,NJ
           IF(CABS1(A(KJ)).GT.SGMA) SGMA =CABS1(A(KJ))
 20     CONTINUE
 21     NMIM1=NMI-1
        IF(NMJP1.GT.NMIM1) GO TO 26
        KJ=JJ
        DO 25 K=NMJP1,NMIM1
           KJ=KJ-K
           IF(CABS1(A(KJ)).GT.SGMA) SGMA=CABS1(A(KJ))
 25     CONTINUE
 26     IF (AAII.GE.ALFLAM*(LAMBDA/SGMA)) GO TO 60
        IF (CABS1(A(JJ)).GE.ALPHA*SGMA) GO TO 55
C
C PERFORM A 2 BY 2 PIVOT STEP
C
        IP1IP1=IIP1+NMI
        INTER(I)=J
        IF(IP2.GT.N) GO TO 51
        IF(J.EQ.IP1) GO TO 30
        CALL I4HP1(A,N,J,IP1,IP1IP1,JJ)
        TEMP=A(JI)
        A(JI)=A(IIP1)
        A(IIP1)=TEMP
 30     AIP1I=A(IIP1)
        AII=A(II)/AIP1I
        AIP1=A(IP1IP1)
        DENOM=AII*AIP1-CONJG(AIP1I)
        JB=IIP1+1
        II=IIP1+2*NMI
        L=II
        DO 50 JI=JB,NI
           JIPNMI=JI+NMI
           TEMP=(CONJG(A(JI))-AII*CONJG(A(JIPNMI)))/DENOM
           SAVE=-(CONJG(A(JIPNMI))+TEMP*AIP1)/AIP1I
           DO 40 KJ=JI,NI
              KJ2=KJ+NMI
              A(L)=A(L)+A(KJ)*SAVE+A(KJ2)*TEMP
              L=L+1
 40        CONTINUE
           A(JI)=SAVE
           A(JIPNMI)=TEMP
 50     CONTINUE
 51     INTER(IP1)=-1
        I=IP2
        NI=NI+2*NMI-1
        GO TO 10
C
C INTERCHANGE THE ITH AND  JTH ROWS AND COLUMNS
C
 55     INTER(I)=J
        CALL I4HP1(A,N,J,I,II,JJ)
C
C PERFORM A 1 X1 PIVOT
C
 60     IF(CABS1(A(II)).GT.EPS) GO TO 61
        A(II)=CMPLX(EPS,0.0)
C/6S
C       IF (IERR.EQ.0)CALL SETERR(21HCHEMD-SINGULAR MATRIX,21,9+I,1)
C/7S
        IF (IERR.EQ.0)CALL SETERR('CHEMD-SINGULAR MATRIX',21,9+I,1)
C/
        IERR=1
 61     AII=A(II)
        IF (CABS1(AII).EQ.0.0) AII =CMPLX(1.0,0.0)
        KE=II+NMI
        II=IIP1+NMI
        L=II
        DO 80 JI=IIP1,NI
           SAVE=-CONJG(A(JI))/AII
           DO 70 KJ=JI,NI
              A(L)=A(L)+A(KJ)*SAVE
              L=L+1
 70        CONTINUE
           A(JI)=SAVE
 80     CONTINUE
 81     I=IP1
        NI=NI+NMI
        GO TO 10
 100    IF (I.GT.N) GO TO 120
        IF (CABS1(A(II)).GT.EPS) GO TO 120
           A(II)=CMPLX(EPS,0.0)
C/6S
C       IF (IERR.EQ.0)CALL SETERR(21HCHEMD-SINGULAR MATRIX,21,9+I,1)
C/7S
        IF (IERR.EQ.0)CALL SETERR('CHEMD-SINGULAR MATRIX',21,9+I,1)
C/
 120    CALL LEAVE
        RETURN
        END