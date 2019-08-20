        SUBROUTINE  DSYMD(N,A,INTER,EPS)
C
C MNEMONIC -DOUBLE PRECISION SYMMETRIC DECOMPOSITION
C GIVEN A  SYMMETRIC MATRIX OF ORDER N STORED IN A DSYMMETRIC
C STORAGE SCHEME IN THE VECTOR A BY COLUMNS,THIS ROUTINE DETERMINES ITS
C DECOMPOSITION INTO PMDM(TRANSPOSE)P(TRANSPOSE) WHERE P IS A
C PERMUTATION MATRIX,M IS A UNIT LOWER TRIANGULAR MATRIX,AND D IS A
C BLOCK DIAGONAL MATRIX WITH BLOCKS OF ORDER 1 AND 2 WHERE D(I+1,1)
C IS NONZERO WHENEVER M(I+1,I) IS ZERO.
C INPUT PARAMTERS
C    N         THE ORDER OF THE PROBLEM
C     A        A VECTOR OF LENGTH N*(N+1)/2 CONTAINING
C              THE MATRIX A STORED BY COLUMNS.I.E. T E FIRST
C              COLUMN OCCUPIES THE FIRST N ELEMENTS OF THE VECTOR,
C              ELEMENTS (2,2) THROUGH (N,2) OF A OCCUPY THE NEXT N-1
C              ELEMENTS OF THE VECTOR.
C              THE SUBROUTINE DESTROYS THE ORIGINAL MATRIX
C       EPS    DOUBLE PRECISION VARIABLE . FOR A 1 X 1 BLOCK IF THE
C              MAGNITUDE OF THE PIVOT IS LESS THAN EPS, THE MATRIX
C              IS CONSIDERED SINGULAR
C OUTPUT PARAMETERS
C    A         CONTAINS THE DECOMPOSITION
C    INTER     AN INTEGER VECTOR OF LENGTH RECORDING THE INTERCHANGES
C              GENERATED
C ERROR CONDITIONS
C    1        N IS LESS THAN 1       FATAL
C   10 +K    SINGULAR MATRIX OF RANK K     RECOVERABLE
C
        DOUBLE PRECISION A(1),TEMP,SAVE,DENOM,AII,AIP1,AIP1I
         DOUBLE PRECISION EPS
        DOUBLE PRECISION ALPHA,ALFLAM,AAII,LAMBDA,SIGMA,DSQRT
        INTEGER INTER(N)
C/6S
C       IF (N.LT.1) CALL SETERR(12HDSYMD-N.LT.1,12,1,2)
C/7S
        IF (N.LT.1) CALL SETERR('DSYMD-N.LT.1',12,1,2)
C/
        INTER(N)=N
        ALPHA=(1.D0+DSQRT(17.D0))/8.D0
        IERR=0
        CALL ENTER(1)
        I=1
        II=1
        NI=N
 10     IF(I.GE.N) GO TO  100
        AAII=DABS(A(II))
        INTER(I)=I
C
C FIND THE LARGEST OFF DIAGONAL ELEMENT IN THE ITH COLUMN
C
        IIP1=II+1
        LAMBDA=DABS(A(IIP1))
        IP1=I+1
        IP2=I+2
        NMI=N-I
        JI=IIP1
        IF(IP2.GT.N) GO TO 16
        DO 15 KI=IIP1,NI
           IF(DABS(A(KI)).LE.LAMBDA) GO TO 15
           LAMBDA=DABS(A(KI))
           JI=KI
 15     CONTINUE
 16     ALFLAM=ALPHA*LAMBDA
        IF (AAII.GE.ALFLAM) GO TO 60
C
C FIND THE LARGEST OFFDIAGONAL ELEMENT IN THE JTH COLUMN
C
        SIGMA=LAMBDA
        J=I+JI-II
        NMJP1=N-J+1
        NJ=(J*(N+NMJP1))/2
        JJ=NJ-(N-J)
        JJP1=JJ+1
        IF(JJP1.GT.NJ) GO TO 21
        DO 20 KJ=JJP1,NJ
           IF(DABS(A(KJ)).GT.SIGMA) SIGMA =DABS(A(KJ))
 20     CONTINUE
 21     NMIM1=NMI-1
        IF(NMJP1.GT.NMIM1) GO TO 26
        KJ=JJ
        DO 25 K=NMJP1,NMIM1
           KJ=KJ-K
           IF(DABS(A(KJ)).GT.SIGMA) SIGMA=DABS(A(KJ))
 25     CONTINUE
 26     IF (AAII.GE.ALFLAM*(LAMBDA/SIGMA)) GO TO 60
        IF (DABS(A(JJ)).GE.ALPHA*SIGMA) GO TO 55
C
C PERFORM A 2 BY 2 PIVOT STEP
C
        IP1IP1=IIP1+NMI
        INTER(I)=J
        IF(IP2.GT.N) GO TO 51
        IF(J.EQ.IP1) GO TO 30
        CALL DI4SP1(A,N,J,IP1,IP1IP1,JJ)
        TEMP=A(JI)
        A(JI)=A(IIP1)
        A(IIP1)=TEMP
 30     AIP1I=A(IIP1)
        AII=A(II)/AIP1I
        AIP1=A(IP1IP1)
        DENOM=AII*AIP1-AIP1I
        JB=IIP1+1
        II=IIP1+2*NMI
        L=II
        DO 50 JI=JB,NI
           JIPNMI=JI+NMI
           TEMP=(A(JI)-AII*A(JIPNMI))/DENOM
           SAVE=-(A(JIPNMI)+TEMP*AIP1)/AIP1I
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
        CALL DI4SP1(A,N,J,I,II,JJ)
C
C PERFORM A 1 X1 PIVOT
C
 60     IF (DABS(A(II)).GT.EPS) GO TO 61
         A(II)=DSIGN(EPS,A(II))
C/6S
C        IF (IERR.EQ.0)CALL SETERR(21HDSYMD-SINGULAR MATRIX,21,9+I,1)
C/7S
         IF (IERR.EQ.0)CALL SETERR('DSYMD-SINGULAR MATRIX',21,9+I,1)
C/
         IERR=1
 61     AII=A(II)
        IF (AII.EQ.0.D0)AII=1.0D0
        II=IIP1+NMI
        L=II
        DO 80 JI=IIP1,NI
           SAVE=-A(JI)/AII
           DO 70 KJ=JI,NI
              A(L)=A(L)+A(KJ)*SAVE
              L=L+1
 70        CONTINUE
           A(JI)=SAVE
 80     CONTINUE
 81     I=IP1
        NI=NI+NMI
        GO TO 10
 100     CONTINUE
         IF (I.GT.N) GO TO 120
        IF (DABS(A(II)).GT.EPS) GO TO 120
          A(II)=DSIGN(EPS,A(II))
C/6S
C         IF (IERR.EQ.0)
C    1    CALL SETERR(21HDSYMD-SINGULAR MATRIX,21,9+I,1)
C/7S
          IF (IERR.EQ.0)
     1    CALL SETERR('DSYMD-SINGULAR MATRIX',21,9+I,1)
C/
 120     CALL LEAVE
        RETURN
        END
