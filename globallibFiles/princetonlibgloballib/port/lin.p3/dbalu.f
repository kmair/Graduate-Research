          SUBROUTINE   DBALU(N,ML,M,G,IG,AL,IAL,INT,MU,EPS)
C
C THIS SUBROUTINE DETERMINES THE LU DECOMPOSITION OF A
C BANDED UNSYMMETRIC MATRIX USING GAUSSIAN ELIMINATION
C WITH PARTIAL PIVOTING
C INPUT PARAMETERS
C N      THE ORDER OF THE MATRIX
C ML      MAXIMUM NUMBER OF NONZERO ELEMENTS ON OR BELOW
C         THE DIAGONAL IN ANY COLUMN OF THE MATRIX
C M       THE MAXIMUM NUMBER OF NONZERO ELEMENTS IN EACH
C         COLUMN
C G       THE MATRIX A STORED AS AN IG X N ARRAY
C         WITH G(ML+J-I,I)=A(I,J).I.E. THE LEFTMOST
C         DIAGONAL OF THE BAND MATRIX IS THE FIRST ROW OF G
C IG      ROW DIMENSION OF IG ,MUST NOT BE LESS THAN M
C IAL     ROW DIMENSION OF AL, MUST NOT BE LESS THAN ML-1
C EPS     LARGEST NONACCEPTABLE PIVOT
C OUTPUT PARAMETERS
C G       THE UPPER TRIANGULAR BAND FACTOR OF A
C AL      AN IAL X N ARRAY CONTAINING THE LOWER TRIANGULAR
C         BAND FACTOR OF A
C INT     AN INTEGER VECTOR OF LENGTH N RECORDING INTERCHANGES
C          INVOLVED IN GAUSSIAN ELIMINATION
C MU      THE NUMBER OF NONZERO DIAGONALS IN THE UPPER
C         TRIANGULAR BAND FACTOR
C SCRATCH STORAGE ALLOCATED-NONE
C ERROR STATES
C 1   N.LT.1      FATAL
C 2   ML.LT.1     FATAL
C 3   M.LT.ML     FATAL
C 4   IG.LT.M     FATAL
C 5   IAL.LT.ML-1 FATAL
C 10+K SINGULAR MATRIX OF RANK K   RECOVERABLE
           INTEGER IG,IAL
           INTEGER N,ML,M,INT(N)
           DOUBLE PRECISION G(IG,N),AL(IAL,N)
           DOUBLE PRECISION X
           DOUBLE PRECISION EPS,XI
C
C CHECK FOR INPUT ERRORS
C
C/6S
C      IF (N.LT.1)CALL SETERR(13H DBALU-N.LT.1,13,1,2)
C      IF (ML.LT.1)CALL SETERR(14H DBALU-ML.LT.1,14,2,2)
C      IF (M.LT.ML) CALL SETERR(14H DBALU-M.LT.ML,14,3,2)
C      IF (IG.LT.M) CALL SETERR(14H DBALU-IG.LT.M,14,4,2)
C     IF (IAL.LT.ML-1)CALL SETERR(18H DBALU-IAL.LT.ML-1,18,5,2)
C/7S
       IF (N.LT.1)CALL SETERR(' DBALU-N.LT.1',13,1,2)
       IF (ML.LT.1)CALL SETERR(' DBALU-ML.LT.1',14,2,2)
       IF (M.LT.ML) CALL SETERR(' DBALU-M.LT.ML',14,3,2)
       IF (IG.LT.M) CALL SETERR(' DBALU-IG.LT.M',14,4,2)
      IF (IAL.LT.ML-1)CALL SETERR(' DBALU-IAL.LT.ML-1',18,5,2)
C/
       IER=0
       MU=M-ML+1
       INTN=1
       MU1=MU
       MU2=MU
       NP1=N+1
       CALL ENTER(1)
       M1=ML-1
       M2=M-ML
       LL=M1
       MLP1=ML+1
       IF (M1.LT.1) GO TO 41
       N2=2*N
       DO 40 I=1,M1
          JBEG=MLP1-I
          JEND=MIN0(M,N2-I)
          DO 20 J=JBEG,JEND
             JMLL=J-LL
              G(JMLL,I)=G(J,I)
 20       CONTINUE
          LL=LL-1
          JBEG=M-LL
          DO 30 J=JBEG,M
              G(J,I)=0.D0
 30       CONTINUE
 40   CONTINUE
C
C ZERO OUT LOWER RHS WART
C
 41    IF (M2.LT.1) GO TO 61
       DO 60 I=1,M2
          JBEG=ML+I
          NP1MI=NP1-I
          DO 50 J=JBEG,M
             G(J,NP1MI)=0.D0
 50      CONTINUE
 60    CONTINUE
 61    CONTINUE
       DO 200 K=1,N
          X=DABS(G(1,K))
          I=K
          LL=MIN0(M1+K,N)
          KP1=K+1
          IF (LL.LT.KP1) GO TO 80
C GET THE PIVOT ROW
          DO 70 J=KP1,LL
            IF (DABS(G(1,J)).LE.X) GO TO 70
               I=J
               X=DABS(G(1,J))
 70       CONTINUE
 80       IF (X.GT.EPS) GO TO 90
C/6S
C      IF (IER.EQ.0)CALL SETERR(22H DBALU-SINGULAR MATRIX,22,9+K,1)
C/7S
       IF (IER.EQ.0)CALL SETERR(' DBALU-SINGULAR MATRIX',22,9+K,1)
C/
          G(1,I)=DSIGN(EPS,G(1,I))
             IER=1
 90       CONTINUE
          INT(K)=I
          IF (I.EQ.K) GO TO 110
             INTN=-INTN
C NEED TO INTERCHANGE COLUMNS
             DO 100 J=1,M
                X=G(J,K)
                G(J,K)=G(J,I)
                G(J,I)=X
 100         CONTINUE
 110      IF (K.GE.LL) GO TO 200
          MM=MIN0(NP1,MU1+I)-K
          MU=MAX0(MU,MM)
          MU2=MAX0(MU2-1,MM)
          IMK=0
          XI=G(1,K)
          IF(XI.EQ.0.E0)XI=1.0
          MU2P1=MU2+1
          DO 170 I=KP1,LL
             X=-G(1,I)/XI
             IMK=IMK+1
             AL(IMK,K)=X
             IF (MU2.LT.2) GO TO 141
             DO 140 J=2,MU2
                G(J-1,I)=G(J,I)+X*G(J,K)
 140         CONTINUE
 141         IF (MU2.EQ.M) GO TO 170
             DO 150 J=MU2P1,M
 150            G(J-1,I)=G(J,I)
 170    CONTINUE
        G(M,LL)=0.D0
 200   CONTINUE
 210   INT(N)=INTN
       CALL LEAVE
       RETURN
       END
