          SUBROUTINE   BPLD(N,ML,G,IG,EPS)
C
C THIS SUBROUTINE DETERMINES THE LU DECOMPOSITION OF A
C BANDED POSITIVE DEFINITE SYMMETRIC MATRIX USING GUASSIAN
C ELIMINATION WITHOUT PIVOTING.
C INPUT PARAMETERS
C N      THE ORDER OF THE MATRIX
C ML      MAXIMUM NUMBER OF NONZERO ELEMENTS ON OR BELOW
C         THE DIAGONAL IN ANY COLUMN OF THE MATRIX
C G       THE UPPER TRIANGULAR PORTION OF THE A MATRIX
C         STORED AS AN IG X N ARRAY WITH G(1+J-I,I)=A(I,J)
C         I.E. THE DIAGONAL  OF THE BAND MATRIX IS THE
C         FIRST ROW OF G, THE FIRST SUPPER DIAGONAL IS
C         THE SECOND ROW
C IG      ROW DIMENSION OF IG ,MUST NOT BE LESS THAN ML
C EPS     IF THE DIAGONAL OF D IS LESS THAN OR EQUAL TO EPS
C         THE MATRIX IS CONSIDERED SINGULAR
C OUTPUT PARAMETERS
C G       THE UPPER TRIANGULAR BAND FACTOR OF A
C SCRATCH STORAGE ALLOCATED-NONE
C ERROR STATES
C 1   N.LT.1      FATAL
C 2   ML.LT.1     FATAL
C 3   IG.LT.ML    FATAL
C 10+K   SINGULAR MATRIX OF RANK K    RECOVERABLE
C 10+N+K KTH PRINCIPAL MINOR IS NOT POSITIVE DEFINITE-RECOVERABLE
           INTEGER IG
           INTEGER N,ML
           REAL G(IG,N),EPS
           REAL T,X
C
C CHECK FOR INPUT ERRORS
C
C/6S
C      IF (N.LT.1)CALL SETERR(13H  BPLD-N.LT.1,13,1,2)
C      IF (ML.LT.1)CALL SETERR(14H  BPLD-ML.LT.1,14,2,2)
C      IF (IG.LT.ML) CALL SETERR(15H  BPLD-IG.LT.ML,15,3,2)
C/7S
       IF (N.LT.1)CALL SETERR('  BPLD-N.LT.1',13,1,2)
       IF (ML.LT.1)CALL SETERR('  BPLD-ML.LT.1',14,2,2)
       IF (IG.LT.ML) CALL SETERR('  BPLD-IG.LT.ML',15,3,2)
C/
       CALL ENTER(1)
       IERR=0
       M1=ML-1
C
C ZERO OUT LOWER RHS WART
C
       IEND=MIN0(M1,N)
       NP1=N+1
       IF (IEND.LT.1) GO TO 61
       DO 60 I=1,IEND
          JBEG=I+1
          NP1MI=NP1-I
          DO 50 J=JBEG,ML
             G(J,NP1MI)=0.0
 50      CONTINUE
 60    CONTINUE
 61    DO 200 K=1,N
        X=G(1,K)
          IF (X.GT.EPS) GO TO 90
             IF (X.LT.0.0) GO TO 70
             G(1,K)=EPS
             X=G(1,K)
             IF (X.EQ.0.0)X=1.0
C/6S
C            IF (IERR.EQ.0)
C    1       CALL SETERR(22H  BPLD-SINGULAR MATRIX,22,9+K,1)
C/7S
             IF (IERR.EQ.0)
     1       CALL SETERR('  BPLD-SINGULAR MATRIX',22,9+K,1)
C/
             IERR=1
             GO TO 90
 70       CALL ERROFF
C/6S
C         CALL SETERR(30H  BPLD-MATRIX NOT POSITIVE DEF,30,N+K+10,1)
C/7S
          CALL SETERR('  BPLD-MATRIX NOT POSITIVE DEF',30,N+K+10,1)
C/
             GOTO 210
 90       CONTINUE
            IEND=MIN0(N-K,M1)
            IF (IEND.LT.1) GO TO 200
            JEND=ML
          DO 170 I=1,IEND
             T=G(I+1,K)/X
             KPI=K+I
             JEND=JEND-1
             DO 140 J=1,JEND
                JPI=J+I
                G(J,KPI)=G(J,KPI)-T*G(JPI,K)
 140         CONTINUE
             G(I+1,K)=T
 170      CONTINUE
 200   CONTINUE
 210   CALL LEAVE
       RETURN
       END
