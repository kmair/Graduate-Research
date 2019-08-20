          SUBROUTINE   CBPDC(N,ML,G,IG)
C
C THIS SUBROUTINE DETERMINES THE LU DECOMPOSITION OF A
C BANDED POSITIVE DEFINITE HERMITIAN MATRIX USING GAUSSIAN
C ELIMINATION WITHOUT PIVOTING.
C INPUT PARAMETERS
C N      THE ORDER OF THE MATRIX
C ML      MAXIMUM NUMBER OF NONZERO ELEMENTS ON OR BELOW
C         THE DIAGONAL IN ANY COLUMN OF THE MATRIX
C G       THE UPPER TRIANGULAR PORTION OF THE A MATRIX
C         STORED AS AN IG X N ARRAY WITH G(1+J-I,I)=A(I,J)
C         I.E. THE DIAGONAL  OF THE BAND MATRIX IS THE
C         FIRST ROW OF G, THE FIRST SUPER DIAGONAL IS
C         THE SECOND ROW
C IG      ROW DIMENSION OF IG ,MUST NOT BE LESS THAN ML
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
           COMPLEX G(IG,N)
           REAL EPS
          REAL R1MACH,CBPNM
C
C CHECK FOR INPUT ERRORS
C
C/6S
C      IF (N.LT.1)CALL SETERR(13H CBPDC-N.LT.1,13,1,2)
C      IF (ML.LT.1)CALL SETERR(14H CBPDC-ML.LT.1,14,2,2)
C      IF (IG.LT.ML) CALL SETERR(15H CBPDC-IG,LT.ML,15,3,2)
C/7S
       IF (N.LT.1)CALL SETERR(' CBPDC-N.LT.1',13,1,2)
       IF (ML.LT.1)CALL SETERR(' CBPDC-ML.LT.1',14,2,2)
       IF (IG.LT.ML) CALL SETERR(' CBPDC-IG,LT.ML',15,3,2)
C/
       IF (ML.EQ.1) RETURN
       CALL ENTER(1)
         EPS=CBPNM(N,ML,G,IG)*R1MACH(4)
           CALL   CBPLD(N,ML,G,IG,EPS)
         IF (NERROR(NERR).EQ.0) GO TO 30
            IF (NERR.LT.10+N) GO TO 5
C/6S
C         CALL N5ERR(30H CBPDC-MATRIX NOT POSITIVE DEF,30,NERR,1)
C/7S
          CALL N5ERR(' CBPDC-MATRIX NOT POSITIVE DEF',30,NERR,1)
C/
            GO TO 30
C/6S
C5          CALL N5ERR(22H CBPDC-SINGULAR MATRIX,22,NERR,1)
C/7S
 5          CALL N5ERR(' CBPDC-SINGULAR MATRIX',22,NERR,1)
C/
 30       CALL LEAVE
          RETURN
          END
