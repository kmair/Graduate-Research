        SUBROUTINE   BASS(N,ML,M,G,IG,B,IB,NB,COND)
C
C THIS SUBROUTINE SOLVES AX= B WHERE A IS A BANDED UNSYMMETRIC
C MATRIX.IT USES GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
C IT ALSO RETURN AN ESTIMATE OF THE CONDITION NUMBER OF A
C INPUT PARAMETERS
C N     ORDER OF THE SYSTEM
C M     THE TOTAL NUMBER OF NONZERO BANDS IN THE MATRIX
C ML    THE NUMBER OF BANDS ON AND BELOW THE DIAGONAL
C G     AN  IG X N ARRAY SUCH THAT
C       G(ML+J-I,I)=A(I,J).I.E. THE LEFTMOST BAND OF A IS THE
C       FIRST ROW OF G.
C IG    ROW DIMENSION OF G MUST BE .GE.M
C B     THE MATRIX OF RIGHT HAND SIDES
C NB    THE NUMBER OF RIGHT HAND SIDES
C IB    ROW DIMENSION OF B, MUST BE GREATER  OR EQUAL TO N
C OUTPUT PARAMETERS
C B     THE SOLUTION VECTORS X
C G     THE UPPER TRIANGULAR FACTOR OF A
C COND  AN ESIMATE OF THE CONDITION NUMBER OF A
C SCRATCH SPACE ALLOCATED-N INTEGER LOCATIONS
C                        ML*N REAL LOCATIONS
C ERROR STATES
C 1 N.LT.1       FATAL
C 2 ML.LT.1      FATAL
C 3 ML.GT.M      FATAL
C 4 IG.LT.M      FATAL
C 5 IB.LT.N      FATAL
C 6 NB.LT.1      FATAL
C 10 +K SINGULAR MATRIX OF RANK K   RECOVERABLE
C
         INTEGER N,ML,M,NB,IG,IB
         REAL G(IG,N),B(IB,NB)
         REAL COND
        REAL R(1000)
        DOUBLE PRECISION D(500)
        INTEGER ISTA(1000)
        COMMON /CSTAK/ D
        EQUIVALENCE(D(1),R(1)),(D(1),ISTA(1))
C CHECK FOR INPUT ERRORS
C/6S
C        IF (N.LT.1) CALL SETERR(13H  BASS-N.LT.1,13,1,2)
C        IF(ML.LT.1) CALL SETERR(14H  BASS-ML.LT.1,14,2,2)
C        IF(ML.GT.M) CALL SETERR(14H  BASS-ML.GT.M,14,3,2)
C        IF (NB.LT.1) CALL SETERR(14H  BASS-NB.LT.1,14,6,2)
C        IF (IG.LT.M) CALL SETERR(14H  BASS-IG.LT.M,14,4,2)
C        IF (IB.LT.N) CALL SETERR(14H  BASS-IB.LT.N,14,5,2)
C/7S
         IF (N.LT.1) CALL SETERR('  BASS-N.LT.1',13,1,2)
         IF(ML.LT.1) CALL SETERR('  BASS-ML.LT.1',14,2,2)
         IF(ML.GT.M) CALL SETERR('  BASS-ML.GT.M',14,3,2)
         IF (NB.LT.1) CALL SETERR('  BASS-NB.LT.1',14,6,2)
         IF (IG.LT.M) CALL SETERR('  BASS-IG.LT.M',14,4,2)
         IF (IB.LT.N) CALL SETERR('  BASS-IB.LT.N',14,5,2)
C/
         CALL ENTER(1)
         INTER=ISTKGT(N,2)
         IF (ML.GT.1)JGL=ISTKGT(N*(ML-1),3)
         CALL BACE(N,ML,M,G,IG,R(JGL),ML-1,ISTA(INTER),MU,COND)
         IF (NERROR(NERR).EQ.0) GO TO 10
C/6S
C           CALL N5ERR(22H  BASS-SINGULAR MATRIX,22,NERR,1)
C/7S
            CALL N5ERR('  BASS-SINGULAR MATRIX',22,NERR,1)
C/
            CALL LEAVE
         RETURN
 10     CONTINUE
            CALL BAFS(N,ML,R(JGL),ML-1,ISTA(INTER),B,IB,NB)
            CALL   BABS(N,G,IG,B,IB,NB,MU)
         CALL LEAVE
         RETURN
         END
