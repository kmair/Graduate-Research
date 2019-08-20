        SUBROUTINE   DBPLE(N,ML,G,IG,B,IB,NB)
C
C THIS SUBROUTINE SOLVES AX= B WHERE A IS A BANDED SYMMETRIC
C POSITIVE DEFINITE MATRIX. IT USES GAUSSIAN ELIMINATION
C WITHOUT PIVOTING
C INPUT PARAMETERS
C N     ORDER OF THE SYSTEM
C ML    THE NUMBER OF BANDS ON AND BELOW THE DIAGONAL
C G     G IS AN ML X N ARRAY INTO WHICH A IS PACKED
C       G(1+J-I,I)=A(I,J),J.GE.I.I.E. THE DIAGONAL BAND OF A IS THE
C       FIRST ROW OF G.
C IG    ROW DIMENSION OF G, MUST BE .GE. ML
C B     THE MATRIX OF RIGHT HAND SIDES
C NB    THE NUMBER OF RIGHT HAND SIDES
C IB    ROW DIMENSION OF B, MUST BE GREATER  OR EQUAL TO N
C OUTPUT PARAMETERS
C B     THE SOLUTION VECTORS X
C G     THE UPPER TRIANGULAR FACTOR OF A
C SCRATCH SPACE ALLOCATED-NONE
C ERROR STATES
C 1 N.LT.1       FATAL
C 2 ML.LT.1      FATAL
C 3 IG.LT.ML     FATAL
C 4 IB.LT.N      FATAL
C 5 NB.LT.1        FATAL
C 10+K   SINGULAR MATRIX OF RANK K     RECOVERABLE
C 10+N+K KTH PRINCIPAL MINOR IS NOT POSITIVE DEFINITE RECOVERABLE
C
         INTEGER ML,N,NB,IG,IB
         DOUBLE PRECISION G(IG,N),B(IB,NB)
C CHECK FOR INPUT ERRORS
C/6S
C        IF (N.LT.1) CALL SETERR(13H DBPLE-N.LT.1,13,1,2)
C        IF(ML.LT.1) CALL SETERR(14H DBPLE-ML.LT.1,14,2,2)
C        IF (NB.LT.1) CALL SETERR(14H DBPLE-NB.LT.1,14,5,2)
C        IF (IG.LT.ML) CALL SETERR(15H DBPLE-IG.LT.ML,15,3,2)
C        IF (IB.LT.N) CALL SETERR(14H DBPLE-IB.LT.N,14,4,2)
C/7S
         IF (N.LT.1) CALL SETERR(' DBPLE-N.LT.1',13,1,2)
         IF(ML.LT.1) CALL SETERR(' DBPLE-ML.LT.1',14,2,2)
         IF (NB.LT.1) CALL SETERR(' DBPLE-NB.LT.1',14,5,2)
         IF (IG.LT.ML) CALL SETERR(' DBPLE-IG.LT.ML',15,3,2)
         IF (IB.LT.N) CALL SETERR(' DBPLE-IB.LT.N',14,4,2)
C/
         CALL ENTER(1)
           CALL  DBPDC(N,ML,G,IG)
         IF (NERROR(NERR).EQ.0) GO TO 10
            IF (NERR.LT.10+N) GO TO 5
C/6S
C         CALL N5ERR(30H DBPLE-MATRIX NOT POSITIVE DEF,30,NERR,1)
C/7S
          CALL N5ERR(' DBPLE-MATRIX NOT POSITIVE DEF',30,NERR,1)
C/
            GO TO 30
C/6S
C5          CALL N5ERR(22H DBPLE-SINGULAR MATRIX,22,NERR,1)
C/7S
 5          CALL N5ERR(' DBPLE-SINGULAR MATRIX',22,NERR,1)
C/
            GO TO 30
 10      CONTINUE
            CALL  DBPFS(N,ML,G,IG,B,IB,NB)
            CALL  DBPBS(N,ML,G,IG,B,IB,NB)
 30      CALL LEAVE
         RETURN
         END