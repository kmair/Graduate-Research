       SUBROUTINE  DSYSS(N,A,B,IB,NB,COND)
C MNEMONIC-DOUBLE PRECISION SYMMETRIC DSYSTEM SOLVER
         INTEGER N,IB,NB
        DOUBLE PRECISION A(N),B(IB,NB)
C
C THIS SUBROUTINE SOLVES THE PROBLEM AX=B WHERE
C A IS SYMMETRIC MATRIX,WHICH MAY BE INDEFINITE
C BUT NOT SINGULAR
C AN ESTIMATE OF THE CONDITION NUMBER IS ALSO GIVEN
C
C INPUT PARAMETERS
C N       ORDER OF THE PROBLEM
C A      AN N X(N+1)/2 VECTOR CONTAINING THE
C        COEFFICIENT MATRIX STORED BY COLUMNS ,
C        I.E. IN THE ORDER
C            1
C            2  5
C            3  6  8
C            4  7  9 10
C B     A MATRIX CONTAINING THE RIGHT HAND SIDES
C IB     THE ROW DIMENSION OF THE B MATRIX
C NB     THE NUMBER OF RIGHT HAND SIDES
C
C OUTPUT PARAMETERS
C B      THE SOLUTION
C COND   ESTIMATE ON THE CONDITION NUMBER
C ERROR CONDITIONS
C   1       N.LT.1       FATAL
C   2       IB.LT.N      FATAL
C   3       NB.LT.1      FATAL
C   10+K    SINGULAR MATRIX OF RANK K  RECOVERABLE
C
C THIS SUBROUTINE CALLS  DSYCE AND  DSYFBS
C N EXTRA INTEGER LOCATIONS ARE REQUESTED BY THE STORAGE ALLOCATOR
C
       COMMON /CSTAK/ D
       DOUBLE PRECISION D(500)
       DOUBLE PRECISION COND
       INTEGER INTER(1000)
       EQUIVALENCE(D(1),INTER(1))
C/6S
C      IF (N.LT.1)CALL SETERR(12HDSYSS-N.LT.1,12,1,2)
C      IF (IB.LT.N) CALL SETERR(13HDSYSS-IB.LT.N,13,2,2)
C      IF (NB.LT.1) CALL SETERR(13HDSYSS-NB.LT.1,13,3,2)
C/7S
       IF (N.LT.1)CALL SETERR('DSYSS-N.LT.1',12,1,2)
       IF (IB.LT.N) CALL SETERR('DSYSS-IB.LT.N',13,2,2)
       IF (NB.LT.1) CALL SETERR('DSYSS-NB.LT.1',13,3,2)
C/
       CALL ENTER(1)
       IN=ISTKGT(N,2)
C DECOMPOSE A
       CALL DSYCE(N,A,INTER(IN),COND)
       IF (NERROR(IER).EQ.0) GO TO 10
C/6S
C        CALL N5ERR(21HDSYSS-SINGULAR MATRIX,21,IER,1)
C/7S
         CALL N5ERR('DSYSS-SINGULAR MATRIX',21,IER,1)
C/
         GO TO 200
 10    CONTINUE
          CALL DSYFBS(N,A,B,IB,NB,INTER(IN))
 200   CALL LEAVE
       RETURN
       END
