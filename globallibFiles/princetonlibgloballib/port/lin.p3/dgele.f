        SUBROUTINE DGELE(N,A,IA,B,IB,NB)
        INTEGER N,IA,IB,NB
        DOUBLE PRECISION A(IA,N),B(IB,NB)
C
C THIS SUBROUTINE SOLVES THE SYSTEM AX=B WHERE A
C IS A DOUBLE PRECISION GENERAL MATRIX.
C
C INPUT PARAMETERS
C     N      THE ORDER OF THE PROBLEM
C     A      AN IA X N ARRAY CONTAINING THE COEFFICIENT
C            MATRIX
C            THE SUBROUTINE DESTROYS THE MATRIX
C     IA     ROW DIMENSION OF THE A ARRAY,MUST BE AT LEAST N
C     B      AN IBXNB ARRAY WHOSE ITH COLUMN CONTAINS
C            THE ITH RIGHT HAND SIDE.
C            THE SUBROUTINE DESTROYS THE MATRIX
C     IB     ROW DIMENSION OF THE B ARRAY,MUST BE AT LEAST N
C     NB     NUMBER OF RIGHT HAND SIDES
C OUTPUT PARAMETERS
C     B      SOLUTION
C
C ERROR CONDITIONS
C    1       N LESS THAN 1      FATAL
C    2       IA  LESS THAN N    FATAL
C    3       IB   LESS THAN N   FATAL
C    4       NB LESS THAN 1     FATAL
C    10+K   NEARLY SINGULAR A OF RANK K  RECOVERABLE
C
       COMMON /CSTAK/ D
       DOUBLE PRECISION D(500)
       INTEGER INTER(1000)
       EQUIVALENCE(D(1),INTER(1))
C/6S
C      IF(N.LT.1)CALL SETERR(12HDGELE-N.LT.1,12,1,2)
C      IF(NB.LT.1)CALL SETERR(13HDGELE-NB.LT.1,13,4,2)
C      IF(IA.LT.N)CALL SETERR(13HDGELE-IA.LT.N,13,2,2)
C      IF(IB.LT.N)CALL SETERR(13HDGELE-IB.LT.N,13,3,2)
C/7S
       IF(N.LT.1)CALL SETERR('DGELE-N.LT.1',12,1,2)
       IF(NB.LT.1)CALL SETERR('DGELE-NB.LT.1',13,4,2)
       IF(IA.LT.N)CALL SETERR('DGELE-IA.LT.N',13,2,2)
       IF(IB.LT.N)CALL SETERR('DGELE-IB.LT.N',13,3,2)
C/
       CALL ENTER(1)
       IN=ISTKGT(N,2)
C DECOMPOSE A
       CALL DGEDC(N,A,IA,INTER(IN))
       IF(NERROR(IERR).EQ.0) GO TO 20
          CALL ERROFF
C/6S
C         CALL SETERR(21HDGELE-SINGULAR MATRIX,21,IERR,1)
C/7S
          CALL SETERR('DGELE-SINGULAR MATRIX',21,IERR,1)
C/
          GO TO 200
 20    CALL DGEFS(N,A,IA,B,IB,NB,INTER(IN))
       CALL DGEBS(N,A,IA,B,IB,NB)
       IF(NERROR(IERR).EQ.0) GO TO 200
          CALL ERROFF
C/6S
C         CALL SETERR(21HDGELE-SINGULAR MATRIX,21,IERR,1)
C/7S
          CALL SETERR('DGELE-SINGULAR MATRIX',21,IERR,1)
C/
 200   CALL LEAVE
       RETURN
       END
