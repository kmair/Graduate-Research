      SUBROUTINE LZ(N,A,NA,B,NB,X,NX,WANTX,EIGA,EIGB)
C
C THIS SUBROUTINE SOLVES THE GENERALIZED EIGENVALUE PROBLEM
C              A X  = LAMBDA B X
C WHERE A AND B ARE COMPLEX MATRICES OF ORDER N
C
C
C INPUT PARAMETERS
C
C
C N      ORDER OF A AND B
C
C A      AN N X N COMPLEX MATRIX
C
C NA     THE ROW DIMENSION OF THE A MATRIX
C
C B      AN N X N COMPLEX MATRIX
C
C NB     THE ROW DIMENSION OF THE B MATRIX
C
C NX     THE ROW DIMENSION OF THE X MATRIX
C
C WANTX  LOGICAL VARIABLE WHICH SHOULD BE SET TO .TRUE.
C        IF EIGENVECTORS ARE WANTED. OTHERWISE IT
C        SHOULD BE SET TO .FALSE.
C
C
C
C OUTPUT PARAMETERS
C
C
C X      THE ITH COLUMN CONTAINS THE ITH EIGENVECTOR
C        IF EIGENVECTORS ARE REQUESTED
C
C EIGA   A COMPLEX ARRAY OF LENGTH N CONTAINING THE DIAGONAL OF A
C
C EIGB   A COMPLEX ARRAY OF LENGTH N CONTAINING THE DIAGONAL OF B
C
C THE ITH EIGENVALUE CAN BE FOUND BY DIVIDING EIGA(I) BY
C EIGB(I). WATCH OUT FOR EIGB(I) BEING ZERO
C
C
C
C
      INTEGER N,NA,NB,NX
      COMPLEX X(NX,N)
      COMPLEX A(NA,N),B(NB,N),EIGA(N),EIGB(N)
      LOGICAL WANTX
C/6S
C     IF (N.LT.1)CALL SETERR(10H LZ-N.LT.1,10,1,2)
C     IF (NA.LT.N)CALL SETERR(11H LZ-NA.LT.N,11,2,2)
C     IF (NB.LT.N)CALL SETERR(11H LZ-NB.LT.N,11,3,2)
C     IF (WANTX.AND.NX.LT.N)CALL SETERR(11H LZ-NX.LT.N,11,4,2)
C/7S
      IF (N.LT.1)CALL SETERR(' LZ-N.LT.1',10,1,2)
      IF (NA.LT.N)CALL SETERR(' LZ-NA.LT.N',11,2,2)
      IF (NB.LT.N)CALL SETERR(' LZ-NB.LT.N',11,3,2)
      IF (WANTX.AND.NX.LT.N)CALL SETERR(' LZ-NX.LT.N',11,4,2)
C/
      CALL L4ZHES(N,A,NA,B,NB,X,NX,WANTX)
      CALL L4ZIT(N,A,NA,B,NB,X,NX,WANTX,EIGA,EIGB,ITER,30)
C/6S
C     IF (ITER.EQ.-1)
C    1 CALL SETERR(18H LZ-NO CONVERGENCE,18,5,1)
C/7S
      IF (ITER.EQ.-1)
     1 CALL SETERR(' LZ-NO CONVERGENCE',18,5,1)
C/
      RETURN
      END
