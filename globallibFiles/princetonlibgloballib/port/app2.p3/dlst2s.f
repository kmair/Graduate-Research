      SUBROUTINE DLST2S(MDIM,M,N,A,UD,B,X)
C
C  TO SOLVE THE LEAST SQUARES PROBLEM A*X=B WHEN THE MATRIX
C  A HAS ALREADY BEEN DECOMPOSED BY DLST2D.
C
C  INPUT
C
C    MDIM - THE DIMENSIONED COLUMN LENGTH OF A.
C    M    - THE NUMBER OF ROWS OF A
C    N    - THE NUMBER OF COLUMNS OF A. N.LE.M IS ASSUMED.
C    A    - THE DECOMPOSED MATRIX.
C    UD   - THE DIAGONAL OF U.
C    B    - THE RIGHT-HAND SIDE.
C
C  OUTPUT
C
C    B - B HAS BEEN CLOBBERED.
C        SQRT(SUM(I=N+1,M)(B(I)**2)) IS THE L2 NORM OF THE RESIDUAL
C        IN THE SOLUTION OF THE EQUATIONS.
C    X - THE SOLUTION VECTORS. X=B IS OK.
C
C  SCRATCH SPACE ALLOCATED - NONE.
C
C  ERROR STATES -
C
C    1 - MDIM.LT.M.
C    2 - N.LT.1.
C    3 - N.GT.M.
C    4 - UD(J)=0 OR A(J,J)=0.
C
      DOUBLE PRECISION A(MDIM,N),UD(N),B(M),X(N)
C
      DOUBLE PRECISION S
C
C ... CHECK THE INPUT.
C
C/6S
C     IF (MDIM.LT.M) CALL SETERR(18HDLST2S - MDIM.LT.M,18,1,2)
C     IF (N.LT.1) CALL SETERR(15HDLST2S - N.LT.1,15,2,2)
C     IF (N.GT.M) CALL SETERR(15HDLST2S - N.GT.M,15,3,2)
C/7S
      IF (MDIM.LT.M) CALL SETERR('DLST2S - MDIM.LT.M',18,1,2)
      IF (N.LT.1) CALL SETERR('DLST2S - N.LT.1',15,2,2)
      IF (N.GT.M) CALL SETERR('DLST2S - N.GT.M',15,3,2)
C/
C
C ... APPLY Q-TRANSPOSE TO B.
C
      DO 20 J=1,N
C/6S
C        IF (UD(J).EQ.0.0D0.OR.A(J,J).EQ.0.0D0) CALL SETERR
C    1      (28HDLST2S - UD(J)=0 OR A(J,J)=0,28,4,2)
C/7S
         IF (UD(J).EQ.0.0D0.OR.A(J,J).EQ.0.0D0) CALL SETERR
     1      ('DLST2S - UD(J)=0 OR A(J,J)=0',28,4,2)
C/
         S=0.D0
         DO 10 I=J,M
 10         S=S+A(I,J)*B(I)
         S=S/(UD(J)*A(J,J))
         DO 20 I=J,M
 20         B(I)=B(I)+S*A(I,J)
C
C ... BACK-SOLVE THE TRIANGULAR SYSTEM U*X=(Q-TRANSPOSE)*B.
C
      X(N)=B(N)/UD(N)
      IF (N.EQ.1) GO TO 50
      DO 40 II=2,N
         I=N+1-II
         S=B(I)
         IP1=I+1
         DO 30 J=IP1,N
 30         S=S-A(I,J)*X(J)
 40      X(I)=S/UD(I)
C
 50   RETURN
      END
