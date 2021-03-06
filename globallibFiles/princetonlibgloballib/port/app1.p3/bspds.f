      SUBROUTINE BSPDS(D,N,M,B,NB)
C
C  TO SOLVE THE SYSTEM OF LINEAR ALGEBRAIC EQUATIONS A*X=B,
C  WHERE A IS A BANDED, SYMMETRIC, POSITIVE-DEFINITE MATRIX,
C  FOR NB RIGHT-HAND SIDES B.
C
C  METHOD - CHOLESKY DECOMPOSITION OF A.
C
C  INPUT
C
C    D  - D(I,J)=A(I,I+J-M), STORES THE LOWER TRIANGULAR PORTION OF A
C         FOR I=1,...,N AND J=1,...,M.
C    N  - THE ORDER OF THE MATRIX A.
C    M  - THE NUMBER OF ENTRIES OF A ON OR BELOW THE DIAGONAL.
C         M IS THE HALF BAND-WIDTH OF THE MATRIX.
C    B  - THE RIGHT-HAND SIDES.
C    NB - THE NUMBER OF RIGHT-HAND SIDES.
C
C  OUTPUT
C
C    D - THE LOWER TRIANGULAR CHOLESKY FACTOR OF A, STORED
C        IN THE SAME FORM AS A WAS IN D.
C    B - THE SOLUTIONS X.
C
C  SCRATCH SPACE ALLOCATED - NONE.
C
C  ERROR STATES
C
C    1 - N.LT.1
C    2 - M.LT.1
C    3 - NB.LT.1
C    4 - A IS NOT POSITIVE-DEFINITE. (RECOVERABLE)
C
      REAL D(N,M),B(N,NB)
C
      CALL ENTER(1)
C
C ... CHECK THE INPUT FOR ERRORS.
C
C/6S
C     IF (N.LT.1) CALL SETERR(15H BSPDS - N.LT.1,15,1,2)
C     IF (M.LT.1) CALL SETERR(15H BSPDS - M.LT.1,15,2,2)
C     IF (NB.LT.1) CALL SETERR(16H BSPDS - NB.LT.3,16,3,2)
C/7S
      IF (N.LT.1) CALL SETERR(' BSPDS - N.LT.1',15,1,2)
      IF (M.LT.1) CALL SETERR(' BSPDS - M.LT.1',15,2,2)
      IF (NB.LT.1) CALL SETERR(' BSPDS - NB.LT.3',16,3,2)
C/
C
C ... GET THE CHOLESKY DECOMPOSITION OF A.
C
      CALL BSPDD(D,N,M,D)
C
C ... TEST FOR A NON-POSITIVE-DEFINITE MATRIX.
C
      IF (NERROR(NERR).NE.0) GO TO 20
C
C ... IF CHOLESKY CALL WORKED, SOLVE THE SYSTEMS.
C
      DO 10 J=1,NB
 10      CALL BSPDB(D,N,M,B(1,J))
      GO TO 30
C
C ... HERE FOR A NON-POSITIVE-DEFINITE MATRIX.
C
 20   CALL ERROFF
C/6S
C     CALL SETERR(35H BSPDS - A IS NOT POSITIVE-DEFINITE,35,4,1)
C/7S
      CALL SETERR(' BSPDS - A IS NOT POSITIVE-DEFINITE',35,4,1)
C/
C
 30   CALL LEAVE
C
      RETURN
C
      END
