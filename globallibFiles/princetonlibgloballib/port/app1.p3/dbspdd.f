      SUBROUTINE DBSPDD(D,N,M,G)
C
C  TO OBTAIN THE CHOLESKY DECOMPOSITION, A=G*(G-TRANSPOSE), OF A
C  BANDED, SYMMETRIC, POSITIVE-DEFINITE MATRIX A.
C
C  INPUT
C
C    D  - D(I,J)=A(I,I+J-M) STORES THE LOWER TRIANGULAR PORTION OF A
C         FOR I=1,...,N AND J=1,...,M.
C    N  - THE ORDER OF THE MATRIX A.
C    M  - THE NUMBER OF ENTRIES OF A ON OR BELOW THE DIAGONAL.
C
C  OUTPUT
C
C    G  - G IS STORED IN THE SAME FORM AS A IS IN D. D=G IS OK.
C
C  SCRATCH SPACE ALLOCATED - NONE.
C
C  ERROR STATES
C
C    1 - N.LT.1
C    2 - M.LT.1
C    3 - A IS NOT POSITIVE-DEFINITE. (RECOVERABLE)
C
      DOUBLE PRECISION D(N,M),G(N,M)
C
      DOUBLE PRECISION T,DSQRT
C
C ... CHECK THE INPUT.
C
C/6S
C     IF (M.LT.1) CALL SETERR(15HDBSPDD - M.LT.1,15,1,2)
C     IF (N.LT.1) CALL SETERR(15HDBSPDD - N.LT.1,15,2,2)
C/7S
      IF (M.LT.1) CALL SETERR('DBSPDD - M.LT.1',15,1,2)
      IF (N.LT.1) CALL SETERR('DBSPDD - N.LT.1',15,2,2)
C/
C
      MM1=M-1
      DO 50 J=1,N
         T=D(J,M)
         IF (M.EQ.1.OR.J.EQ.1) GO TO 20
         KLO=MAX0(M+1-J,1)
         DO 10 K=KLO,MM1
 10         T=T-G(J,K)**2
C
 20      IF (T.LE.0.0D0) GO TO 60
C
         G(J,M)=DSQRT(T)
         IF (J.EQ.N) GO TO 70
         IF (M.EQ.1) GO TO 50
         JP1=J+1
         ITOP=MIN0(MM1+J,N)
         DO 40 I=JP1,ITOP
            IDX=J+M-I
            T=D(I,IDX)
            K1=MAX0(I,M)-J+1
            IF (K1.GE.M) GO TO 40
            KDX=K1-I+J
            DO 30 K=K1,MM1
               T=T-G(I,KDX)*G(J,K)
 30            KDX=KDX+1
 40         G(I,IDX)=T/G(J,M)
 50      CONTINUE
      GO TO 70
C
C ... HERE FOR A NON-POSITIVE-DEFINITE MATRIX.
C
C/6S
C60   CALL SETERR(35HDBSPDD - A IS NOT POSITIVE-DEFINITE,35,3,1)
C/7S
 60   CALL SETERR('DBSPDD - A IS NOT POSITIVE-DEFINITE',35,3,1)
C/
C
 70   RETURN
C
      END