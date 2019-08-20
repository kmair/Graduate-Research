      SUBROUTINE DSPMML(N, IA, JA, A,  X, B)
      INTEGER N
      INTEGER IA(N), JA(N)
      DOUBLE PRECISION A(N), X(N), B(N)
      DOUBLE PRECISION SUM
      INTEGER JMIN, JMAX, I, J, JJ
C THIS SUBROUTINE MULTIPLIES A BY X AND PUTS THE RESULT IN B WHERE
C A IS A SPARSE MATRIX
C INPUT PARAMETERS
C N      NUMBER OF EQUATIONS
C IA      INTEGER VECTOR, LENGTH N+1, POINTING TO BEGINNINGS
C         OF ROWS IN JA AND A VECTORS
C JA      COLUMN INDICES OF NONZERO ELEMENTS OF MATRIX
C A       NONZERO ELEMENTS OF THE MATRIX
C X       N-VECTOR TO BE MULTIPLIED
C OUTPUT PARAMETERS
C B      A*X
C ERROR STATES
C 1 N.LT.1        FATAL
C/6S
C     IF (N .LT. 1) CALL SETERR(13HDSPMML-N.LT.1, 13, 1, 2)
C/7S
      IF (N .LT. 1) CALL SETERR('DSPMML-N.LT.1', 13, 1, 2)
C/
      SUM=0.0D0
      DO 30 I=1,N
         JMIN=IA(I)
         JMAX=IA(I+1)-1
         SUM=0.0D0
         IF (JMAX.LT.JMIN) GO TO 20
         DO 10 JJ=JMIN,JMAX
            J=JA(JJ)
            SUM=SUM+A(JJ)*X(J)
  10     CONTINUE
  20     B(I)=SUM
  30  CONTINUE
      RETURN
      END
