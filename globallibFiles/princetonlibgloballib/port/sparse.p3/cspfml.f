      SUBROUTINE CSPFML(N, AROW, X, B)
      INTEGER N
      COMPLEX  X(N), B(N)
      EXTERNAL AROW
      COMPLEX SUM
      INTEGER JMAX, I, J, JJ, IIA, IIAM1, JP, JR, IROW, IROWM1
      COMPLEX R(500)
      INTEGER IA(1000)
      COMMON /CSTAK/  R
      EQUIVALENCE (R(1),IA(1))
C THIS SUBROUTINE MULTIPLIES A BY X AND PUTS THE RESULT IN B WHERE
C A COMPUTED BY A FUNCTION
C INPUT PARAMETERS
C N      NUMBER OF EQUATIONS
C AROW    FUNCTION WHICH DELIVERS TO CSPFML ONE ROW OF THE
C         MATRIX AT A TIME
C X       N-VECTOR TO BE MULTIPLIED
C OUTPUT PARAMETERS
C B      A*X
C ERROR STATES
C 1 N.LT.1        FATAL
C STORAGE TAKEN FROM STACK- N COMPLEX AND N INTEGER LOCATIONS
C/6S
C     IF (N .LT. 1) CALL SETERR(13HCSPFML-N.LT.1, 13, 1, 2)
C/7S
      IF (N .LT. 1) CALL SETERR('CSPFML-N.LT.1', 13, 1, 2)
C/
      CALL ENTER(1)
      IIA=ISTKGT(N, 2)
      IIAM1=IIA-1
      IROW = ISTKGT(N, 5)
      IROWM1=IROW-1
      DO 30 I=1,N
         CALL AROW(I, R(IROW), IA(IIA), JMAX)
         SUM=(0.0,0.0)
         IF (JMAX.LT.1) GO TO 20
         DO 10 JJ=1,JMAX
            JP=JJ+IIAM1
            J=IA(JP)
            JR=JJ+IROWM1
            SUM=SUM+R(JR)*X(J)
  10     CONTINUE
  20     B(I)=SUM
  30  CONTINUE
      CALL LEAVE
      RETURN
      END
