      SUBROUTINE GQLOG(N, X, W)
      INTEGER N
      REAL X(N), W(N)
      COMMON /CSTAK/ D
      DOUBLE PRECISION D(500)
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR
      REAL R(500)
      EQUIVALENCE (D(1), R(1))
C J. L. BLUE, 15 DEC 77
C CALCULATE GAUSS QUADRATURE RULES ON (0, 1),
C    WITH WEIGHT FUNCTION LOG (1/X)
C USE GAUSQ = SACK-DONOVAN PROGRAM,
C   WITH SHIFTED LEGENDRE POLYNOMIALS.
C/6S
C     IF (N .LT. 1) CALL SETERR(18H  GQLOG - N .LT. 1, 18, 1, 2)
C/7S
      IF (N .LT. 1) CALL SETERR('  GQLOG - N .LT. 1', 18, 1, 2)
C/
      CALL ENTER(1)
      IA = ISTKGT(8*N, 3)
      IB = IA+2*N
      IC = IB+2*N
      IN = IC+2*N
      CALL G8LOG(N, X, W, R(IA), R(IB), R(IC), R(IN))
      IF (NERROR(NERR) .EQ. 0) GOTO 10
         CALL ERROFF
C/6S
C        CALL SETERR(30H GQLOG - CANNOT OBTAIN X AND W, 30, 2, 1)
C/7S
         CALL SETERR(' GQLOG - CANNOT OBTAIN X AND W', 30, 2, 1)
C/
   10 CALL LEAVE
      RETURN
      END