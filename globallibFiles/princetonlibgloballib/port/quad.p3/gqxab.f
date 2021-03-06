      SUBROUTINE GQXAB(N, ALPHA, BETA, X, W)
      INTEGER N
      REAL ALPHA, BETA, X(N), W(N)
      COMMON /CSTAK/ D
      DOUBLE PRECISION D(500)
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR
      REAL R(500)
      EQUIVALENCE (D(1), R(1))
C J. L. BLUE, 15 DEC 77
C CALCULATE GAUSS QUADRATURE RULES ON (-1, 1),
C    WITH WEIGHT FUNCTION (1-X)**ALPHA * (1+X)**BETA
C USE GAUSQ = SACK-DONOVAN PROGRAM, WITH JACOBI POLYNOMIALS.
C/6S
C     IF (N .LT. 1) CALL SETERR(18H  GQXAB - N .LT. 1, 18, 1, 2)
C     IF (AMIN1(ALPHA, BETA) .LE. (-1.E0)) CALL SETERR(
C    1   31H  GQXAB - ALPHA OR BETA .LE. -1, 31, 2, 2)
C/7S
      IF (N .LT. 1) CALL SETERR('  GQXAB - N .LT. 1', 18, 1, 2)
      IF (AMIN1(ALPHA, BETA) .LE. (-1.E0)) CALL SETERR(
     1   '  GQXAB - ALPHA OR BETA .LE. -1', 31, 2, 2)
C/
      CALL ENTER(1)
      IA = ISTKGT(8*N, 3)
      IB = IA+2*N
      IC = IB+2*N
      IN = IC+2*N
      CALL G8XAB(N, ALPHA, BETA, X, W, R(IA), R(IB), R(IC), R(IN))
      IF (NERROR(NERR) .EQ. 0) GOTO 10
         CALL ERROFF
C/6S
C        CALL SETERR(30H GQXAB - CANNOT OBTAIN X AND W, 30, 3, 1)
C/7S
         CALL SETERR(' GQXAB - CANNOT OBTAIN X AND W', 30, 3, 1)
C/
   10 CALL LEAVE
      IF (ALPHA .NE. BETA) RETURN
         CALL ASYM(N,X)
         CALL SYM(N,W)
         RETURN
      END
