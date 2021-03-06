      SUBROUTINE GQXA(N, ALPHA, X, W)
      INTEGER N
      REAL ALPHA, X(N), W(N)
      COMMON /CSTAK/ D
      DOUBLE PRECISION D(500)
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR
      REAL R(500)
      EQUIVALENCE (D(1), R(1))
C J. L. BLUE, 15 DEC 77
C CALCULATE GAUSS QUADRATURE RULES ON (0, 1),
C    WITH WEIGHT FUNCTION X**ALPHA
C USE GAUSQ = SACK-DONOVAN PROGRAM,
C   WITH SHIFTED JACOBI POLYNOMIALS.
C/6S
C     IF (N .LT. 1) CALL SETERR(17H  GQXA - N .LT. 1, 17, 1, 2)
C     IF (ALPHA .LE. (-1.E0)) CALL SETERR(22H  GQXA - ALPHA .LE. -1, 22,
C    1   2, 2)
C/7S
      IF (N .LT. 1) CALL SETERR('  GQXA - N .LT. 1', 17, 1, 2)
      IF (ALPHA .LE. (-1.E0)) CALL SETERR('  GQXA - ALPHA .LE. -1', 22,
     1   2, 2)
C/
      CALL ENTER(1)
      IA = ISTKGT(8*N, 3)
      IB = IA+2*N
      IC = IB+2*N
      IN = IC+2*N
      CALL G8XA(N, ALPHA, X, W, R(IA), R(IB), R(IC), R(IN))
      IF (NERROR(NERR) .EQ. 0) GOTO 10
         CALL ERROFF
C/6S
C        CALL SETERR(30H GQXA  - CANNOT OBTAIN X AND W, 30, 3, 1)
C/7S
         CALL SETERR(' GQXA  - CANNOT OBTAIN X AND W', 30, 3, 1)
C/
   10 CALL LEAVE
      RETURN
      END
