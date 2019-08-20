      SUBROUTINE DGQ1(N, X, W)
      INTEGER N
      DOUBLE PRECISION X(N), W(N)
      COMMON /CSTAK/ D
      DOUBLE PRECISION D(500)
      INTEGER IA, IB, IC, IN, ISTKGT, NERR, NERROR
      DOUBLE PRECISION R(500)
      EQUIVALENCE (D(1), R(1))
C J. L. BLUE, 15 DEC 77
C CALCULATE GAUSS QUADRATURE RULES ON (-1, 1),
C    WITH WEIGHT FUNCTION 1
C USE DGAUSQ = SACK-DONOVAN PROGRAM, WITH LEGENDRE POLYNOMIALS.
C/6S
C     IF (N .LT. 1) CALL SETERR(16H DGQ1 - N .LT. 1, 16, 1, 2)
C/7S
      IF (N .LT. 1) CALL SETERR(' DGQ1 - N .LT. 1', 16, 1, 2)
C/
      CALL ENTER(1)
      IA = ISTKGT(8*N, 4)
      IB = IA+2*N
      IC = IB+2*N
      IN = IC+2*N
      CALL DG8XAB(N, 0.D0, 0.D0, X, W, R(IA), R(IB), R(IC), R(IN))
      IF (NERROR(NERR) .EQ. 0) GOTO 10
         CALL ERROFF
C/6S
C        CALL SETERR(30HDGQ1   - CANNOT OBTAIN X AND W, 30, 2, 1)
C/7S
         CALL SETERR('DGQ1   - CANNOT OBTAIN X AND W', 30, 2, 1)
C/
   10 CALL LEAVE
      RETURN
      END