      SUBROUTINE DBURM1(NPTS, MESH, FN, MAXITR, ITOL, M, N, P, Q
     1   , DELK)
      INTEGER NPTS
      INTEGER MAXITR, ITOL, M, N
      DOUBLE PRECISION MESH(NPTS), FN(NPTS), P(1), Q(1), DELK
      COMMON /CSTAK/ DSTAK
      DOUBLE PRECISION DSTAK(500)
      INTEGER IDIG, IDFLR, I1MACH, ISTKGT, NPPTR, NQPTR
      INTEGER ENPTR, QKPTR, IEXPTR, J, ISTAK(1000)
      LOGICAL SMONOD
      DOUBLE PRECISION D1MACH, DFLOAT, QLRG, WS(500), DABS
      EQUIVALENCE (DSTAK(1), ISTAK(1))
      EQUIVALENCE (DSTAK(1), WS(1))
C   DBURM1 IS A LONG REAL PROCEDURE WHICH FINDS A
C   A RATIONAL FUNCTION WHICH IS THE BEST APPROXIMATION,
C   IN THE UNIFORM OR MINIMAX SENSE, TO A GIVEN DISCRETE
C   FUNCTION.  THE RATIONAL FUNCTION IS REPRESENTED AS
C   THE QUOTIENT OF TWO POLYNOMIALS EACH EXPANDED IN TERMS
C   OF TCHEBYCHEV POLYNOMIALS.  THIS ROUTINE STARTS FROM AN
C   INITIAL APPROXIMATION AND TERMINATES FOR ONE OF FOUR
C   REASONS: (1) THE ERROR CURVE EQUIOSCILLATES AND THE
C   ALTERNATING EXTREMA MATCH TO ITOL DIGITS, (2) THE NUMBER
C   OF ITERATIONS EXCEEDS MAXITR, (3) THE APPROXIMATION
C   CANNOT BE IMPROVED, OR (4) THE APPROXIMATION IS ESSENTIALLY
C   EQUAL TO THE GIVEN DISCRETE FUNCTION.
C   INPUT:
C   NPTS   - THE NUMBER OF MESH POINTS.
C   MESH   - THE ARRAY OF MESH POINTS.
C   FN     - THE ARRAY OF FUNCTION VALUES.
C   MAXITR - THE MAXIMUM NUMBER OF ITERATIONS.
C   ITOL   - THE NUMBER OF DIGITS TO WHICH THE EXTREMA SHOULD MATCH.
C   M      - THE DEGREE OF THE DESIRED NUMERATOR POLYNOMIAL.
C   N      - THE DEGREE OF THE DESIRED DENOMINATOR POLYNOMIAL.
C   P      - THE ARRAY OF COEFFICIENTS FOR THE INITIAL NUMERATOR.
C   Q      - THE ARRAY OF COEFFICIENTS FOR THE INITIAL DENOMINATOR.
C   OUTPUT:
C   P      - THE ARRAY OF COEFFICIENTS FOR THE NUMERATOR POLYNOMIAL.
C   Q      - THE ARRAY OF COEFFICIENTS FOR THE DENOMINATOR POLYNOMIAL.
C   DELK   - THE MAXIMUM ERROR IN THE APPROXIMATION.
C   ERROR STATES (ASTERISK INDICATES RECOVERABLE):
C   1  - INVALID DEGREE
C   2  - TOO FEW MESH POINTS
C   3  - MESH IS NOT STRICTLY MONOTONE
C   4  - MAXITR .LT. 0
C   5  - INVALID ACCURACY REQUEST
C   6  - DENOMINATOR IS NONPOSITIVE
C   7* - APPROXIMATION EQUALS FUNCTION
C   8* - NO IMPROVEMENT IN APPROXIMATION
C   9* - REACHED MAXIMUM NO. OF ITERATIONS
      CALL ENTER(1)
C/6S
C     IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(
C    1   23HDBURM1 - INVALID DEGREE, 23, 1, 2)
C     IF (NPTS .LT. M+N+2) CALL SETERR(28HDBURM1 - TOO FEW MESH POINTS
C    1   , 28, 2, 2)
C     IF (.NOT. SMONOD(MESH, NPTS, 1)) CALL SETERR(
C    1   38HDBURM1 - MESH IS NOT STRICTLY MONOTONE, 38, 3, 2)
C     IF (MAXITR .LT. 0) CALL SETERR(22HDBURM1 - MAXITR .LT. 0, 22, 4, 2
C    1   )
C/7S
      IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(
     1   'DBURM1 - INVALID DEGREE', 23, 1, 2)
      IF (NPTS .LT. M+N+2) CALL SETERR('DBURM1 - TOO FEW MESH POINTS'
     1   , 28, 2, 2)
      IF (.NOT. SMONOD(MESH, NPTS, 1)) CALL SETERR(
     1   'DBURM1 - MESH IS NOT STRICTLY MONOTONE', 38, 3, 2)
      IF (MAXITR .LT. 0) CALL SETERR('DBURM1 - MAXITR .LT. 0', 22, 4, 2
     1   )
C/
      IDIG = IDFLR(D1MACH(5)*DFLOAT(I1MACH(14)))
C/6S
C     IF (ITOL .LT. 1 .OR. IDIG .LT. ITOL) CALL SETERR(
C    1   33HDBURM1 - INVALID ACCURACY REQUEST, 33, 5, 2)
C/7S
      IF (ITOL .LT. 1 .OR. IDIG .LT. ITOL) CALL SETERR(
     1   'DBURM1 - INVALID ACCURACY REQUEST', 33, 5, 2)
C/
      QLRG = DABS(Q(1))
      J = 2
         GOTO  2
   1     J = J+1
   2     IF (J .GT. N+1) GOTO  3
         IF (QLRG .LT. DABS(Q(J))) QLRG = DABS(Q(J))
         GOTO  1
   3  IF (QLRG .NE. 0.0D0) GOTO 4
C/6S
C        CALL SETERR(35HDBURM1 - DENOMINATOR IS NONPOSITIVE, 35, 6, 2)
C/7S
         CALL SETERR('DBURM1 - DENOMINATOR IS NONPOSITIVE', 35, 6, 2)
C/
         GOTO  11
   4     J = 1
            GOTO  6
   5        J = J+1
   6        IF (J .GT. N+1) GOTO  7
            Q(J) = Q(J)/QLRG
            GOTO  5
   7     J = 1
            GOTO  9
   8        J = J+1
   9        IF (J .GT. M+1) GOTO  10
            P(J) = P(J)/QLRG
            GOTO  8
  10     CONTINUE
  11  NPPTR = ISTKGT(M+1, 4)
      NQPTR = ISTKGT(N+1, 4)
      ENPTR = ISTKGT(NPTS, 4)
      QKPTR = ISTKGT(NPTS, 4)
      IEXPTR = ISTKGT(NPTS, 2)
      CALL DB1RM1(NPTS, MESH, FN, MAXITR, ITOL, M, N, P, Q, DELK, WS(
     1   NPPTR), WS(NQPTR), WS(ENPTR), WS(QKPTR), ISTAK(IEXPTR))
      CALL LEAVE
      RETURN
      END
