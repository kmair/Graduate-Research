      SUBROUTINE BURAM(NPTS, MESH, FN, M, N, P, Q, DELK)
      INTEGER NPTS
      INTEGER M, N
      REAL MESH(NPTS), FN(NPTS), P(1), Q(1), DELK
      INTEGER MAXITR, ITOL, NERROR, IER, I
      REAL FNMAX, FNMIN
      LOGICAL SMONOR
C   BURAM  IS A REAL PROCEDURE WHICH FINDS A
C   A RATIONAL FUNCTION WHICH IS THE BEST APPROXIMATION,
C   IN THE UNIFORM OR MINIMAX SENSE, TO A GIVEN DISCRETE
C   FUNCTION.  THE RATIONAL FUNCTION IS REPRESENTED AS
C   THE QUOTIENT OF TWO POLYNOMIALS EACH EXPANDED IN TERMS
C   OF TCHEBYCHEV POLYNOMIALS.  THIS ROUTINE IS A SHELL
C   WHICH IN TURN CALLS THE ROUTINE BURM1  WITH CERTAIN
C   DEFAULT VALUES FOR THE INITIAL APPROXIMATION AND  FOR
C   THE STOPPING CRITERIA.
C   INPUT:
C   NPTS   - THE NUMBER OF MESH POINTS.
C   MESH   - THE ARRAY OF MESH POINTS.
C   FN     - THE ARRAY OF FUNCTION VALUES.
C   M      - THE DEGREE OF THE DESIRED NUMERATOR POLYNOMIAL.
C   N      - THE DEGREE OF THE DESIRED DENOMINATOR POLYNOMIAL.
C   OUTPUT:
C   P      - THE ARRAY OF COEFFICIENTS FOR THE NUMERATOR POLYNOMIAL.
C   Q      - THE ARRAY OF COEFFICIENTS FOR THE DENOMINATOR POLYNOMIAL.
C   DELK   - THE MAXIMUM ERROR IN THE APPROXIMATION.
C   ERROR STATES (ASTERISK INDICATES RECOVERABLE):
C   1  - INVALID DEGREE
C   2  - TOO FEW MESH POINTS
C   3  - MESH IS NOT STRICTLY MONOTONE
C   4* - APPROXIMATION EQUALS FUNCTION
C   5* - NO IMPROVEMENT IN APPROXIMATION
C   6* - REACHED 50 ITERATIONS
      CALL ENTER(1)
C/6S
C     IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(
C    1   23HBURAM  - INVALID DEGREE, 23, 1, 2)
C     IF (NPTS .LT. M+N+2) CALL SETERR(28HBURAM  - TOO FEW MESH POINTS
C    1   , 28, 2, 2)
C     IF (.NOT. SMONOR(MESH, NPTS, 1)) CALL SETERR(
C    1   38HBURAM  - MESH IS NOT STRICTLY MONOTONE, 38, 3, 2)
C/7S
      IF (M .LT. 0 .OR. N .LT. 0) CALL SETERR(
     1   'BURAM  - INVALID DEGREE', 23, 1, 2)
      IF (NPTS .LT. M+N+2) CALL SETERR('BURAM  - TOO FEW MESH POINTS'
     1   , 28, 2, 2)
      IF (.NOT. SMONOR(MESH, NPTS, 1)) CALL SETERR(
     1   'BURAM  - MESH IS NOT STRICTLY MONOTONE', 38, 3, 2)
C/
C   INITIALIZE THE NUMERATOR AND DEMONINATOR POLYNOMIALS.
      FNMAX = FN(1)
      FNMIN = FN(1)
      DO  3 I = 2, NPTS
         IF (FNMAX .GE. FN(I)) GOTO 1
            FNMAX = FN(I)
            GOTO  2
   1        IF (FN(I) .LT. FNMIN) FNMIN = FN(I)
   2     CONTINUE
   3     CONTINUE
      CALL SETR(M+1, 0.0E0, P)
      P(1) = 0.5E0*(FNMAX+FNMIN)
      CALL SETR(N+1, 0.0E0, Q)
      Q(1) = 1.0E0
      DELK = FNMAX-P(1)
      IF (0 .GE. M .AND. 0 .GE. N) GOTO 11
         MAXITR = 50
         ITOL = 2
         CALL BURM1(NPTS, MESH, FN, MAXITR, ITOL, M, N, P, Q, DELK)
         IF (NERROR(IER) .EQ. 0) GOTO 10
            IF (IER .NE. 7) GOTO 4
C/6S
C              CALL N5ERR(38HBURAM  - APPROXIMATION EQUALS FUNCTION, 38,
C    1            4, 1)
C/7S
               CALL N5ERR('BURAM  - APPROXIMATION EQUALS FUNCTION', 38,
     1            4, 1)
C/
               GOTO  9
   4           IF (IER .NE. 8) GOTO 5
C/6S
C                 CALL N5ERR(
C    1               40HBURAM  - NO IMPROVEMENT IN APPROXIMATION, 40, 5,
C    2               1)
C/7S
                  CALL N5ERR(
     1               'BURAM  - NO IMPROVEMENT IN APPROXIMATION', 40, 5,
     2               1)
C/
                  GOTO  8
   5              IF (IER .NE. 9) GOTO 6
C/6S
C                    CALL N5ERR(30HBURAM  - REACHED 50 ITERATIONS, 30, 6
C    1                  , 1)
C/7S
                     CALL N5ERR('BURAM  - REACHED 50 ITERATIONS', 30, 6
     1                  , 1)
C/
                     GOTO  7
   6                 CALL EPRINT
   7           CONTINUE
   8        CONTINUE
   9        CONTINUE
  10     CONTINUE
  11  CALL LEAVE
      RETURN
      END