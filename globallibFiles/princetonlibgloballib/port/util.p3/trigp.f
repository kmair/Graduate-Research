      REAL FUNCTION TRIGP(N, ALPHA, BETA, THETA)
      INTEGER N
      REAL ALPHA(1), BETA(1), THETA
      INTEGER K
      REAL C, S, MU, GK, GK1, DK
      REAL DK1, COS, SIN
C  TRIGP IS A REAL FORTRAN FUNCTION WHICH RETURNS THE
C  VALUE OF A TRIGONOMETRIC POLYNOMIAL, P(THETA), OF DEGREE N,
C  WHERE P(THETA) = SUM ALPHA(K)COS(K THETA) + BETA(K)SIN(K THETA).
C  N      - THE DEGREE OF P(X).
C  ALPHA  - THE N+1 COEFFICIENTS OF THE COSINE TERMS.
C  BETA   - THE N+1 COEFFICIENTS OF THE SINE TERMS.
C           BETA(1) IS NOT USED.
C  THETA  - THE ARGUMENT AT WHICH P(THETA) IS TO BE EVALUATED.
C  ERROR STATES-
C       1 - INVALID DEGREE.
C/6S
C     IF (N .LT. 0) CALL SETERR(22HTRIGP - INVALID DEGREE, 22, 1, 2)
C/7S
      IF (N .LT. 0) CALL SETERR('TRIGP - INVALID DEGREE', 22, 1, 2)
C/
      IF (N .NE. 0) GOTO 1
         TRIGP = ALPHA(1)
         GOTO  9
   1     C = COS(THETA)
         S = SIN(THETA)
         IF (S .LT. 0.0) GOTO 2
            MU = C/(S+1.0)
            GOTO  3
   2        MU = (-C)/(1.0-S)
   3     GK1 = ALPHA(N+1)
         DK1 = BETA(N+1)
         K = N
            GOTO  5
   4        K = K-1
   5        IF (K .LT. 2)GOTO  8
            GK = C*GK1+S*DK1
            IF (S .LT. 0.0) GOTO 6
               DK = MU*(GK+DK1)-GK1
               GOTO  7
   6           DK = MU*(GK-DK1)+GK1
   7        GK1 = GK+ALPHA(K)
            DK1 = DK+BETA(K)
            GOTO  4
   8     TRIGP = ALPHA(1)+C*GK1+S*DK1
   9  RETURN
      END
