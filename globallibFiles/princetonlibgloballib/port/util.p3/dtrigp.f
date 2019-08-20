      DOUBLE PRECISION FUNCTION DTRIGP(N, ALPHA, BETA, THETA)
      INTEGER N
      DOUBLE PRECISION ALPHA(1), BETA(1), THETA
      INTEGER K
      DOUBLE PRECISION C, S, MU, GK, GK1, DK
      DOUBLE PRECISION DK1, DCOS, DSIN
C  DTRIGP IS A DOUBLE PRECISION FORTRAN FUNCTION WHICH RETURNS THE
C  VALUE OF A TRIGONOMETRIC POLYNOMIAL, P(THETA), OF DEGREE N,
C  WHERE P(THETA) = SUM ALPHA(K)COS(K THETA) + BETA(K)SIN(K THETA).
C  N      - THE DEGREE OF P(X).
C  ALPHA  - THE N+1 COEFFICIENTS OF THE COSINE TERMS.
C  BETA   - THE N+1 COEFFICIENTS OF THE SINE TERMS.
C           BETA(1) IS NOT USED.
C  THETA  - THE ARGUMENT AT WHICH P(THETA) IS TO BE EVALUATED.
C  ERROR STATES-
C       1 - INVALID DEGREE.
      IF (N .GE. 0) GOTO 1
C/6S
C        CALL SETERR(23HDTRIGP - INVALID DEGREE, 23, 1, 2)
C/7S
         CALL SETERR('DTRIGP - INVALID DEGREE', 23, 1, 2)
C/
         GOTO  11
   1     IF (N .NE. 0) GOTO 2
            DTRIGP = ALPHA(1)
            GOTO  10
   2        C = DCOS(THETA)
            S = DSIN(THETA)
            IF (S .LT. 0.0D0) GOTO 3
               MU = C/(S+1.0D0)
               GOTO  4
   3           MU = (-C)/(1.0D0-S)
   4        GK1 = ALPHA(N+1)
            DK1 = BETA(N+1)
            K = N
               GOTO  6
   5           K = K-1
   6           IF (K .LT. 2)GOTO  9
               GK = C*GK1+S*DK1
               IF (S .LT. 0.0D0) GOTO 7
                  DK = MU*(GK+DK1)-GK1
                  GOTO  8
   7              DK = MU*(GK-DK1)+GK1
   8           GK1 = GK+ALPHA(K)
               DK1 = DK+BETA(K)
               GOTO  5
   9        DTRIGP = ALPHA(1)+C*GK1+S*DK1
  10  CONTINUE
  11  RETURN
      END
