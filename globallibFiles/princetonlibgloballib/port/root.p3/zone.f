      SUBROUTINE ZONE(FUNC, N, X, EPS, JMAX, F2NORM)
      INTEGER N
      EXTERNAL FUNC
      INTEGER JMAX
      REAL X(N), EPS, F2NORM
      COMMON /CSTAK/ D
      DOUBLE PRECISION D(500)
      EXTERNAL Z1JAC
      INTEGER IFF, ISTKGT, IDX, IDF, IPP, IGG
      INTEGER IAUX, IRMAT, IQMAT, IPRINT, KFLAG
      REAL R(1000)
      EQUIVALENCE (D(1), R(1))
C SOLVE N NONLINEAR EQUATIONS F(X)=0
C INPUTS
C FUNC IS SUBROUTINE, CALL FUNC(N,X,F).
C IT MUST RETURN F-VECTOR, GIVEN X-VECTOR.
C Z1JAC IS SUBROUTINE TO CALCULATE JACOBIAN
C X IS N-VECTOR, GUESS AT ANSWER.
C EPS IS TOLERANCE.  SUCCESS MEANS 2-NORM OF FUNCTION .LE. EPS.
C JMAX IS UPPER LIMIT ON NUMBER OF CALLS TO FUNC
C OUTPUTS
C X IS REPLACED BY NEW GUESS
C F2NORM IS 2-NORM OF FUNC AT X.
C/6S
C     IF (N .LT. 1) CALL SETERR(28H ZONE - N MUST BE AT LEAST 1, 28, 1
C    1   , 2)
C/7S
      IF (N .LT. 1) CALL SETERR(' ZONE - N MUST BE AT LEAST 1', 28, 1
     1   , 2)
C/
      IFF = ISTKGT(N*(2*N+6), 3)
      IDX = IFF+N
      IDF = IDX+N
      IPP = IDF+N
      IGG = IPP+N
      IAUX = IGG+N
      IRMAT = IAUX+N
      IQMAT = IRMAT+N*N
      IPRINT = 0
      CALL Z1ONE(FUNC, Z1JAC, N, X, EPS, JMAX, F2NORM, IPRINT, R(IFF), R
     1   (IDX), R(IDF), R(IPP), R(IGG), R(IAUX), R(IRMAT), R(IQMAT),
     2   KFLAG)
      CALL ISTKRL(1)
      IF (KFLAG .NE. 2) GOTO 1
C/6S
C        CALL SETERR(32H ZONE - INITIAL X-VECTOR NO GOOD, 32, 2, 1)
C/7S
         CALL SETERR(' ZONE - INITIAL X-VECTOR NO GOOD', 32, 2, 1)
C/
         GOTO  8
   1     IF (KFLAG .NE. 3) GOTO 2
C/6S
C           CALL SETERR(30H ZONE - TOO MANY CALLS TO FUNC, 30, 3, 1)
C/7S
            CALL SETERR(' ZONE - TOO MANY CALLS TO FUNC', 30, 3, 1)
C/
            GOTO  7
   2        IF (KFLAG .NE. 4) GOTO 3
C/6S
C              CALL SETERR(28H ZONE - CONVERGENCE TOO SLOW, 28, 4, 1)
C/7S
               CALL SETERR(' ZONE - CONVERGENCE TOO SLOW', 28, 4, 1)
C/
               GOTO  6
   3           IF (KFLAG .NE. 5) GOTO 4
C/6S
C                 CALL SETERR(34H ZONE - COULD NOT GET NEW JACOBIAN, 34,
C    1               5, 1)
C/7S
                  CALL SETERR(' ZONE - COULD NOT GET NEW JACOBIAN', 34,
     1               5, 1)
C/
                  GOTO  5
C/6S
C  4              IF (KFLAG .EQ. 6) CALL SETERR(
C    1               41H ZONE - DID NOT IMPROVE WITH NEW JACOBIAN, 41, 6
C    2               , 1)
C/7S
   4              IF (KFLAG .EQ. 6) CALL SETERR(
     1               ' ZONE - DID NOT IMPROVE WITH NEW JACOBIAN', 41, 6
     2               , 1)
C/
   5        CONTINUE
   6     CONTINUE
   7  CONTINUE
   8  RETURN
      END
