      SUBROUTINE DB1RM1(NPTS, X, FN, MAXITR, ITOL, M, N, P, Q,
     1   DELK, NEWP, NEWQ, EN, QK, IEXT)
      INTEGER NPTS
      INTEGER MAXITR, ITOL, M, N, IEXT(NPTS)
      DOUBLE PRECISION X(NPTS), FN(NPTS), P(1), Q(1), DELK, NEWP(1)
      DOUBLE PRECISION NEWQ(1), EN(NPTS), QK(NPTS)
      INTEGER NITR, NEX, IMAX, IMIN, ILRG, L5RGXD
      INTEGER NERROR, IER, I
      DOUBLE PRECISION EPS, BND, D1MACH, DELNEW
      EPS = D1MACH(4)*10.0D0**ITOL
      CALL EXTRMD(NPTS, FN, NEX, IEXT, IMAX, IMIN, ILRG)
      BND = DABS(FN(ILRG))*EPS
      CALL DC5EQK(NPTS, X, FN, M, N, P, Q, QK, EN)
      DO  1 I = 1, NPTS
C/6S
C        IF (QK(I) .LE. 0.0D0) CALL SETERR(
C    1      35HDBURM1 - DENOMINATOR IS NONPOSITIVE, 35, 6, 2)
C/7S
         IF (QK(I) .LE. 0.0D0) CALL SETERR(
     1      'DBURM1 - DENOMINATOR IS NONPOSITIVE', 35, 6, 2)
C/
   1     CONTINUE
      CALL EXTRMD(NPTS, EN, NEX, IEXT, IMAX, IMIN, ILRG)
      DELK = DABS(EN(ILRG))
      DELNEW = DELK
      CALL MOVEFD(M+1, P, NEWP)
      CALL MOVEFD(N+1, Q, NEWQ)
      NITR = 0
         GOTO  3
   2     NITR = NITR+1
   3     IF (NITR .GE. MAXITR) GOTO  6
C   OUTPT3 (X,NPTS,P,Q,DELK,M,N,EN,IEXT,NEX)
         IF (DELK .GT. BND) GOTO 4
C/6S
C           CALL SETERR(38HDBURM1 - APPROXIMATION EQUALS FUNCTION, 39, 7
C    1         , 1)
C/7S
            CALL SETERR('DBURM1 - APPROXIMATION EQUALS FUNCTION', 39, 7
     1         , 1)
C/
            RETURN
C   TEST FOR OPTIMAL SOLUTION.
   4     IF (L5RGXD(NPTS, EN, NEX, IEXT, ILRG, ITOL) .GE. M+N+2) RETURN
         CALL DL5STP(NPTS, X, FN, QK, DELNEW, M, N, NEWP, NEWQ)
         IF (NERROR(IER) .NE. 0) CALL ERROFF
         CALL DC5EQK(NPTS, X, FN, M, N, NEWP, NEWQ, QK, EN)
         CALL EXTRMD(NPTS, EN, NEX, IEXT, IMAX, IMIN, ILRG)
         DELNEW = DABS(EN(ILRG))
         IF (DELK .GT. DELNEW) GOTO 5
C/6S
C           CALL SETERR(40HDBURM1 - NO IMPROVEMENT IN APPROXIMATION, 40,
C    1         8, 1)
C/7S
            CALL SETERR('DBURM1 - NO IMPROVEMENT IN APPROXIMATION', 40,
     1         8, 1)
C/
            RETURN
   5     CALL MOVEFD(M+1, NEWP, P)
         CALL MOVEFD(N+1, NEWQ, Q)
         DELK = DELNEW
         GOTO  2
C/6S
C  6  CALL SETERR(42HDBURM1 - REACHED MAXIMUM NO. OF ITERATIONS, 42, 9
C    1   , 1)
C/7S
   6  CALL SETERR('DBURM1 - REACHED MAXIMUM NO. OF ITERATIONS', 42, 9
     1   , 1)
C/
      RETURN
      END