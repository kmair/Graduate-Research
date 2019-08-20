      SUBROUTINE Z1ONE(FUNC, Z1JAC, N, X, EPS, JJMAX, F2OUT,
     1   IPRINT, F, DX, DF, P, G, AUX, R, QT, KFLAG)
      INTEGER N
      EXTERNAL FUNC, Z1JAC
      INTEGER JJMAX, IPRINT, KFLAG
      REAL X(N), EPS, F2OUT, F(N), DX(N), DF(N)
      REAL P(N), G(N), AUX(N), R(N, N), QT(N, N)
      COMMON /Z1COM/ P2, G2, XP, XG, RATSAV, XGMAX, X2, DX2OLD, DXTRY,
     1   RELERR, F2PREV, F2BEST, F2LAST, F2NORM, F2DMAX, JMAX, JCALL,
     2   ISLOW, JPRINT, NUMUPD, N2BIG, SMALL, SING, GFLAG, VFLAG
      INTEGER JMAX, JCALL, ISLOW, JPRINT, NUMUPD, N2BIG
      REAL P2, G2, XP, XG, RATSAV, XGMAX
      REAL X2, DX2OLD, DXTRY, RELERR, F2PREV, F2BEST
      REAL F2LAST, F2NORM, F2DMAX
      LOGICAL SMALL, SING, GFLAG, VFLAG
      INTEGER ISMAX, IROLD, MAXSD, MIN0, NERROR, NERR
      INTEGER MODE, JUSED, MAX0, J1IMPR, J2IMPR, J
      REAL SNRM2, R1MACH, Z1CON, Z1DOT, COND, CONMAX
      REAL DX2, G2OLD, F2IMPR, PMIN, RATE1, RATE2
      REAL TEMP, COS, XPBIG, FLOAT, AMIN1, SQRT
      REAL AMAX1
      LOGICAL NEWJAC, LAST, FIRST, NOGOOD
      DATA ISMAX/3/
      DATA RATE1/0.98E0/
      DATA RATE2/0.95E0/
      DATA PMIN/1.E-4/
C SOLVE N NONLINEAR EQUATIONS F(X)=0
C INPUTS
C FUNC IS SUBROUTINE, CALL FUNC(N,X,F).
C IT MUST RETURN F-VECTOR, GIVEN X-VECTOR.
C Z1JAC IS SUBROUTINE, CALL Z1JAC(FUNC,N,X,F,DFDX,J).
C X IS N-VECTOR, GUESS AT ANSWER.
C EPS IS TOLERANCE.  SUCCESS MEANS 2-NORM OF FUNCTION .LE. EPS.
C JJMAX IS UPPER LIMIT ON NUMBER OF CALLS TO FUNC
C IPRINT = PRINTING LEVEL
C F,DX,DF,P,G,AUX,R,QT ARE SCRATCH ARRAYS
C OUTPUTS
C NEW X
C F2OUT = 2-NORM OF FUNC AT X
C F = FUNC-VECTOR AT X
C KFLAG=1   SUCCESSFUL
C       2   INITIAL X-VECTOR NO GOOD
C       3   TOO MANY CALLS TO FUNC
C       4   CONVERGENCE TOO SLOW
C       5   COULD NOT GET NEW JACOBIAN
C       6   DID NOT IMPROVE WITH NEW JACOBIAN
      CALL ENTSRC(IROLD, 1)
      JMAX = JJMAX
      MAXSD = MIN0(3*N, 15)
      JPRINT = IPRINT
      KFLAG = 0
      CALL FUNC(N, X, F)
      JCALL = 1
      IF (NERROR(NERR) .EQ. 0) F2NORM = SNRM2(N, F, 1)
      IF (NERROR(NERR) .EQ. 0) GOTO 1
         CALL ERROFF
         KFLAG = 2
         F2OUT = R1MACH(2)
         RETURN
   1  IF (F2NORM .GT. EPS) GOTO 2
         F2OUT = F2NORM
         RETURN
   2  RELERR = FLOAT(2*N)*R1MACH(4)
      CONMAX = AMIN1(1.E4, 0.01/R1MACH(4))
      F2BEST = F2NORM
      F2PREV = 2.0E0*F2BEST
      DX2OLD = 0.0E0
      XG = 1.0E0
      MODE = 2
      LAST = .FALSE.
      SMALL = .FALSE.
C GET NEW JACOBIAN MATRIX R AND CONTINUE
   3     IF (F2BEST .LE. RATE2*F2PREV) GOTO 5
            ISLOW = ISLOW+1
C SLOW CONVERGENCE
            IF (ISLOW .LE. ISMAX) GOTO 4
               KFLAG = 4
               GOTO  26
   4        LAST = LAST .OR. ISLOW .EQ. ISMAX .OR. F2DMAX .LT. 0.3E0
            GFLAG = XPBIG .LE. 0.0E0
            GOTO  6
   5        ISLOW = 0
C ACCEPTABLE CONVERGENCE
            GFLAG = .FALSE.
   6     X2 = SNRM2(N, X, 1)
         DXTRY = SQRT(R1MACH(4))*AMAX1(1.0E0, X2)
C        *** G2 IS NOT DEFINED WHEN JCALL .LE. 1 ***
         IF (JCALL .LE. 1) GO TO 61
            IF (G2 .GT. 0.0E0) DXTRY = AMIN1(DXTRY, G2)
   61    JUSED = 0
         CALL Z1JAC(FUNC, N, X, F, R, JUSED)
         JCALL = JCALL+MAX0(0, JUSED)
         IF (NERROR(NERR) .EQ. 0) GOTO 7
            CALL ERROFF
            KFLAG = 5
            GOTO  26
   7     NUMUPD = 0
         J1IMPR = 0
         J2IMPR = 0
         F2IMPR = F2BEST
         F2PREV = F2BEST
         F2DMAX = 0.0E0
         FIRST = .TRUE.
         VFLAG = .FALSE.
         XPBIG = 0.0E0
         RATSAV = 1.0E0
         N2BIG = 0
C QR FACTORIZATION OF JACOBIAN
         CALL Z1FAC(N, R, QT)
C LOWER BOUND ON CONDITION NUMBER
         COND = Z1CON(N, R)
         IF (.NOT. LAST) GOTO 8
            SING = COND .GT. 0.5E0*R1MACH(2)
            GOTO  9
   8        SING = COND .GT. CONMAX
   9     CONTINUE
C USE CURRENT JACOBIAN APPROXIMATION AS LONG AS IT WORKS.
C GET P=NEWTON STEP AND G=STEEPEST-DESCENT STEP
            CALL Z1PG(N, F, F2NORM, QT, R, P, G, P2, G2, SING, AUX)
            IF ((.NOT. FIRST) .OR. G2 .GT. RELERR*X2) GOTO 11
               IF (G2 .GT. 0.0E0 .AND. ((.NOT. SMALL) .OR. (.NOT. SING))
     1            ) GOTO 10
                  KFLAG = 6
                  GOTO  24
  10        CONTINUE
  11        IF (FIRST) GOTO 14
               COS = Z1DOT(N, G, G2, DX, G2OLD)
               IF (MODE .EQ. 2 .AND. COS .GT. SQRT(1.0E0-1.0E0/FLOAT(N))
     1            ) GOTO  24
C AND XG.GE.1.0E0
               VFLAG = VFLAG .AND. COS .LT. 0.3E0 .AND. COS .GT. (-
     1            0.9E0) .AND. NUMUPD .LT. 3 .AND. G2 .GT. 0.25E0*G2OLD
     2             .AND. F2DMAX .LT. 0.3E0 .AND. F2NORM .GT. RATE1*
     3            F2BEST
               IF (.NOT. VFLAG) GOTO 13
                  DO  12 J = 1, N
C AVERAGE OF TWO GS MAY BE DOWN THE VALLEY
                     G2 = 0.5E0*XG
                     TEMP = G2*(G(J)+DX(J))
                     G(J) = G2*(G(J)-DX(J))
                     P(J) = TEMP
  12                 CONTINUE
                  G2 = SNRM2(N, G, 1)
                  P2 = SNRM2(N, P, 1)
  13           CONTINUE
C GET ACCEPTABLE NEW VALUES OF X, F
  14        CALL Z1NEW(FUNC, N, X, EPS, F, DX, DF, AUX, P, G, DX2, PMIN,
     1         ISMAX, MODE, NEWJAC, NOGOOD)
            IF (F2NORM .GT. EPS) GOTO 15
               KFLAG = 1
               GOTO  24
  15        XPBIG = AMAX1(XP, XPBIG)
            IF (JCALL .LT. JMAX) GOTO 16
               KFLAG = 3
               GOTO  24
  16        IF ((.NOT. FIRST) .OR. (.NOT. NEWJAC) .OR. (.NOT. NOGOOD))
     1          GOTO 17
               KFLAG = 6
               GOTO  24
  17        J1IMPR = J1IMPR+1
            IF (F2NORM .GE. F2BEST) GOTO 18
               IF (F2NORM .LT. RATE1*F2BEST) J1IMPR = 0
               F2BEST = F2NORM
  18        J2IMPR = J2IMPR+1
            IF (F2NORM .GE. RATE2*F2IMPR) GOTO 19
               F2IMPR = F2NORM
               J2IMPR = 0
  19        IF (J2IMPR .GT. 4 .OR. NEWJAC .OR. J1IMPR .GT. 4 .OR. SMALL
     1          .AND. (.NOT. FIRST) .OR. XPBIG .LE. 0.0E0 .AND. NUMUPD
     2          .GT. MAXSD) GOTO  24
            IF (.NOT. SMALL) GOTO 20
               SING = COND .GT. 0.5E0*R1MACH(2)
               IF (SING) GOTO  24
               GOTO  22
  20           CALL Z1MV(N, QT, R, DX, AUX, P)
C RANK-ONE UPDATE TO APPROXIMATE JACOBIAN
C P=Q*R*DX
               DO  21 J = 1, N
                  DF(J) = (DF(J)-P(J))/DX2
                  DX(J) = DX(J)/DX2
  21              CONTINUE
               CALL Z1UPD(N, QT, R, DF, DX, AUX)
               COND = Z1CON(N, R)
               SING = COND .GT. CONMAX
               FIRST = .FALSE.
               NUMUPD = NUMUPD+1
  22        VFLAG = (.NOT. VFLAG) .AND. (XP .LE. 0.01E0 .AND. F2NORM
     1          .GT. RATE2*F2LAST)
C SAVE G FOR NEXT TIME
            DO  23 J = 1, N
               DX(J) = G(J)
  23           CONTINUE
            G2OLD = G2
            GOTO  9
  24     IF (KFLAG .EQ. 0) GOTO 25
            IF (LAST .OR. KFLAG .NE. 4) GOTO  26
            LAST = .TRUE.
            KFLAG = 0
  25     CONTINUE
         GOTO  3
  26  F2OUT = F2NORM
      CALL RETSRC(IROLD)
      RETURN
      END
