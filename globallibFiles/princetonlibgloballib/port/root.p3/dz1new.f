      SUBROUTINE DZ1NEW(FUNC, N, X, EPS, F, XNEW, FNEW, FSAV, P, G
     1   , DX2, PMIN, ISMAX, MODE, NEWJAC, NOGOOD)
      INTEGER N
      EXTERNAL FUNC
      INTEGER ISMAX, MODE
      LOGICAL NEWJAC, NOGOOD
      DOUBLE PRECISION X(N), EPS, F(N), XNEW(N), FNEW(N), FSAV(N)
      DOUBLE PRECISION P(N), G(N), DX2, PMIN
      COMMON /DZ1COM/ P2, G2, XP, XG, RATSAV, XGMAX, X2, DX2OLD, DXTRY
     1   , RELERR, F2PREV, F2BEST, F2LAST, F2NORM, F2DMAX, SMALL, SING
     2   , GFLAG, VFLAG, JMAX, JCALL, ISLOW, JPRINT, NUMUPD, N2BIG
      INTEGER JMAX, JCALL, ISLOW, JPRINT, NUMUPD, N2BIG
      LOGICAL SMALL, SING, GFLAG, VFLAG
      DOUBLE PRECISION P2, G2, XP, XG, RATSAV, XGMAX
      DOUBLE PRECISION X2, DX2OLD, DXTRY, RELERR, F2PREV, F2BEST
      DOUBLE PRECISION F2LAST, F2NORM, F2DMAX
      INTEGER LASTM, NTMAX, NT, J, NERROR, NERR
      INTEGER I1SGN, I2SGN, MIN0
      REAL FLOAT
      LOGICAL STEEP, TOOFAR, FIRST, OKAY3, LTEMP
      DOUBLE PRECISION DNRM2, DZ1DEL, DDOT, DZ1DOT, DELTA, DX2SAV
      DOUBLE PRECISION F2GOAL, F2NEW, F2SAV, RATOLD, RATIO, RHO
      DOUBLE PRECISION TEMP, XGSAV, XPSAV, FW, FWORSE, PG
      DOUBLE PRECISION F2IN, F2DEL, CONVR, COS
      DOUBLE PRECISION DSQRT
      DATA FW/1.5D0/
      DATA RHO/0.7D0/
C OBTAIN NEW X-VECTOR AND F-VECTOR
C INPUTS
C FUNC IS SUBROUTINE, CALL FUNC(N,X,F).
C IT MUST RETURN F-VECTOR, GIVEN X-VECTOR.
C X IS CURRENT X-VECTOR
C EPS IS TOLERANCE.  SUCCESS MEANS 2-NORM OF FUNCTION .LE. EPS.
C F IS N-VECTOR OF VALUE OF FUNC AT X
C XNEW,FNEW,FSAV ARE SCRATCH ARRAYS
C IF (VFLAG) DO VALLEY SEARCH
C    P IS N-VECTOR, STEP ALONG VALLEY
C    G IS N-VECTOR, STEP INTO VALLEY
C IF ((NOT)VFLAG) DOGLEG STRATEGY
C    P IS N-VECTOR, NEWTON STEP
C    G IS N-VECTOR, STEEPEST-DESCENT STEP
C PMIN IS MINIMUM ALLOWABLE FRACTION OF P IN DX STEP
C ISMAX IS MAXIMUM VALUE OF ISLOW
C OUTPUTS
C IF SUCCESSFUL
C    NEW X AND F-VECTORS
C    XNEW, FNEW HAVE DX, DF-VECTORS
C    DX2 IS LENGTH OF DX-VECTOR
C IF UNSUCCESSFUL
C    X AND F-VECTORS ARE UNCHANGED
C MODE = STRATEGY MODE USED
C      = 1, STANDARD HYBRID
C      = 2, GRADIENT SEARCH
C      = 3, HYBRID SEARCH
C      = 4, VALLEY SEARCH
C NEWJAC = (DO NOT WANT TO USE OLD JACOBIAN ANY MORE)
C NOGOOD = (DID NOT GET NEW F THAT WAS ACCEPTABLE)
      NEWJAC = .FALSE.
      NOGOOD = .FALSE.
      LTEMP = .FALSE.
      FIRST = NUMUPD .EQ. 0
      IF (.NOT. FIRST) GOTO 1
         XG = DMIN1(XG, 1.0D0)
         DELTA = DMAX1(XG*G2, DX2OLD)
         GOTO  2
   1     DELTA = DZ1DEL(DX2OLD, XP, P2, G2, PMIN, F2NORM, F2LAST,
     1      F2BEST)
COKAY3=NUMUPD.LT.3 AND DELTA.GE.PMIN*P2      AND XG.GE.1.0D0
C  AND XG.GE.1.0D0
   2  OKAY3 = FIRST .AND. DELTA .GE. PMIN*P2
      LASTM = MODE
      MODE = 1
      NTMAX = 2
      IF (FIRST) NTMAX = 15
C  IF (DELTA.LT.0.1D0*P2)      SEARCH ALONG STEEPEST-DESCENT
C     MODE=2, NTMAX=2*N, DELTA=G2
      IF ((.NOT. FIRST) .OR. ISLOW .NE. ISMAX .AND. (.NOT. GFLAG) .OR. (
     1   .NOT. OKAY3)) GOTO 3
         MODE = 3
C DOGLEG SEARCH
         NTMAX = 15
   3  IF (.NOT. VFLAG) GOTO 6
         MODE = 4
         PG = DDOT(N, P, 1, G, 1)
         IF (P2 .LE. 0.1D0*G2) GOTO 4
            NTMAX = 15
            GOTO  5
   4        NTMAX = 2
   5     CONTINUE
   6  F2IN = F2NORM
      NT = 1
         GOTO  8
   7     NT = NT+1
   8     IF (NT .GT. NTMAX)GOTO  58
C USING SAME P, AND G,
C LOOK FOR ACCEPTABLE XNEW.
         IF (.NOT. FIRST) GOTO 9
            STEEP = DELTA .LT. 0.05D0*P2 .AND. MODE .NE. 3 .OR. LASTM
     1          .EQ. 2 .AND. ISLOW .EQ. 0
            GOTO  10
   9        STEEP = DELTA .LE. G2 .AND. DELTA .LT. 0.05D0*P2 .OR. XP
     1          .LE. 0.0D0 .AND. DELTA .LT. 0.05D0*P2 .AND. F2NORM
     2          .LT. RHO*F2LAST .AND. G2 .GT. 0.25D0*DX2OLD
  10     IF (STEEP .OR. DELTA .LT. PMIN*P2) DELTA = DMIN1(DELTA, G2)
C GET XP AND XG, DX=XP*P+XG*G
         IF (MODE .NE. 2) GOTO 11
            XP = 0.0D0
            XG = NT
            STEEP = .TRUE.
            DX2 = XG*G2
            GOTO  16
  11        IF (MODE .NE. 4) GOTO 14
               XG = 1.0D0
               IF (NT .NE. 1) GOTO 12
                  XP = 2.0D0
                  GOTO  13
  12              XP = 2.0D0*XP
                  IF (F2DEL .LT. 0.5D0) XP = 0.5D0*XP/DMAX1(0.1D0,
     1               F2DEL)
  13           DX2 = DSQRT(G2**2+2.0D0*XP*PG+(XP*P2)**2)
               GOTO  15
  14           CALL DZ1XPG(N, STEEP, DELTA, P, P2, PMIN, G, G2, XP, XG
     1            , DX2)
  15     CONTINUE
  16     IF (DX2 .GE. RELERR*X2) GOTO 23
            IF (.NOT. SMALL) GOTO 21
               IF ((.NOT. SING) .AND. FIRST) GOTO 17
                  NEWJAC = .TRUE.
                  GOTO  20
  17              MODE = 3
                  NTMAX = 15
                  XG = 1.0D0
                  PG = DDOT(N, P, 1, G, 1)
  18              IF ((.NOT. SMALL) .OR. NT .GE. NTMAX)GOTO  19
                     XP = PMIN*2.0D0**(NT-1)
                     NT = NT+1
                     DX2 = DSQRT(G2**2+2.0D0*XP*PMIN*PG+(XP*PMIN*P2)**2)
                     SMALL = DX2 .LT. RELERR*X2
                     GOTO  18
  19              CONTINUE
  20           CONTINUE
               GOTO  22
  21           SMALL = .TRUE.
  22        CONTINUE
            GOTO  24
  23        SMALL = .FALSE.
  24     DO  25 J = 1, N
            XNEW(J) = X(J)+XP*P(J)+XG*G(J)
  25        CONTINUE
         JCALL = JCALL+1
CGET NEW F
         CALL FUNC(N, XNEW, FNEW)
         IF (NERROR(NERR) .NE. 0) GOTO 26
            F2NEW = DNRM2(N, FNEW, 1)
            F2LAST = F2NORM
  26     IF (NERROR(NERR) .EQ. 0) GOTO 27
            CALL ERROFF
            F2NEW = 4.0*F2BEST
  27     IF (NT .GT. 1) RATOLD = RATIO
         RATIO = F2NEW/F2NORM
         IF (RATIO .LE. 1.0D0) GOTO 30
            IF (.NOT. FIRST) GOTO 28
               N2BIG = 1
               GOTO  29
  28           N2BIG = N2BIG+1
  29     CONTINUE
  30     IF (N2BIG .LT. 4) GOTO 31
            FWORSE = 1.0D0
            GOTO  32
  31        FWORSE = FW*F2BEST/F2IN
  32     F2DEL = DMIN1(10.0D0*F2IN, F2NEW)/F2IN
         IF (F2NEW .LE. 0.) GOTO 33
            COS = DZ1DOT(N, F, F2IN, FNEW, F2NEW)
            F2DEL = F2DEL*(F2DEL-2.0D0*COS)+1.0D0
            GOTO  34
  33        COS = 0.0D0
            F2DEL = 1.0D0
  34     F2DEL = DSQRT(DMAX1(0.0D0, F2DEL))
C SEE IF TENTATIVE MODE IS REASONABLE
         TOOFAR = .FALSE.
         IF (F2NEW .GT. EPS) GOTO 35
            MODE = 1
            GOTO  58
  35     IF (NT .NE. 1) GOTO 39
            IF (XP .GT. 0.0D0 .OR. RATIO .GT. 0.4D0) GOTO 36
               MODE = 2
               NTMAX = 2*N
               CONVR = (F2IN/F2NEW)**(1.0D0/FLOAT(N+1))-1.0D0
  36        IF ((MODE .NE. 4 .OR. RATIO .LE. FWORSE) .AND. (MODE .NE. 3
     1          .OR. RATIO .LE. 1.0D0)) GOTO 37
               MODE = 1
               NTMAX = 15
  37        IF (RATIO .GE. 1.0D0 .OR. (.NOT. OKAY3) .OR. RATIO .LE.
     1         0.98D0 .OR. F2DEL .GE. 0.1D0 .OR. MODE .EQ. 4) GOTO 38
               MODE = 3
               NTMAX = 15
C     IF (FIRST AND XP.LE.0.0D0 AND RATIO.GT.1.0D0) NEWJAC=.TRUE.
  38        CONTINUE
C DECIDE ON ACCEPTABILITY OF XNEW AND FNEW
  39     IF (MODE .NE. 2) GOTO 45
            IF (NT .NE. 1) GOTO 40
               I1SGN = 0
               GOTO  43
  40           IF (RATIO .LE. RATOLD) GOTO 41
                  I2SGN = 1
                  GOTO  42
  41              I2SGN = -1
  42        CONTINUE
C  COS .LE. 0.7
  43        TOOFAR = F2DEL .GT. 1.0D0 .OR. RATIO .GT. 1.0D0
            IF (I1SGN*I2SGN .LT. 0) NTMAX = NT
            IF (NT .LE. 1 .OR. TOOFAR) GOTO 44
               TEMP = (F2IN/F2NEW)**(1.0D0/FLOAT(N+NT))-1.0D0
               TOOFAR = TEMP .LT. 1.05D0*CONVR
               CONVR = TEMP
               I1SGN = I2SGN
  44        CONTINUE
  45     IF (MODE .EQ. 3) TOOFAR = F2NEW .GT. FWORSE*F2IN
         IF (MODE .NE. 4) GOTO 48
            IF (F2NORM .GT. 0.9D0*F2PREV) GOTO 46
               TOOFAR = F2NEW .GT. F2NORM
               GOTO  47
  46           TOOFAR = F2NEW .GT. FWORSE*F2IN
  47        TOOFAR = TOOFAR .OR. NT .EQ. 1 .AND. RATIO .GT. 0.5D0*
     1         RATSAV+0.5D0
            IF (F2DEL .GT. 0.7) NTMAX = NT
  48     IF (MODE .NE. 1) TOOFAR = TOOFAR .OR. F2DEL .GT. 1.0D0
         IF (NT .EQ. 1 .AND. TOOFAR) MODE = 1
         IF (MODE .NE. 1) GOTO 51
            F2GOAL = FWORSE*F2IN
            IF (F2BEST .LT. 0.1D0*F2PREV .OR. NT .EQ. 1 .AND. XP .LE.
     1         0.0D0) F2GOAL = F2IN
            TOOFAR = F2NEW .GT. DMAX1(EPS, F2GOAL)
            IF (.NOT. TOOFAR) GOTO 49
               DELTA = DMAX1(0.25D0, DMIN1(0.5D0, F2IN/F2NEW))*DX2
               GOTO  50
  49           F2DMAX = DMAX1(F2DMAX, F2DEL)
               NTMAX = NT
  50        CONTINUE
            GOTO  57
  51        IF (.NOT. TOOFAR) GOTO 54
               IF (MODE .NE. 3 .OR. (.NOT. OKAY3) .OR. DX2 .LE. 3.0*
     1            DX2OLD .OR. NT .LE. 1) GOTO 52
                  DELTA = DSQRT(DX2*DX2SAV)
                  OKAY3 = .FALSE.
                  LTEMP = .TRUE.
                  TOOFAR = .FALSE.
                  NTMAX = MIN0(NT+1, NTMAX)
                  GOTO  53
  52              GOTO  58
  53           CONTINUE
  54        IF (LTEMP) GOTO  7
C 2-LEVEL NEXT
            XPSAV = XP
            XGSAV = XG
            F2SAV = F2NEW
            F2NORM = F2NEW
            DX2SAV = DX2
            F2DMAX = DMAX1(F2DMAX, F2DEL)
            DO  55 J = 1, N
               FSAV(J) = FNEW(J)
  55           CONTINUE
            IF (MODE .NE. 3) GOTO 56
               IF (XP .GE. 0.99D0) GOTO  58
               DELTA = DMAX1(2.0D0*DX2, RELERR*X2)
               IF (F2DEL .LT. 0.5D0) DELTA = 0.5D0*DELTA/DMAX1(0.1D0,
     1            F2DEL)
               IF (.NOT. SING) DELTA = DMAX1(DELTA, PMIN*P2)
  56        CONTINUE
  57     IF (JCALL .GE. JMAX .OR. N2BIG .GE. 5 .OR. SMALL .AND. RATIO
     1       .GT. 0.98D0) GOTO  58
         GOTO  7
C END OF  FOR -LOOP ON NT
  58  IF (MODE .EQ. 1 .OR. (.NOT. TOOFAR)) GOTO 60
         XP = XPSAV
C RETURN SUCCESSFUL VALUES SAVED EARLIER
         XG = XGSAV
         DX2 = DX2SAV
         F2NORM = F2SAV
         DO  59 J = 1, N
            XNEW(J) = XP*P(J)+XG*G(J)
            X(J) = X(J)+XNEW(J)
            FNEW(J) = FSAV(J)-F(J)
            F(J) = FSAV(J)
  59        CONTINUE
         IF (NT .EQ. 2) MODE = 1
         GOTO  64
  60     IF (TOOFAR) GOTO 62
            F2NORM = F2NEW
C SUCCESSFUL MODE 1, OR MODE 2 OR 3, AND FELL
C OUT BOTTOM OF FOR-LOOP
            DO  61 J = 1, N
               TEMP = XNEW(J)
               XNEW(J) = XNEW(J)-X(J)
               X(J) = TEMP
               TEMP = FNEW(J)
               FNEW(J) = FNEW(J)-F(J)
               F(J) = TEMP
  61           CONTINUE
            GOTO  63
  62        NEWJAC = .TRUE.
C FAILED
            NOGOOD = .TRUE.
  63  CONTINUE
  64  IF (MODE .NE. 2 .AND. MODE .NE. 4 .OR. NT .LE. 3) GOTO 65
         DX2OLD = 0.0D0
         NEWJAC = .TRUE.
         GOTO  66
  65     DX2OLD = DX2
  66  NEWJAC = NEWJAC .OR. N2BIG .GE. 5
      IF (F2NORM .LT. F2IN) RATSAV = F2NORM/F2IN
      RETURN
      END
