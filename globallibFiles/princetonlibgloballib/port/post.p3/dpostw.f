      SUBROUTINE DPOSTW(J, F, R, I, L)
      INTEGER J, I
      REAL R
      LOGICAL L
      DOUBLE PRECISION F
      INTEGER MGQ, MAX0, K, M, N(100), IABS
      INTEGER KEEJAC, KMAX, MMAX, SAVEB, KINIT, MINIT
      INTEGER MAXIT
      REAL HFRACT, EGIVE, FLOAT
      LOGICAL USENFD, USENGJ, USENNS, ERPUTS, XPOLY
      DOUBLE PRECISION BETA, DBLE, GAMMA, DELTA, THETA, DSQRT
      INTEGER TEMP
      LOGICAL TEMP1
C/7
      SAVE K
C/
      DATA K/0/
      DATA THETA/1D0/
      DATA BETA/1D0/
      DATA GAMMA/1D0/
      DATA DELTA/0D0/
      DATA HFRACT/1E0/
      DATA EGIVE/1E+2/
      DATA KEEJAC/0/
      DATA MINIT/10/
      DATA MAXIT/50/
      DATA KMAX/10/
      DATA KINIT/4/
      DATA MMAX/15/
      DATA MGQ/0/
      DATA SAVEB/0/
      DATA XPOLY/.FALSE./
      DATA ERPUTS/.FALSE./
      DATA USENGJ/.FALSE./
      DATA USENNS/.FALSE./
      DATA USENFD/.FALSE./
      DATA N(1)/1/, N(2)/0/, N(3)/0/
C THE PARAMETER SETTING ROUTINE FOR POST.
C THE VARIABLES ARE
C J = 1.
C J = 2.
C J = 3.
C J = 4.
C J = 1001.
C J = 1002.
C J = 2001.
C J = 2002.
C J = 2003.
C J = 2004.
C J = 2005.
C J = 2006.
C J = 2007. 0 IMPLIES MGQ = K-1 BY DEFAULT.
C J = 2008. -1 DO NOT SAVE, 0 DEFAULT, +1 SAVE.
C J = 3001.
C J = 3002.
C J = 3003.
C J = 3004.
C J = 3005.
C J = 4001, ... , 4100.
      GOTO  62
C   EXPORT THE VARIABLES.
   1     F = THETA
         GOTO  63
   2     F = BETA
         GOTO  63
   3     F = GAMMA
         GOTO  63
   4     F = DELTA
         GOTO  63
   5     R = HFRACT
         GOTO  63
   6     R = EGIVE
         GOTO  63
   7     I = KEEJAC
         GOTO  63
   8     I = MINIT
         GOTO  63
   9     I = MAXIT
         GOTO  63
  10     I = KMAX
         GOTO  63
  11     I = KINIT
         GOTO  63
  12     I = MMAX
         GOTO  63
  13     IF (MGQ .NE. 0) GOTO 14
            I = K-1
            GOTO  15
  14        I = MGQ
  15     GOTO  63
  16     I = SAVEB
         GOTO  63
  17     L = XPOLY
         GOTO  63
  18     L = ERPUTS
         GOTO  63
  19     L = USENGJ
         GOTO  63
  20     L = USENNS
         GOTO  63
  21     L = USENFD
         GOTO  63
C POST VERSION NUMBER.
  22     F = 3D0
         GOTO  63
C SET THE VARIABLES TO THE DEFAULTS.
  23     THETA = 1D0
         BETA = 1
         GAMMA = 1
         DELTA = 0
         HFRACT = 1
         EGIVE = 1E+2
         KEEJAC = 0
         MINIT = 10
         MAXIT = 50
         KMAX = 10
         KINIT = 4
         MMAX = 15
C 0 IMPLIES MGQ = K-1, BY DEFAULT.
         MGQ = 0
         SAVEB = 0
         XPOLY = .FALSE.
         ERPUTS = .FALSE.
         USENGJ = .FALSE.
         USENNS = .FALSE.
         USENFD = .FALSE.
         CALL SETI(100, 0, N)
         N(1) = 1
C   IMPORT THE VARIABLES.
         GOTO  63
  24     THETA = F
         IF (THETA .EQ. 0.5) GOTO 25
            GAMMA = 1
            HFRACT = 1
            GOTO  29
  25        GAMMA = 2
            HFRACT = 0.5
            N(1) = 2
            N(2) = 4
            N(3) = 6
            M = 4
               GOTO  27
  26           M = M+1
  27           IF (M .GT. MMAX) GOTO  28
               N(M) = 2*N(M-2)
               GOTO  26
  28        CONTINUE
  29     GOTO  63
  30     BETA = F
         GOTO  63
  31     GAMMA = F
         GOTO  63
  32     DELTA = F
         GOTO  63
  33     HFRACT = R
         GOTO  63
  34     EGIVE = R
         GOTO  63
  35     KEEJAC = I
         GOTO  63
  36     MINIT = I
         GOTO  63
  37     MAXIT = I
         GOTO  63
  38     KMAX = I
         MMAX = KMAX+5
         GOTO  63
  39     KINIT = I
         GOTO  63
  40     MMAX = I
         GOTO  63
  41     MGQ = I
         GOTO  63
  42     SAVEB = I
         GOTO  63
  43     XPOLY = L
         GOTO  63
  44     ERPUTS = L
         IF (.NOT. ERPUTS) GOTO 45
            DELTA = 1
            GOTO  46
  45        DELTA = 0
  46     GOTO  63
  47     USENGJ = L
         GOTO  63
  48     USENNS = L
         GOTO  63
  49     USENFD = L
         GOTO  63
  50     TEMP1 = IABS(J) .GT. 4100
         IF (.NOT. TEMP1) TEMP1 = IABS(J) .LT. 4001
C/6S
C        IF (TEMP1) CALL SETERR(24HDPOSTW - J OUT OF BOUNDS, 24, 1, 2)
C/7S
         IF (TEMP1) CALL SETERR('DPOSTW - J OUT OF BOUNDS', 24, 1, 2)
C/
         IF (J .GE. 0) GOTO 60
            IF (N(2) .NE. 0) GOTO 51
               N(2) = DSQRT(2D0)*DBLE(FLOAT(N(1)))
C EXPORT N(ABS(J)-4000)
C ONLY N(1) IS GIVEN, USE SQRT(2) INCREASE.
               IF (N(2) .EQ. N(1)) N(2) = N(2)+1
               N(3) = DSQRT(2D0)*DBLE(FLOAT(N(2)))
               IF (N(3) .EQ. N(2)) N(3) = N(3)+1
               N(4) = 0
  51        TEMP = IABS(J)
            IF (N(TEMP-4000) .NE. 0) GOTO 59
               DO  57 K = 1, MMAX
C FILL IN THE MISSING N(M).
                  IF (N(K) .NE. 0) GOTO 56
                     IF (K .NE. 3) GOTO 53
                        DO  52 M = K, MMAX
                           N(M) = (N(2)*N(M-1))/MAX0(1, N(1))
  52                       CONTINUE
                        GOTO  55
  53                    DO  54 M = K, MMAX
                           N(M) = 2*N(M-2)
  54                       CONTINUE
  55                 GOTO  58
  56              CONTINUE
  57              CONTINUE
  58           CONTINUE
  59        TEMP = IABS(J)
            I = N(TEMP-4000)
            GOTO  61
  60        N(J-4000) = I
C IMPORT N(J-4000)
            IF (J-4000 .LT. 100) N(J-3999) = 0
  61     CONTINUE
         GOTO  63
  62     IF (J .EQ. 3005) GOTO  49
         IF (J .EQ. 3004) GOTO  48
         IF (J .EQ. 3003) GOTO  47
         IF (J .EQ. 3002) GOTO  44
         IF (J .EQ. 3001) GOTO  43
         IF (J .EQ. 2008) GOTO  42
         IF (J .EQ. 2007) GOTO  41
         IF (J .EQ. 2006) GOTO  40
         IF (J .EQ. 2005) GOTO  39
         IF (J .EQ. 2004) GOTO  38
         IF (J .EQ. 2003) GOTO  37
         IF (J .EQ. 2002) GOTO  36
         IF (J .EQ. 2001) GOTO  35
         IF (J .EQ. 1002) GOTO  34
         IF (J .EQ. 1001) GOTO  33
         IF (J .EQ. 4) GOTO  32
         IF (J .EQ. 3) GOTO  31
         IF (J .EQ. 2) GOTO  30
         IF (J .EQ. 1) GOTO  24
         IF (J .EQ. 0) GOTO  23
         IF (J .EQ. (-6000)) GOTO  22
         IF (J .EQ. (-3005)) GOTO  21
         IF (J .EQ. (-3004)) GOTO  20
         IF (J .EQ. (-3003)) GOTO  19
         IF (J .EQ. (-3002)) GOTO  18
         IF (J .EQ. (-3001)) GOTO  17
         IF (J .EQ. (-2008)) GOTO  16
         IF (J .EQ. (-2007)) GOTO  13
         IF (J .EQ. (-2006)) GOTO  12
         IF (J .EQ. (-2005)) GOTO  11
         IF (J .EQ. (-2004)) GOTO  10
         IF (J .EQ. (-2003)) GOTO  9
         IF (J .EQ. (-2002)) GOTO  8
         IF (J .EQ. (-2001)) GOTO  7
         IF (J .EQ. (-1002)) GOTO  6
         IF (J .EQ. (-1001)) GOTO  5
         IF (J .EQ. (-4)) GOTO  4
         IF (J .EQ. (-3)) GOTO  3
         IF (J .EQ. (-2)) GOTO  2
         IF (J .EQ. (-1)) GOTO  1
         GOTO  50
  63  RETURN
      END
