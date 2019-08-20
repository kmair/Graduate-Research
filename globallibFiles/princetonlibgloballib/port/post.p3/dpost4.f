      SUBROUTINE DPOST4(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT,
     1   AF, BC, D, THETA, KEEJAC, MINIT, MAXIT, MGQ, BETA, GAMMA,
     2   DELTA, N, MMAX, HFRACT, EGIVE, SAVEB, KMAX, XPOLY, KINIT,
     3   ERROR, ERRPAR, ERPUTS, INMI, SCALE, HANDLE)
      INTEGER MMAX
      EXTERNAL AF, BC, D, ERROR, INMI, SCALE
      EXTERNAL HANDLE
      INTEGER NU, K, NX, NV, KEEJAC, MINIT
      INTEGER MAXIT, MGQ, N(MMAX), SAVEB, KMAX, KINIT
      REAL HFRACT, EGIVE, ERRPAR(2)
      LOGICAL XPOLY, ERPUTS
      DOUBLE PRECISION U(1), X(1), V(1), TSTART, TSTOP, DT
      DOUBLE PRECISION THETA, BETA, GAMMA, DELTA
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      COMMON /DPOSTF/ FAILED
      LOGICAL FAILED
      COMMON /D90STY/ WV, RV, IV, LV
      INTEGER IV(40)
      REAL RV(30)
      LOGICAL LV(20)
      DOUBLE PRECISION WV(30)
      COMMON /D90STV/ IEU
      INTEGER IEU
      COMMON /D90STT/ TGOOD
      DOUBLE PRECISION TGOOD
      COMMON /D90STR/ STATS
      INTEGER STATS(8)
      COMMON /D90STQ/ IXGQ, IWGQ, MGQQ
      INTEGER IXGQ, IWGQ, MGQQ
      COMMON /D90STK/ NUU, NVV, KK, NXX
      INTEGER NUU, NVV, KK, NXX
      COMMON /D90STB/ IGSSIS, SET
      INTEGER IGSSIS
      LOGICAL SET
      COMMON /D9OSTT/ TC, DTC
      DOUBLE PRECISION TC, DTC
      COMMON /D9OSTS/ IMEM
      INTEGER IMEM(4)
      COMMON /D9OSTM/ THETAC, EGIVEC, MINITC, MAXITC, KEEACC
      INTEGER MINITC, MAXITC, KEEACC
      REAL EGIVEC
      DOUBLE PRECISION THETAC
      COMMON /D9OSTL/ ERPTSC
      LOGICAL ERPTSC
      COMMON /D9OSTK/ IUTETA, IVTETA, IUT, IVT
      INTEGER IUTETA, IVTETA, IUT, IVT
      COMMON /D9OSTJ/ IJP, IB, IAFB, IALFA, IBETA, IGAMMA, ID4, ID5,
     1   IORDER, IBC, IEQS, IAA, IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT,
     2   IDMAT, IDIAG, IDPVOT
      INTEGER IJP(3), IB(3), IAFB(3), IALFA(3), IBETA(3), IGAMMA(3)
      INTEGER ID4(3), ID5(3), IORDER, IBC, IEQS, IAA
      INTEGER IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT
      INTEGER IDMAT, IDIAG, IDPVOT
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE
      LOGICAL GETJAC, SEPATE
      DOUBLE PRECISION TJ, DTJ
      COMMON /D9OSTF/ FNUM
      INTEGER FNUM
      EXTERNAL D9OSTH, D9OSTN, D9OSTP, D9OSTA, D9OSTB, D9OSTD
      EXTERNAL D9OSTE
      INTEGER ISTKGT, NERROR, I, NERR, IS(1000)
      REAL RS(1000)
      LOGICAL LS(1000)
      DOUBLE PRECISION DABS, WS(500)
      INTEGER TEMP, TEMP2
      LOGICAL TEMP1
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C THE BOTTOM LEVEL OF POSTS.
C SCRATCH SPACE ALLOCATED -
C     S(DPOST4) = 10*NU**2 + 8*NU + 2*NU*(NV+1) + S(D9OSTS) +
C                 IF ( KEEPJAC > 0 ) { 2*K*MGQ*(NX-2*K+1) }
C LONG REAL WORDS +
C                 MMAX
C INTEGER WORDS.
C U(NX-K,NU),X(NX),V(NV).
C THE PORT LIBRARY STACK AND ITS ALIASES.
      IF (TSTART .EQ. TSTOP) RETURN
      CALL ENTER(1)
C CHECK THE INPUT FOR ERRORS.
C/6S
C     IF (NU .LT. 0) CALL SETERR(16HDPOST4 - NU.LT.0, 16, 1, 2)
C     IF (NV .LT. 0) CALL SETERR(16HDPOST4 - NV.LT.0, 16, 2, 2)
C/7S
      IF (NU .LT. 0) CALL SETERR('DPOST4 - NU.LT.0', 16, 1, 2)
      IF (NV .LT. 0) CALL SETERR('DPOST4 - NV.LT.0', 16, 2, 2)
C/
      TEMP1 = NU .EQ. NV
      IF (TEMP1) TEMP1 = NV .EQ. 0
C/6S
C     IF (TEMP1) CALL SETERR(16HDPOST4 - NU=NV=0, 16, 3, 2)
C/7S
      IF (TEMP1) CALL SETERR('DPOST4 - NU=NV=0', 16, 3, 2)
C/
      TEMP1 = NU .GT. 0
      IF (TEMP1) TEMP1 = K .LT. 2
C/6S
C     IF (TEMP1) CALL SETERR(32HDPOST4 - K.LT.2 WHEN NU POSITIVE, 32, 4,
C    1   2)
C/7S
      IF (TEMP1) CALL SETERR('DPOST4 - K.LT.2 WHEN NU POSITIVE', 32, 4,
     1   2)
C/
      TEMP1 = NU .GT. 0
      IF (TEMP1) TEMP1 = NX .LT. 2*K
C/6S
C     IF (TEMP1) CALL SETERR(35HDPOST4 - NX.LT.2*K WHEN NU POSITIVE, 35,
C    1   5, 2)
C     IF (TSTART+DT .EQ. TSTART) CALL SETERR(
C    1   31HDPOST4 - INPUT VALUE OF DT IS 0, 31, 6, 2)
C     IF ((DT/DABS(DT))*(TSTOP-TSTART) .LE. 0D0) CALL SETERR(
C    1   45HDPOST4 - INPUT VALUE OF DT HAS THE WRONG SIGN, 45, 7, 2)
C/7S
      IF (TEMP1) CALL SETERR('DPOST4 - NX.LT.2*K WHEN NU POSITIVE', 35,
     1   5, 2)
      IF (TSTART+DT .EQ. TSTART) CALL SETERR(
     1   'DPOST4 - INPUT VALUE OF DT IS 0', 31, 6, 2)
      IF ((DT/DABS(DT))*(TSTOP-TSTART) .LE. 0D0) CALL SETERR(
     1   'DPOST4 - INPUT VALUE OF DT HAS THE WRONG SIGN', 45, 7, 2)
C/
C ???
      TEMP1 = THETA .LT. 0D0
      IF (.NOT. TEMP1) TEMP1 = THETA .GT. 1D0
C/6S
C     IF (TEMP1) CALL SETERR(27HDPOST4 - THETA NOT IN (0,1), 27, 8, 2)
C/7S
      IF (TEMP1) CALL SETERR('DPOST4 - THETA NOT IN (0,1)', 27, 8, 2)
C/
C ???
      TEMP1 = KEEJAC .LT. 0
      IF (.NOT. TEMP1) TEMP1 = KEEJAC .GT. 5
C/6S
C     IF (TEMP1) CALL SETERR(37HDPOST4 - KEEPJAC NOT ONE OF (0,...,5),
C    1   37, 9, 2)
C     IF (MINIT .LT. 1) CALL SETERR(19HDPOST4 - MINIT.LT.1, 19, 10, 2)
C     IF (MAXIT .LT. 1) CALL SETERR(19HDPOST4 - MAXIT.LT.1, 19, 11, 2)
C/7S
      IF (TEMP1) CALL SETERR('DPOST4 - KEEPJAC NOT ONE OF (0,...,5)',
     1   37, 9, 2)
      IF (MINIT .LT. 1) CALL SETERR('DPOST4 - MINIT.LT.1', 19, 10, 2)
      IF (MAXIT .LT. 1) CALL SETERR('DPOST4 - MAXIT.LT.1', 19, 11, 2)
C/
      TEMP1 = NU .GT. 0
      IF (TEMP1) TEMP1 = MGQ .LT. 1
C/6S
C     IF (TEMP1) CALL SETERR(37HDPOST4 - MGQ.LT.1 WHEN NU IS POSITIVE,
C    1   37, 12, 2)
C     IF (KMAX .LT. 1) CALL SETERR(18HDPOST4 - KMAX.LT.1, 18, 13, 2)
C     IF (KINIT .LT. 1) CALL SETERR(19HDPOST4 - KINIT.LT.1, 19, 14, 2)
C/7S
      IF (TEMP1) CALL SETERR('DPOST4 - MGQ.LT.1 WHEN NU IS POSITIVE',
     1   37, 12, 2)
      IF (KMAX .LT. 1) CALL SETERR('DPOST4 - KMAX.LT.1', 18, 13, 2)
      IF (KINIT .LT. 1) CALL SETERR('DPOST4 - KINIT.LT.1', 19, 14, 2)
C/
      IF (NU .LE. 0) GOTO 3
         DO  1 I = 1, K
C/6S
C           IF (X(I) .NE. X(1)) CALL SETERR(
C    1         35HDPOST4 - X(1) NOT OF MULTIPLICITY K, 35, 15, 2)
C/7S
            IF (X(I) .NE. X(1)) CALL SETERR(
     1         'DPOST4 - X(1) NOT OF MULTIPLICITY K', 35, 15, 2)
C/
            TEMP = NX-K+I
C/6S
C           IF (X(TEMP) .NE. X(NX)) CALL SETERR(
C    1         36HDPOST4 - X(NX) NOT OF MULTIPLICITY K, 36, 16, 2)
C/7S
            IF (X(TEMP) .NE. X(NX)) CALL SETERR(
     1         'DPOST4 - X(NX) NOT OF MULTIPLICITY K', 36, 16, 2)
C/
   1        CONTINUE
         TEMP = NX-K
         DO  2 I = K, TEMP
C/6S
C           IF (X(I) .GT. X(I+1)) CALL SETERR(
C    1         34HDPOST4 - X NOT MONOTONE INCREASING, 34, 17, 2)
C/7S
            IF (X(I) .GT. X(I+1)) CALL SETERR(
     1         'DPOST4 - X NOT MONOTONE INCREASING', 34, 17, 2)
C/
            IF (I+K .GT. NX) GOTO  2
            TEMP2 = I+K
C/6S
C           IF (X(TEMP2) .LE. X(I)) CALL SETERR(
C    1         34HDPOST4 - X NOT MONOTONE INCREASING, 34, 17, 2)
C/7S
            IF (X(TEMP2) .LE. X(I)) CALL SETERR(
     1         'DPOST4 - X NOT MONOTONE INCREASING', 34, 17, 2)
C/
   2        CONTINUE
C/6S
C  3  IF (BETA .LE. 0D0) CALL SETERR(19HDPOST4 - BETA .LE.0, 19, 19, 2)
C     IF (GAMMA .LE. 0D0) CALL SETERR(20HDPOST4 - GAMMA .LE.0, 20, 20, 2
C    1   )
C     IF (DELTA .LT. 0D0) CALL SETERR(20HDPOST4 - DELTA .LT.0, 20, 21, 2
C    1   )
C     IF (BETA+GAMMA-DELTA .LE. 0D0) CALL SETERR(
C    1   30HDPOST4 - BETA+GAMMA-DELTA.LE.0, 30, 22, 2)
C     IF (MMAX .LT. KMAX+2) CALL SETERR(23HDPOST4 - MMAX.LT.KMAX+2, 23
C    1   , 23, 2)
C     IF (N(1) .LT. 1) CALL SETERR(18HDPOST4 - N(1).LT.1, 18, 24, 2)
C/7S
   3  IF (BETA .LE. 0D0) CALL SETERR('DPOST4 - BETA .LE.0', 19, 19, 2)
      IF (GAMMA .LE. 0D0) CALL SETERR('DPOST4 - GAMMA .LE.0', 20, 20, 2
     1   )
      IF (DELTA .LT. 0D0) CALL SETERR('DPOST4 - DELTA .LT.0', 20, 21, 2
     1   )
      IF (BETA+GAMMA-DELTA .LE. 0D0) CALL SETERR(
     1   'DPOST4 - BETA+GAMMA-DELTA.LE.0', 30, 22, 2)
      IF (MMAX .LT. KMAX+2) CALL SETERR('DPOST4 - MMAX.LT.KMAX+2', 23
     1   , 23, 2)
      IF (N(1) .LT. 1) CALL SETERR('DPOST4 - N(1).LT.1', 18, 24, 2)
C/
      DO  4 I = 2, MMAX
C/6S
C        IF (N(I) .LE. N(I-1)) CALL SETERR(
C    1      37HDPOST4 - N IS NOT MONOTONE INCREASING, 37, 25, 2)
C/7S
         IF (N(I) .LE. N(I-1)) CALL SETERR(
     1      'DPOST4 - N IS NOT MONOTONE INCREASING', 37, 25, 2)
C/
   4     CONTINUE
C/6S
C     IF (HFRACT .LE. 0.) CALL SETERR(20HDPOST4 - HFRACT.LE.0, 20, 26, 2
C    1   )
C     IF (EGIVE .LT. 0.) CALL SETERR(21HDPOST4 - EGIVE .LT. 0, 21, 27, 2
C    1   )
C/7S
      IF (HFRACT .LE. 0.) CALL SETERR('DPOST4 - HFRACT.LE.0', 20, 26, 2
     1   )
      IF (EGIVE .LT. 0.) CALL SETERR('DPOST4 - EGIVE .LT. 0', 21, 27, 2
     1   )
C/
      ERPTSC = ERPUTS
      THETAC = THETA
      MINITC = MINIT
      MAXITC = MAXIT
      KEEACC = KEEJAC
      TEMP1 = KEEJAC .EQ. 1
      IF (TEMP1) TEMP1 = MAXIT .EQ. 1
      IF (TEMP1) KEEACC = 0
      IF (SAVEB .LE. 0) GOTO 5
         IGSSIS = ISTKGT(2*K*MGQ*(NX-2*K+1), 4)
         GOTO  10
   5     IF (SAVEB .GE. 0) GOTO 6
            IGSSIS = 1
            GOTO  9
   6        TEMP1 = NU .GT. 0
            IF (TEMP1) TEMP1 = KEEACC .GT. 1
            IF (.NOT. TEMP1) GOTO 7
               IGSSIS = ISTKGT(2*K*MGQ*(NX-2*K+1), 4)
               GOTO  8
   7           IGSSIS = 1
   8        CONTINUE
   9  CONTINUE
  10  SET = .FALSE.
      IF (KEEACC .LE. 1) GOTO 11
         SEPATE = .TRUE.
         GOTO  12
  11     SEPATE = .FALSE.
  12  IF (KEEACC .LT. 3) GOTO 13
         GETJAC = .TRUE.
         TJ = TSTART
         GOTO  18
  13     IF (KEEACC .NE. 2) GOTO 14
            TJ = TSTOP
C CANNOT BE TSTART.
            GOTO  17
  14        IF (THETA .LE. 0.5) GOTO 15
               TJ = TSTART
C CANNOT BE TSTART+THETA*DT/N.
               GOTO  16
  15           TJ = TSTOP
  16     CONTINUE
C CANNOT BE TSTART.
  17     CONTINUE
  18  DTJ = 0
C START WITH NO ERROR STATES.
      FNUM = 0
      IORDER = ISTKGT(2*NU**2, 2)
      IBC = ISTKGT(8*NU, 2)
      IEQS = IBC+4*NU
      IF (NU .LE. 0) GOTO 19
         CALL SETI(4*NU, 1, IS(IEQS))
         CALL SETI(4*NU, -2, IS(IBC))
  19  IAFB(1) = ISTKGT(6*NU*(2*NU+NV+1), 4)
      IAFB(2) = IAFB(1)+2*NU*(2*NU+NV+1)
      IAFB(3) = IAFB(2)+2*NU*(2*NU+NV+1)
      IDMAT = ISTKGT(NV**2, 4)
      IALFA(1) = ISTKGT(6*NU*(2*NU+NV+1), 4)
      IBETA(1) = IALFA(1)+2*NU**2
      IGAMMA(1) = IBETA(1)+2*NU**2
      IALFA(2) = IGAMMA(1)+2*NU*(NV+1)
      IBETA(2) = IALFA(2)+2*NU**2
      IGAMMA(2) = IBETA(2)+2*NU**2
      IALFA(3) = IGAMMA(2)+2*NU*(NV+1)
      IBETA(3) = IALFA(3)+2*NU**2
      IGAMMA(3) = IBETA(3)+2*NU**2
      IL = 1
      IPPVOT = 1
      IDIAG = ISTKGT(NV, 4)
      IDPVOT = ISTKGT(NV, 2)
      IAA = ISTKGT(2*NU*(3*NU+2*(NV+1)), 4)
      IBB = IAA+2*NU**2
      ICC = IBB+2*NU**2
      ISGMAD = ICC+2*NU**2
      ISGMAM = ISGMAD+2*NU*(NV+1)
      ID4(1) = ISTKGT(3*NV**2, 4)
      ID4(2) = ID4(1)+NV**2
      ID4(3) = ID4(2)+NV**2
      IF (.NOT. SEPATE) GOTO 20
         IJP(1) = ISTKGT(3*(2*K*NU-1)*(NX-K)*NU, 4)
         IJP(2) = IJP(1)+(2*K*NU-1)*(NX-K)*NU
         IJP(3) = IJP(2)+(2*K*NU-1)*(NX-K)*NU
         IB(1) = ISTKGT(3*NU*(NX-K)*(NV+1), 4)
         IB(2) = IB(1)+NU*(NX-K)*(NV+1)
         IB(3) = IB(2)+NU*(NX-K)*(NV+1)
         ID5(1) = ISTKGT(3*NU*(NX-K)*NV, 4)
         ID5(2) = ID5(1)+NU*(NX-K)*NV
         ID5(3) = ID5(2)+NU*(NX-K)*NV
         IL = ISTKGT((K*NU-1)*(NX-K)*NU, 4)
         IPPVOT = ISTKGT((NX-K)*NU, 2)
         GOTO  22
  20     CALL SETI(3, 1, IJP)
         CALL SETI(3, 1, IB)
         IF (KEEACC .NE. 1) GOTO 21
            IJP(1) = ISTKGT(NU*(NX-K)*(2*K*NU-1), 4)
            IJP(3) = IJP(1)
            IL = ISTKGT(NU*(NX-K)*(K*NU-1), 4)
            IB(1) = ISTKGT(NU*(NX-K)*(NV+1), 4)
            IB(3) = IB(1)
            IPPVOT = ISTKGT(NU*(NX-K), 2)
  21     IAFB(3) = IAFB(1)
         IALFA(3) = IALFA(1)
         IBETA(3) = IBETA(1)
         IGAMMA(3) = IGAMMA(1)
         ID4(3) = ID4(1)
         ID5(1) = ISTKGT(2*NU*(NX-K)*NV, 4)
         ID5(2) = ID5(1)+NU*(NX-K)*NV
         ID5(3) = ID5(1)
C FLAG SCALING WORK-SPACE AS UN-ALLOCATED.
  22  CALL SETI(4, 0, IMEM)
C GET SPACE FOR PDE SCALING.
      TEMP = IJP(3)
      CALL SCALE(1, 1, WS(TEMP), NU*(NX-K), 2*K*NU-1)
C GET SPACE FOR DIRICHLET BC SCALING.
      CALL SCALE(2, 1, WS(IAA), NU, NU)
C GET SPACE FOR NEUMAN BC SCALING.
      CALL SCALE(3, 1, WS(IAA), NU, NU)
C GET SPACE FOR ODE SCALING.
      TEMP = ID4(3)
      CALL SCALE(4, 1, WS(TEMP), NV, NV)
      DO  23 I = 1, 4
C/6S
C        IF (IMEM(I) .LE. 0) CALL SETERR(
C    1      51HDPOST4 - SCALE FAILED TO INITIALIZE COMMON /D9OSTS/, 51
C    2      , 18, 2)
C/7S
         IF (IMEM(I) .LE. 0) CALL SETERR(
     1      'DPOST4 - SCALE FAILED TO INITIALIZE COMMON /D9OSTS/', 51
     2      , 18, 2)
C/
  23     CONTINUE
      EGIVEC = EGIVE
      TGOOD = TSTART
      IF (NU .LE. 0) GOTO 24
         IXGQ = ISTKGT(2*MGQ, 4)
         IWGQ = IXGQ+MGQ
         MGQQ = MGQ
         CALL DGQ1(MGQ, WS(IXGQ), WS(IWGQ))
         GOTO  25
  24     IXGQ = 1
         IWGQ = 1
         MGQQ = 0
  25  NUU = NU
      NVV = NV
      IF (NU .NE. 0) GOTO 26
         KK = 2
         NXX = 2*KK
         GOTO  27
  26     KK = K
         NXX = NX
C TELL STATS ROUTINE IN POST.
  27  CALL D9OSTX(STATS, 1)
      CALL D90STS(U, NU, KK, X, NXX, V, NV, TSTART, TSTOP, DT, D9OSTA,
     1   AF, D9OSTB, BC, D9OSTD, D, D9OSTE, ERROR, ERRPAR, INMI, SCALE
     2   , D9OSTH, HANDLE, BETA, GAMMA, DELTA, N, KMAX, MMAX, XPOLY,
     3   KINIT, HFRACT, D9OSTP, D9OSTN)
C TELL STATS ROUTINE OUT OF POST.
      CALL D9OSTX(STATS, -1)
      TSTOP = TGOOD
C CAPTURE THE ERROR NUMBER, IF ANY.
      NERR = NERROR(NERR)
      IF (NERR .NE. 15) GOTO 28
         CALL ERROFF
C/6S
C        CALL SETERR(13HDPOST4 - DT=0, 13, 1000, 1)
C/7S
         CALL SETERR('DPOST4 - DT=0', 13, 1000, 1)
C/
  28  IF (NERR .NE. 16) GOTO 29
         CALL ERROFF
C/6S
C        CALL SETERR(32HDPOST4 - DT=0 RETURNED BY HANDLE, 32, 1001, 1)
C/7S
         CALL SETERR('DPOST4 - DT=0 RETURNED BY HANDLE', 32, 1001, 1)
C/
  29  IF (NERR .NE. 17) GOTO 30
         CALL ERROFF
C/6S
C        CALL SETERR(45HDPOST4 - DT RETURNED BY HANDLE HAS WRONG SIGN,
C    1      45, 1002, 1)
C/7S
         CALL SETERR('DPOST4 - DT RETURNED BY HANDLE HAS WRONG SIGN',
     1      45, 1002, 1)
C/
  30  IF (NERR .NE. 18) GOTO 31
         CALL ERROFF
C/6S
C        CALL SETERR(46HDPOST4 - CANNOT RAISE DT IN HANDLE WHEN FAILED
C    1      , 46, 1003, 1)
C/7S
         CALL SETERR('DPOST4 - CANNOT RAISE DT IN HANDLE WHEN FAILED'
     1      , 46, 1003, 1)
C/
  31  IF (NERR .NE. 19) GOTO 32
         CALL ERROFF
C/6S
C        CALL SETERR(36HDPOST4 - E(I).LE.0 RETURNED BY ERROR, 36, 1004
C    1      , 1)
C/7S
         CALL SETERR('DPOST4 - E(I).LE.0 RETURNED BY ERROR', 36, 1004
     1      , 1)
C/
  32  IF (NERR .NE. 15) GOTO 42
         IF (FNUM .NE. 1) GOTO 33
            CALL ERROFF
C/6S
C           CALL SETERR(19HDPOST4 - AF FAILURE, 19, 1013, 1)
C/7S
            CALL SETERR('DPOST4 - AF FAILURE', 19, 1013, 1)
C/
  33     IF (FNUM .NE. 2) GOTO 34
            CALL ERROFF
C/6S
C           CALL SETERR(19HDPOST4 - BC FAILURE, 19, 1014, 1)
C/7S
            CALL SETERR('DPOST4 - BC FAILURE', 19, 1014, 1)
C/
  34     IF (FNUM .NE. 3) GOTO 35
            CALL ERROFF
C/6S
C           CALL SETERR(18HDPOST4 - D FAILURE, 18, 1015, 1)
C/7S
            CALL SETERR('DPOST4 - D FAILURE', 18, 1015, 1)
C/
  35     IF (FNUM .NE. 4) GOTO 36
            CALL ERROFF
C/6S
C           CALL SETERR(
C    1         47HDPOST4 - SINGULAR DIRICHLET BOUNDARY CONDITIONS, 47,
C    2         1016, 1)
C/7S
            CALL SETERR(
     1         'DPOST4 - SINGULAR DIRICHLET BOUNDARY CONDITIONS', 47,
     2         1016, 1)
C/
  36     IF (FNUM .NE. 5) GOTO 37
            CALL ERROFF
C/6S
C           CALL SETERR(43HDPOST4 - SINGULAR MIXED BOUNDARY CONDITIONS
C    1         , 43, 1017, 1)
C/7S
            CALL SETERR('DPOST4 - SINGULAR MIXED BOUNDARY CONDITIONS'
     1         , 43, 1017, 1)
C/
  37     IF (FNUM .NE. 6) GOTO 38
            CALL ERROFF
C/6S
C           CALL SETERR(30HDPOST4 - SINGULAR PDE JACOBIAN, 30, 1018, 1)
C/7S
            CALL SETERR('DPOST4 - SINGULAR PDE JACOBIAN', 30, 1018, 1)
C/
  38     IF (FNUM .NE. 7) GOTO 39
            CALL ERROFF
C/6S
C           CALL SETERR(30HDPOST4 - SINGULAR ODE JACOBIAN, 30, 1019, 1)
C/7S
            CALL SETERR('DPOST4 - SINGULAR ODE JACOBIAN', 30, 1019, 1)
C/
  39     IF (FNUM .NE. 8) GOTO 40
            CALL ERROFF
C/6S
C           CALL SETERR(
C    1         45HDPOST4 - TOO MANY NEWTON ITERATIONS PREDICTED, 45,
C    2         1020, 1)
C/7S
            CALL SETERR(
     1         'DPOST4 - TOO MANY NEWTON ITERATIONS PREDICTED', 45,
     2         1020, 1)
C/
  40     IF (FNUM .NE. 9) GOTO 41
            CALL ERROFF
C/6S
C           CALL SETERR(42HDPOST4 - TOO MANY NEWTON ITERATIONS NEEDED,
C    1         42, 1021, 1)
C/7S
            CALL SETERR('DPOST4 - TOO MANY NEWTON ITERATIONS NEEDED',
     1         42, 1021, 1)
C/
  41     CONTINUE
  42  CALL LEAVE
      RETURN
      END
