      LOGICAL FUNCTION A90STA(U, UT, NU, V, VT, NV, T, DT, K, X,
     1   NX, AF, AF1, BC, BC1, D, D1234, SCALE, DU, ALFA, BETA, GAMMA0
     2   , GAMMA1, GAMMA, AA, BB, CC, SGAMAD, SGAMAM, BCS, EQS, ORDER, B
     3   , EXCHG, BT, NXMK, NRHS)
      INTEGER NRHS, NXMK, NU, NX
      EXTERNAL AF, AF1, BC, BC1, D, D1234
      EXTERNAL SCALE
      INTEGER NV, K, BCS(NU, 2, 2), EQS(NU, 2, 2), ORDER(NU, NU, 2)
      REAL U(NXMK, NU), UT(NXMK, NU), V(1), VT(1), T, DT
      REAL X(NX), DU(NXMK, NU), ALFA(NU, NU, 2), BETA(NU, NU, 2),
     1   GAMMA0(NU, NRHS, 2), GAMMA1(NU, NRHS, 2)
      REAL GAMMA(NU, NRHS, 2), AA(NU, NU, 2), BB(NU, NU, 2), CC(NU, NU
     1   , 2), SGAMAD(NU, NRHS, 2), SGAMAM(NU, NRHS, 2)
      REAL B(NU, NXMK, NRHS), EXCHG(NU, NXMK), BT(NXMK, NU, NRHS)
      LOGICAL BC
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      COMMON /A9OSTT/ TIME, DELTAT
      REAL TIME, DELTAT
      COMMON /A9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC
      INTEGER MINIT, MAXIT, KEEJAC
      REAL THETA, EGIVE
      COMMON /A9OSTJ/ IJP, IB, IAFB, IALFA, IBETA, IGAMMA, ID4, ID5,
     1   IORDER, IBC, IEQS, IAA, IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT,
     2   IDMAT, IDIAG, IDPVOT
      INTEGER IJP(3), IB(3), IAFB(3), IALFA(3), IBETA(3), IGAMMA(3)
      INTEGER ID4(3), ID5(3), IORDER, IBC, IEQS, IAA
      INTEGER IBB, ICC, ISGMAD, ISGMAM, IL, IPPVOT
      INTEGER IDMAT, IDIAG, IDPVOT
      COMMON /A9OSTG/ TJ, DTJ, GETJAC, SEPATE
      REAL TJ, DTJ
      LOGICAL GETJAC, SEPATE
      COMMON /A9OSTF/ FNUM
      INTEGER FNUM
      COMMON /A90STQ/ IXGQ, IWGQ, MGQ
      INTEGER IXGQ, IWGQ, MGQ
      COMMON /A90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS
      INTEGER NNFS, NRS
      COMMON /A90STB/ IGSSIS, SET
      INTEGER IGSSIS
      LOGICAL SET
      INTEGER JACLEN, IEU, SORDER, VORDER, ISTKGT, NERROR
      INTEGER MAX0, I, J, L, NBCS, IEUX
      INTEGER IEUT, NERR, IS(1000), IEUTX, NRHSD, NRHSG
      INTEGER MAXDER
      REAL WS(500), RS(1000)
      LOGICAL NEESUM, GETACT, LS(1000), GLSIN, GLSBI, GLSBC
      INTEGER TEMP, TEMP2, TEMP3, TEMP4, TEMP5, TEMP6
      LOGICAL TEMP1
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C (V,VT)(NV).
      JACLEN = NU*(NX-K)*(2*K*NU-1)
      NEESUM = DTJ .NE. DT
      NEESUM = NEESUM .AND. SEPATE
      TEMP1 = GETJAC
      IF (TEMP1) TEMP1 = .NOT. SEPATE
      IF (.NOT. TEMP1) GOTO 1
         NRHSG = NRHS
         GOTO  2
   1     NRHSG = 1
   2  TEMP1 = GETJAC
      IF (.NOT. TEMP1) TEMP1 = NEESUM
      IF (.NOT. TEMP1) GOTO 3
         NRHSD = NRHS
         GOTO  4
   3     NRHSD = 1
   4  TEMP1 = SEPATE
      IF (TEMP1) TEMP1 = GETJAC
      IF (.NOT. TEMP1) GOTO 7
         TIME = TJ
         TEMP4 = IJP(1)
         TEMP = IJP(2)
         TEMP3 = IB(1)
         TEMP5 = IB(2)
         IF (.NOT. GLSIN(K, X, NX, AF, AF1, NU, NRHS, NRHS, MGQ, WS(
     1      IXGQ), WS(IWGQ), GETJAC, SEPATE, WS(TEMP4), WS(TEMP), WS(
     2      TEMP3), WS(TEMP5), ORDER, IGSSIS, SET)) GOTO 5
            TIME = T
            A90STA = .TRUE.
            RETURN
   5     TIME = T
         IF (NERROR(NERR) .NE. 0) CALL ERROFF
C/6S
C        IF (NERR .EQ. 14) CALL SETERR(
C    1      33H POST4 - MGQ=K-1 AND ORDER(I,.)=0, 33, 1011, 1)
C        IF (NERR .EQ. 15) CALL SETERR(26H POST4 - PDE(I) IS VACUOUS,
C    1      26, 1012, 1)
C/7S
         IF (NERR .EQ. 14) CALL SETERR(
     1      ' POST4 - MGQ=K-1 AND ORDER(I,.)=0', 33, 1011, 1)
         IF (NERR .EQ. 15) CALL SETERR(' POST4 - PDE(I) IS VACUOUS',
     1      26, 1012, 1)
C/
         IF (NERR .EQ. 0) GOTO 6
            A90STA = .TRUE.
            RETURN
   6     CONTINUE
   7  IF (.NOT. NEESUM) GOTO 10
         DO  8 I = 1, JACLEN
            TEMP5 = IJP(3)+I
            TEMP3 = IJP(1)+I
            TEMP = IJP(2)+I
            WS(TEMP5-1) = WS(TEMP3-1)+WS(TEMP-1)/DT
   8        CONTINUE
         TEMP = NU*NXMK*NRHS
         DO  9 I = 1, TEMP
            TEMP3 = IB(1)+I
            TEMP5 = IB(2)+I
            B(I, 1, 1) = WS(TEMP3-1)+WS(TEMP5-1)/DT
   9        CONTINUE
  10  TEMP1 = GETJAC
      IF (TEMP1) TEMP1 = .NOT. SEPATE
      GETACT = TEMP1
      TEMP = IJP(3)
      TEMP5 = IJP(2)
      TEMP3 = IB(3)
      TEMP4 = IB(2)
      IF (.NOT. GLSIN(K, X, NX, AF, AF1, NU, NRHS, NRHSG, MGQ, WS(IXGQ),
     1   WS(IWGQ), GETACT, .FALSE., WS(TEMP), WS(TEMP5), WS(TEMP3), WS(
     2   TEMP4), ORDER, IGSSIS, SET)) GOTO 11
         A90STA = .TRUE.
         RETURN
  11  IF (NERROR(NERR) .NE. 0) CALL ERROFF
C/6S
C     IF (NERR .EQ. 14) CALL SETERR(
C    1   33H POST4 - MGQ=K-1 AND ORDER(I,.)=0, 33, 1011, 1)
C     IF (NERR .EQ. 15) CALL SETERR(26H POST4 - PDE(I) IS VACUOUS, 26,
C    1   1012, 1)
C/7S
      IF (NERR .EQ. 14) CALL SETERR(
     1   ' POST4 - MGQ=K-1 AND ORDER(I,.)=0', 33, 1011, 1)
      IF (NERR .EQ. 15) CALL SETERR(' POST4 - PDE(I) IS VACUOUS', 26,
     1   1012, 1)
C/
      IF (NERR .EQ. 0) GOTO 12
         A90STA = .TRUE.
         RETURN
  12  IEU = ISTKGT(8*NU, 3)
      IEUX = IEU+2*NU
      IEUT = IEUX+2*NU
      IEUTX = IEUT+2*NU
      TEMP1 = SEPATE
      IF (TEMP1) TEMP1 = GETJAC
      IF (.NOT. TEMP1) GOTO 14
         TIME = TJ
         TEMP4 = IALFA(1)
         TEMP3 = IBETA(1)
         TEMP5 = IALFA(2)
         TEMP = IBETA(2)
         IF (.NOT. BC(U, UT, NU, V, VT, NV, TJ, 0E0, K, X, NX, GETJAC,
     1      SEPATE, BC1, WS(TEMP4), WS(TEMP3), GAMMA0, WS(TEMP5), WS(
     2      TEMP), GAMMA1, NX-K, NRHS, NRHS, WS(IEU), WS(IEUX), WS(IEUT)
     3      , WS(IEUTX))) GOTO 13
            TIME = T
            A90STA = .TRUE.
            RETURN
  13     TIME = T
  14  IF (.NOT. NEESUM) GOTO 19
         TEMP = 2*NU**2
         DO  15 I = 1, TEMP
            TEMP5 = IALFA(1)+I
            TEMP3 = IALFA(2)+I
            ALFA(I, 1, 1) = WS(TEMP5-1)+WS(TEMP3-1)/DT
            TEMP3 = IBETA(1)+I
            TEMP5 = IBETA(2)+I
            BETA(I, 1, 1) = WS(TEMP3-1)+WS(TEMP5-1)/DT
  15        CONTINUE
         DO  18 L = 1, 2
            DO  17 I = 1, NU
               DO  16 J = 1, NRHS
                  GAMMA(I, J, L) = GAMMA0(I, J, L)+GAMMA1(I, J, L)/DT
  16              CONTINUE
  17           CONTINUE
  18        CONTINUE
  19  TEMP1 = GETJAC
      IF (TEMP1) TEMP1 = .NOT. SEPATE
      GETACT = TEMP1
      TEMP = IALFA(3)
      TEMP5 = IBETA(3)
      TEMP3 = IALFA(2)
      TEMP4 = IBETA(2)
      IF (.NOT. BC(U, UT, NU, V, VT, NV, T, DT, K, X, NX, GETACT,
     1   .FALSE., BC1, WS(TEMP), WS(TEMP5), GAMMA, WS(TEMP3), WS(TEMP4),
     2   GAMMA1, NX-K, NRHS, NRHSG, WS(IEU), WS(IEUX), WS(IEUT), WS(
     3   IEUTX))) GOTO 20
         A90STA = .TRUE.
         RETURN
  20  CALL ISTKRL(1)
      DO  22 L = 1, 2
         CALL GLSSB(ALFA(1, 1, L), BETA(1, 1, L), GAMMA(1, 1, L), NU,
     1      NRHSD, AA(1, 1, L), BB(1, 1, L), CC(1, 1, L), SGAMAD(1, 1, L
     2      ), SGAMAM(1, 1, L), BCS(1, 1, L))
         IF (NERROR(NERR) .NE. 0) CALL ERROFF
C/6S
C        IF (NERR .EQ. 3) CALL SETERR(
C    1      53H POST4 - MIXED BOUNDARY CONDITIONS ARE OVERDETERMINED,
C    2      53, 1006, 1)
C/7S
         IF (NERR .EQ. 3) CALL SETERR(
     1      ' POST4 - MIXED BOUNDARY CONDITIONS ARE OVERDETERMINED',
     2      53, 1006, 1)
C/
         IF (NERR .EQ. 5) FNUM = 5
C/6S
C        IF (NERR .EQ. 4) CALL SETERR(
C    1      57H POST4 - DIRICHLET BOUNDARY CONDITIONS ARE OVERDETERMINED
C    2      , 57, 1005, 1)
C/7S
         IF (NERR .EQ. 4) CALL SETERR(
     1      ' POST4 - DIRICHLET BOUNDARY CONDITIONS ARE OVERDETERMINED'
     2      , 57, 1005, 1)
C/
         IF (NERR .EQ. 6) FNUM = 4
         IF (NERR .EQ. 0) GOTO 21
            A90STA = .TRUE.
            RETURN
  21     CONTINUE
  22     CONTINUE
      TEMP1 = SEPATE
      IF (TEMP1) TEMP1 = GETJAC
      IF (.NOT. TEMP1) GOTO 24
         TIME = TJ
         TEMP4 = IAFB(1)
         TEMP3 = IAFB(1)+2*NU**2
         TEMP5 = IAFB(1)+4*NU**2
         TEMP = IAFB(2)
         TEMP2 = IAFB(2)+2*NU**2
         TEMP6 = IAFB(2)+4*NU**2
         IF (.NOT. GLSBI(K, X, NX, AF, AF1, NU, NRHS, NRHS, GETJAC,
     1      SEPATE, WS(TEMP4), WS(TEMP3), WS(TEMP5), WS(TEMP), WS(TEMP2)
     2      , WS(TEMP6))) GOTO 23
            TIME = T
            A90STA = .TRUE.
            RETURN
  23     TIME = T
  24  IF (.NOT. NEESUM) GOTO 26
         TEMP6 = 2*NU*(2*NU+NRHS)
         DO  25 I = 1, TEMP6
            TEMP2 = IAFB(3)+I
            TEMP = IAFB(1)+I
            TEMP5 = IAFB(2)+I
            WS(TEMP2-1) = WS(TEMP-1)+WS(TEMP5-1)/DT
  25        CONTINUE
  26  TEMP1 = GETJAC
      IF (TEMP1) TEMP1 = .NOT. SEPATE
      GETACT = TEMP1
      TEMP6 = IAFB(3)
      TEMP5 = IAFB(3)+2*NU**2
      TEMP = IAFB(3)+4*NU**2
      TEMP2 = IAFB(2)
      TEMP3 = IAFB(2)+2*NU**2
      TEMP4 = IAFB(2)+4*NU**2
      IF (.NOT. GLSBI(K, X, NX, AF, AF1, NU, NRHS, NRHSG, GETACT,
     1   .FALSE., WS(TEMP6), WS(TEMP5), WS(TEMP), WS(TEMP2), WS(TEMP3)
     2   , WS(TEMP4))) GOTO 27
         A90STA = .TRUE.
         RETURN
  27  TEMP1 = GETJAC
      IF (.NOT. TEMP1) TEMP1 = NEESUM
      GETACT = TEMP1
      TEMP4 = IAFB(3)
      TEMP3 = IAFB(3)+2*NU**2
      TEMP2 = IAFB(3)+4*NU**2
      TEMP = IJP(3)
      CALL GLSBT(K, X, NX, NU, NRHS, NRHSD, GETACT, WS(TEMP4), WS(TEMP3)
     1   , WS(TEMP2), AA, BB, SGAMAM, BCS, WS(TEMP), B)
      TEMP1 = NEESUM
      IF (.NOT. TEMP1) TEMP1 = GETJAC
      IF (.NOT. TEMP1) GOTO 39
         IF (GLSBC(NU, ORDER, BCS, EQS)) GOTO 30
            CALL GLSBP(NU, ORDER, BCS, EQS)
            IF (NERROR(NERR) .EQ. 0) GOTO 28
               CALL ERROFF
C/6S
C              CALL SETERR(21H POST4 - IMPROPER BCS, 21, 1007, 1)
C/7S
               CALL SETERR(' POST4 - IMPROPER BCS', 21, 1007, 1)
C/
               GOTO  29
C/6S
C 28           IF (.NOT. GLSBC(NU, ORDER, BCS, EQS)) CALL SETERR(
C    1            26HA90STA -  GLSBP-BC FAILURE, 26, 1, 2)
C/7S
  28           IF (.NOT. GLSBC(NU, ORDER, BCS, EQS)) CALL SETERR(
     1            'A90STA -  GLSBP-BC FAILURE', 26, 1, 2)
C/
  29        CONTINUE
C COUNT THE BOUNDARY CONDITIONS.
  30     NBCS = 0
         DO  32 I = 1, NU
            DO  31 J = 1, 2
               IF (BCS(I, 1, J) .GE. 0) NBCS = NBCS+1
               IF (BCS(I, 2, J) .GE. 0) NBCS = NBCS+1
  31           CONTINUE
  32        CONTINUE
         SORDER = 0
C COUNT THE TOTAL ORDER OF THE SYSTEM.
         VORDER = 0
         DO  35 J = 1, NU
            MAXDER = 0
            DO  33 I = 1, NU
               MAXDER = MAX0(MAXDER, ORDER(I, J, 1), ORDER(I, J, 2))
  33           CONTINUE
            SORDER = SORDER+MAXDER
            MAXDER = 0
            DO  34 I = 1, NU
               MAXDER = MAX0(MAXDER, ORDER(J, I, 1), ORDER(J, I, 2))
  34           CONTINUE
            VORDER = VORDER+MAXDER
  35        CONTINUE
         IF (SORDER .EQ. VORDER) GOTO 36
            CALL ERROFF
C/6S
C           CALL SETERR(
C    1         45H POST4 - PDE SYSTEM NOT IN MINIMAL ORDER FORM, 45,
C    2         1008, 1)
C/7S
            CALL SETERR(
     1         ' POST4 - PDE SYSTEM NOT IN MINIMAL ORDER FORM', 45,
     2         1008, 1)
C/
            A90STA = .TRUE.
            RETURN
  36     IF (NBCS .GE. SORDER) GOTO 37
            CALL ERROFF
C/6S
C           CALL SETERR(36H POST4 - TOO FEW BOUNDARY CONDITIONS, 36,
C    1         1009, 1)
C/7S
            CALL SETERR(' POST4 - TOO FEW BOUNDARY CONDITIONS', 36,
     1         1009, 1)
C/
  37     IF (NBCS .LE. SORDER) GOTO 38
            CALL ERROFF
C/6S
C           CALL SETERR(37H POST4 - TOO MANY BOUNDARY CONDITIONS, 37,
C    1         1010, 1)
C/7S
            CALL SETERR(' POST4 - TOO MANY BOUNDARY CONDITIONS', 37,
     1         1010, 1)
C/
  38     CONTINUE
  39  IF (NERROR(NERR) .EQ. 0) GOTO 40
         A90STA = .TRUE.
         RETURN
  40  TEMP1 = GETJAC
      IF (.NOT. TEMP1) TEMP1 = NEESUM
      GETACT = TEMP1
      TEMP = IJP(3)
      CALL GLSBD(K, NX, NU, NRHS, NRHSD, BCS, EQS, CC, SGAMAD, GETACT,
     1   WS(TEMP), B)
      IF (KEEJAC .NE. 0) GOTO 41
         IL = ISTKGT((K*NU-1)*(NX-K)*NU, 3)
         IPPVOT = ISTKGT(NU*(NX-K), 2)
  41  TEMP1 = NEESUM
      IF (.NOT. TEMP1) TEMP1 = GETJAC
      IF (.NOT. TEMP1) GOTO 43
         TEMP = IJP(3)
C SCALE PDE JACOBIAN.
         CALL SCALE(1, 1, WS(TEMP), (NX-K)*NU, 2*K*NU-1)
         TEMP = IJP(3)
         CALL BNDLU((NX-K)*NU, K*NU, 2*K*NU-1, WS(TEMP), WS(IL), IS(
     1      IPPVOT))
         IF (NERROR(NERR) .EQ. 0) GOTO 42
            CALL ERROFF
            FNUM = 6
            A90STA = .TRUE.
            RETURN
  42     CONTINUE
C SCALE THE PDE RHS.
  43  CALL SCALE(1, 2, B, (NX-K)*NU, NRHSD)
      TEMP = IJP(3)
      CALL BNDFB((NX-K)*NU, K*NU, 2*K*NU-1, WS(IL), WS(TEMP), IS(IPPVOT)
     1   , NRHSD, B)
C/6S
C     IF (NERROR(NERR) .NE. 0) CALL SETERR(
C    1   31HA90STC - SINGULAR JAC IN  BNDFB, 31, 1, 2)
C/7S
      IF (NERROR(NERR) .NE. 0) CALL SETERR(
     1   'A90STC - SINGULAR JAC IN  BNDFB', 31, 1, 2)
C/
C UN-SCALE THE PDE SOLUTION.
      CALL SCALE(1, 3, B, (NX-K)*NU, NRHSD)
      IF (KEEJAC .EQ. 0) CALL ISTKRL(2)
C FORM BT = B-TRANSPOSE.
      DO  46 L = 1, NRHSD
         CALL MOVEFR((NX-K)*NU, B(1, 1, L), EXCHG)
         TEMP = NX-K
         DO  45 I = 1, TEMP
            DO  44 J = 1, NU
               BT(I, J, L) = EXCHG(J, I)
  44           CONTINUE
  45        CONTINUE
  46     CONTINUE
      DO  48 J = 1, NU
         TEMP = NX-K
         DO  47 I = 1, TEMP
            DU(I, J) = BT(I, J, 1)
  47        CONTINUE
  48     CONTINUE
      A90STA = .FALSE.
      RETURN
      END
