      LOGICAL FUNCTION DGLSIN(K, X, NX, AF, AF1, NU, NRHS, NRHSG
     1   , MQ, XQ, WQ, GETJAC, SEPATE, G, GT, B, BT, ORDER, IGSSIS, SET)
      INTEGER MQ, NU, NX
      EXTERNAL AF, AF1
      INTEGER K, NRHS, NRHSG, ORDER(NU, NU, 2), IGSSIS
      LOGICAL GETJAC, SEPATE, SET
      DOUBLE PRECISION X(NX), XQ(MQ), WQ(MQ), G(1, 1), GT(1, 1), B(1, 1)
      DOUBLE PRECISION BT(1, 1)
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      INTEGER ITQ, ISTKGT, IA1T, IA2T, IA3T, IA4T
      INTEGER IF1T, IF2T, I, ISBSIS, IQ, IS(1000)
      INTEGER IA1, IA2, IA3, IA4, IF1, IF2
      INTEGER IQ0, IZERO
      REAL RS(1000)
      LOGICAL FAILED, LS(1000), D6LSIN
      DOUBLE PRECISION WS(500)
      INTEGER TEMP1
      LOGICAL TEMP
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C MNEMONIC - DOUBLE PRECISION GALERKIN'S METHOD FOR LINEAR SYSTEMS,
C            INTEGRALS.
C SCRATCH SPACE ALLOCATED -
C       S(DGLSIN) = MQ*( 2*K+1 + 8*NU**2 + 2*NU*NRHS ) +
C                   MAX( IF ( SET ) { 0 } ELSE { 3*K }, S(AF) )
C   LONG REAL WORDS +
C                   NU
C   LOGICAL WORDS.
C LONG REAL G((NX-K)*NU,2*K*NU-1),B((NX-K)*NU,NRHS),BT((NX-K)*NU,NRHS)
C CHECK THE DATA FOR ERRORS.
C/6S
C     IF (K .LT. 2) CALL SETERR(15HDGLSIN - K.LT.2, 15, 1, 2)
C     IF (NX .LT. 2*K) CALL SETERR(18HDGLSIN - NX.LT.2*K, 18, 2, 2)
C     IF (NU .LT. 1) CALL SETERR(16HDGLSIN - NU.LT.1, 16, 3, 2)
C     IF (NRHS .LT. 1) CALL SETERR(18HDGLSIN - NRHS.LT.1, 18, 4, 2)
C     IF (NRHSG .LT. 0) CALL SETERR(19HDGLSIN - NRHSG.LT.0, 19, 5, 2)
C     IF (MQ .LT. K-1) CALL SETERR(18HDGLSIN - MQ.LT.K-1, 18, 6, 2)
C     IF (XQ(1) .LT. (-1D0)) CALL SETERR(20HDGLSIN - XQ(1).LT.-1, 20, 7,
C    1   2)
C/7S
      IF (K .LT. 2) CALL SETERR('DGLSIN - K.LT.2', 15, 1, 2)
      IF (NX .LT. 2*K) CALL SETERR('DGLSIN - NX.LT.2*K', 18, 2, 2)
      IF (NU .LT. 1) CALL SETERR('DGLSIN - NU.LT.1', 16, 3, 2)
      IF (NRHS .LT. 1) CALL SETERR('DGLSIN - NRHS.LT.1', 18, 4, 2)
      IF (NRHSG .LT. 0) CALL SETERR('DGLSIN - NRHSG.LT.0', 19, 5, 2)
      IF (MQ .LT. K-1) CALL SETERR('DGLSIN - MQ.LT.K-1', 18, 6, 2)
      IF (XQ(1) .LT. (-1D0)) CALL SETERR('DGLSIN - XQ(1).LT.-1', 20, 7,
     1   2)
C/
      IF (WQ(1) .EQ. 0D0) GOTO 1
         IQ0 = 1
         GOTO  2
   1     IQ0 = MQ+1
   2  IQ = 2
         GOTO  4
   3     IQ = IQ+1
   4     IF (IQ .GT. MQ) GOTO  5
C CHECK XQ FOR MONOTONICITY
C AND WQ BEING 0.
C/6S
C        IF (XQ(IQ-1) .GE. XQ(IQ)) CALL SETERR(
C    1      44HDGLSIN - XQ NOT STRICTLY MONOTONE INCREASING, 44, 8, 2)
C/7S
         IF (XQ(IQ-1) .GE. XQ(IQ)) CALL SETERR(
     1      'DGLSIN - XQ NOT STRICTLY MONOTONE INCREASING', 44, 8, 2)
C/
         IF (WQ(IQ) .NE. 0D0) IQ0 = IQ
         GOTO  3
C/6S
C  5  IF (IQ0 .GT. MQ) CALL SETERR(31HDGLSIN - WQ IS IDENTICALLY ZERO,
C    1   31, 9, 2)
C     IF (XQ(MQ) .GT. 1D0) CALL SETERR(20HDGLSIN - XQ(MQ).GT.1, 20, 10
C    1   , 2)
C/7S
   5  IF (IQ0 .GT. MQ) CALL SETERR('DGLSIN - WQ IS IDENTICALLY ZERO',
     1   31, 9, 2)
      IF (XQ(MQ) .GT. 1D0) CALL SETERR('DGLSIN - XQ(MQ).GT.1', 20, 10
     1   , 2)
C/
      DO  7 I = 1, K
         TEMP = X(I) .NE. X(1)
         IF (TEMP) GOTO 6
            TEMP1 = NX-K+I
            TEMP = X(TEMP1) .NE. X(NX)
C/6S
C  6     IF (TEMP) CALL SETERR(
C    1      46HDGLSIN - END POINTS OF X NOT OF MULTIPLICITY K, 46, 11, 2
C    2      )
C/7S
   6     IF (TEMP) CALL SETERR(
     1      'DGLSIN - END POINTS OF X NOT OF MULTIPLICITY K', 46, 11, 2
     2      )
C/
   7     CONTINUE
C/6S
C     IF (X(1) .GE. X(NX)) CALL SETERR(
C    1   34HDGLSIN - X NOT MONOTONE INCREASING, 34, 12, 2)
C/7S
      IF (X(1) .GE. X(NX)) CALL SETERR(
     1   'DGLSIN - X NOT MONOTONE INCREASING', 34, 12, 2)
C/
      TEMP = SET
      IF (TEMP) TEMP = IGSSIS .LE. 1
C/6S
C     IF (TEMP) CALL SETERR(34HDGLSIN - SET=.T. AND IGSBASIS.LE.1, 34,
C    1   13, 2)
C/7S
      IF (TEMP) CALL SETERR('DGLSIN - SET=.T. AND IGSBASIS.LE.1', 34,
     1   13, 2)
C/
      CALL ENTER(1)
      ITQ = ISTKGT(MQ, 4)
      ISBSIS = ISTKGT(2*K*MQ, 4)
      IA1 = ISTKGT(4*MQ*NU*(2*NU+NRHS), 4)
      IA1T = IA1+MQ*NU**2
      IA2 = IA1T+MQ*NU**2
      IA2T = IA2+MQ*NU**2
      IA3 = IA2T+MQ*NU**2
      IA3T = IA3+MQ*NU**2
      IA4 = IA3T+MQ*NU**2
      IA4T = IA4+MQ*NU**2
      IF1 = IA4T+MQ*NU**2
      IF1T = IF1+MQ*NU*NRHS
      IF2 = IF1T+MQ*NU*NRHS
      IF2T = IF2+MQ*NU*NRHS
      IZERO = ISTKGT(NU, 1)
      FAILED = D6LSIN(K, X, NX, AF, AF1, NU, NRHS, NRHSG, MQ, XQ, WQ,
     1   GETJAC, SEPATE, G, GT, B, BT, ORDER, IGSSIS, SET, WS(ITQ), WS(
     2   ISBSIS), WS(IA1), WS(IA1T), WS(IA2), WS(IA2T), WS(IA3), WS(
     3   IA3T), WS(IA4), WS(IA4T), WS(IF1), WS(IF1T), WS(IF2), WS(IF2T),
     4   LS(IZERO), WS(IGSSIS), (NX-K)*NU, 2*K*NU-1, 2*MQ*K)
      CALL LEAVE
      DGLSIN = FAILED
      RETURN
      END