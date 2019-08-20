      LOGICAL FUNCTION DGLSBI(K, X, NX, AF, AF1, NU, NRHS, NRHSG
     1   , GETJAC, SEPATE, A1, A2, F1, A1T, A2T, F1T)
      INTEGER NRHS, NU, NX
      EXTERNAL AF, AF1
      INTEGER K, NRHSG
      LOGICAL GETJAC, SEPATE
      DOUBLE PRECISION X(NX), A1(NU, NU, 2), A2(NU, NU, 2), F1(NU, NRHS,
     1   2), A1T(NU, NU, 2), A2T(NU, NU, 2)
      DOUBLE PRECISION F1T(NU, NRHS, 2)
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      INTEGER IAS, IFS, ISTKGT, I, ISBSIS, IS(1000)
      REAL RS(1000)
      LOGICAL FAILED, LS(1000), D6LSBI
      DOUBLE PRECISION WS(500)
      INTEGER TEMP1
      LOGICAL TEMP
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C MNEMONIC - DOUBLE PRECISION GALERKIN'S METHOD FOR LINEAR SYSTEMS,
C            BOUNDARY INTEGRAL TERMS.
C SCRATCH SPACE ALLOCATED -
C       S(DGLSBI) = NU*(8*NU+2*NRHS) + 2*K
C   LONG REAL WORDS +
C       MAX(3*K,S(AF))
C   INTEGER WORDS.
C CHECK THE INPUT FOR ERRORS.
C/6S
C     IF (K .LT. 2) CALL SETERR(15HDGLSBI - K.LT.2, 15, 1, 2)
C     IF (NX .LT. 2*K) CALL SETERR(18HDGLSBI - NX.LT.2*K, 18, 2, 2)
C     IF (NU .LT. 1) CALL SETERR(16HDGLSBI - NU.LT.1, 16, 3, 2)
C     IF (NRHS .LT. 1) CALL SETERR(18HDGLSBI - NRHS.LT.1, 18, 4, 2)
C     IF (NRHSG .LT. 0) CALL SETERR(19HDGLSBI - NRHSG.LT.0, 19, 5, 2)
C/7S
      IF (K .LT. 2) CALL SETERR('DGLSBI - K.LT.2', 15, 1, 2)
      IF (NX .LT. 2*K) CALL SETERR('DGLSBI - NX.LT.2*K', 18, 2, 2)
      IF (NU .LT. 1) CALL SETERR('DGLSBI - NU.LT.1', 16, 3, 2)
      IF (NRHS .LT. 1) CALL SETERR('DGLSBI - NRHS.LT.1', 18, 4, 2)
      IF (NRHSG .LT. 0) CALL SETERR('DGLSBI - NRHSG.LT.0', 19, 5, 2)
C/
C CHECK THAT X IS MONOTONE INCREASING.
      DO  1 I = 2, NX
C/6S
C        IF (X(I-1) .GT. X(I)) CALL SETERR(
C    1      37HDGLSBI - X IS NOT MONOTONE INCREASING, 37, 6, 2)
C/7S
         IF (X(I-1) .GT. X(I)) CALL SETERR(
     1      'DGLSBI - X IS NOT MONOTONE INCREASING', 37, 6, 2)
C/
         IF (I+K .GT. NX) GOTO  1
         TEMP1 = I+K
C/6S
C        IF (X(TEMP1) .LE. X(I)) CALL SETERR(
C    1      37HDGLSBI - X IS NOT MONOTONE INCREASING, 37, 6, 2)
C/7S
         IF (X(TEMP1) .LE. X(I)) CALL SETERR(
     1      'DGLSBI - X IS NOT MONOTONE INCREASING', 37, 6, 2)
C/
   1     CONTINUE
C CHECK THE MULTIPLICITY OF THE END POINTS.
      DO  3 I = 1, K
         TEMP = X(I) .NE. X(1)
         IF (TEMP) GOTO 2
            TEMP1 = NX-K+I
            TEMP = X(TEMP1) .NE. X(NX)
C/6S
C  2     IF (TEMP) CALL SETERR(
C    1      46HDGLSBI - END POINTS OF X NOT OF MULTIPLICITY K, 46, 7, 2)
C/7S
   2     IF (TEMP) CALL SETERR(
     1      'DGLSBI - END POINTS OF X NOT OF MULTIPLICITY K', 46, 7, 2)
C/
   3     CONTINUE
      CALL ENTER(1)
C SCRATCH A AND F VALUES.
      IAS = ISTKGT(4*NU*(2*NU+NRHS), 4)
C SCRATCH F1 AND F2 VALUES.
      IFS = IAS+8*NU**2
C SPLINE BASIS AND DERIVATIVES AT L AND R.
      ISBSIS = ISTKGT(2*K, 4)
      FAILED = D6LSBI(K, X, NX, AF, AF1, NU, NRHS, NRHSG, GETJAC,
     1   SEPATE, A1, A2, F1, A1T, A2T, F1T, WS(IAS), WS(IFS), WS(ISBSIS)
     2   )
      CALL LEAVE
      DGLSBI = FAILED
      RETURN
      END
