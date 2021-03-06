      SUBROUTINE DGLSBT(K, X, NX, NU, NRHS, NRHSG, GETJAC, A1, A2,
     1   F1, AA, BB, GAM, BC, G, B)
      INTEGER NRHS, NU, NX
      INTEGER K, NRHSG, BC(NU, 2, 2)
      LOGICAL GETJAC
      DOUBLE PRECISION X(NX), A1(NU, NU, 2), A2(NU, NU, 2), F1(NU, NRHS,
     1   2), AA(NU, NU, 2), BB(NU, NU, 2)
      DOUBLE PRECISION GAM(NU, NRHS, 2), G(1, 1), B(1)
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      INTEGER IDF, IDG, ISTKGT, I, IDGI, ISBSIS
      INTEGER IS(1000)
      REAL RS(1000)
      LOGICAL LS(1000)
      DOUBLE PRECISION WS(500)
      INTEGER TEMP1
      LOGICAL TEMP
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C MNEMONIC - DOUBLE PRECISION GALERKIN'S METHOD FOR LINEAR SYSTEMS,
C            BOUNDARY TERMS.
C SCRATCH SPACE ALLOCATED -
C       S(DGLSBT) = NU*(4*NU+2*NRHS) + 2*K
C   LONG REAL WORDS +
C       3*K
C   INTEGER WORDS.
C G(NU*(NX-K),2*K*NU-1),B(NU*(NX-K),NRHS).
C CHECK THE INPUT FOR ERRORS.
C/6S
C     IF (K .LT. 2) CALL SETERR(15HDGLSBT - K.LT.2, 15, 1, 2)
C     IF (NX .LT. 2*K) CALL SETERR(18HDGLSBT - NX.LT.2*K, 18, 2, 2)
C     IF (NU .LT. 1) CALL SETERR(16HDGLSBT - NU.LT.1, 16, 3, 2)
C     IF (NRHS .LT. 1) CALL SETERR(18HDGLSBT - NRHS.LT.1, 18, 4, 2)
C     IF (NRHSG .LT. 0) CALL SETERR(19HDGLSBT - NRHSG.LT.0, 19, 5, 2)
C/7S
      IF (K .LT. 2) CALL SETERR('DGLSBT - K.LT.2', 15, 1, 2)
      IF (NX .LT. 2*K) CALL SETERR('DGLSBT - NX.LT.2*K', 18, 2, 2)
      IF (NU .LT. 1) CALL SETERR('DGLSBT - NU.LT.1', 16, 3, 2)
      IF (NRHS .LT. 1) CALL SETERR('DGLSBT - NRHS.LT.1', 18, 4, 2)
      IF (NRHSG .LT. 0) CALL SETERR('DGLSBT - NRHSG.LT.0', 19, 5, 2)
C/
C CHECK THAT X IS MONOTONE INCREASING.
      DO  1 I = 2, NX
C/6S
C        IF (X(I-1) .GT. X(I)) CALL SETERR(
C    1      37HDGLSBT - X IS NOT MONOTONE INCREASING, 37, 6, 2)
C/7S
         IF (X(I-1) .GT. X(I)) CALL SETERR(
     1      'DGLSBT - X IS NOT MONOTONE INCREASING', 37, 6, 2)
C/
         IF (I+K .GT. NX) GOTO  1
         TEMP1 = I+K
C/6S
C        IF (X(TEMP1) .LE. X(I)) CALL SETERR(
C    1      37HDGLSBT - X IS NOT MONOTONE INCREASING, 37, 6, 2)
C/7S
         IF (X(TEMP1) .LE. X(I)) CALL SETERR(
     1      'DGLSBT - X IS NOT MONOTONE INCREASING', 37, 6, 2)
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
C    1      46HDGLSBT - END POINTS OF X NOT OF MULTIPLICITY K, 46, 7, 2)
C/7S
   2     IF (TEMP) CALL SETERR(
     1      'DGLSBT - END POINTS OF X NOT OF MULTIPLICITY K', 46, 7, 2)
C/
   3     CONTINUE
      CALL ENTER(1)
      IDG = ISTKGT(NU, 4)
      IDGI = ISTKGT(NU, 4)
      IDF = ISTKGT(NRHS, 4)
C SPLINE BASIS AND DERIVATIVES AT L AND R.
      ISBSIS = ISTKGT(2*K, 4)
      CALL D6LSBT(K, X, NX, NU, NRHS, NRHSG, GETJAC, A1, A2, F1, AA, BB,
     1   GAM, BC, G, B, NU*(NX-K), WS(IDG), WS(IDGI), WS(IDF), WS(
     2   ISBSIS))
      CALL LEAVE
      RETURN
      END
