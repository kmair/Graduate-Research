      SUBROUTINE LPLM(XI, NX, V, NV, X, XXI, XXIV, XV, XVV, XT,
     1   XTV)
      INTEGER NV, NX
      REAL XI(NX), V(NV), X(NX), XXI(NX), XXIV(NX, NV), XV(NX, NV)
      REAL XVV(NX, NV, NV), XT(NX), XTV(NX, NV)
      INTEGER MIN0, I, IX
      REAL FLOAT
      LOGICAL TEMP
C X(XI,V) IS A PIECEWISE LINEAR MAP IN XI, V == BREAKPOINTS.
C/6S
C     IF (NX .LT. 1) CALL SETERR(27H LPLM - MUST HAVE NX .GE. 1, 27, 1
C    1   , 2)
C     IF (NV .LT. 2) CALL SETERR(27H LPLM - MUST HAVE NV .GE. 2, 27, 2
C    1   , 2)
C/7S
      IF (NX .LT. 1) CALL SETERR(' LPLM - MUST HAVE NX .GE. 1', 27, 1
     1   , 2)
      IF (NV .LT. 2) CALL SETERR(' LPLM - MUST HAVE NV .GE. 2', 27, 2
     1   , 2)
C/
      CALL SETR(NX*NV, 0E0, XXIV)
      CALL SETR(NX*NV, 0E0, XV)
      CALL SETR(NX*NV**2, 0E0, XVV)
      CALL SETR(NX, 0E0, XT)
      CALL SETR(NX*NV, 0E0, XTV)
      DO  1 IX = 1, NX
         TEMP = XI(IX) .LT. 0.
         IF (.NOT. TEMP) TEMP = XI(IX) .GT. FLOAT(NV-1)
C/6S
C        IF (TEMP) CALL SETERR(30H LPLM - XI(IX) NOT IN (0,NV-1), 30, 3,
C    1      2)
C/7S
         IF (TEMP) CALL SETERR(' LPLM - XI(IX) NOT IN (0,NV-1)', 30, 3,
     1      2)
C/
         I = XI(IX)
C THE INDEX OF XI(IX) IN V.
         I = I+1
C DO NOT RUN OFF THE END.
         I = MIN0(NV-1, I)
         X(IX) = V(I)+(V(I+1)-V(I))*(XI(IX)-FLOAT(I-1))
         XV(IX, I) = FLOAT(I)-XI(IX)
         XV(IX, I+1) = XI(IX)-FLOAT(I-1)
         XXI(IX) = V(I+1)-V(I)
         XXIV(IX, I) = -1
         XXIV(IX, I+1) = 1
   1     CONTINUE
      RETURN
      END
