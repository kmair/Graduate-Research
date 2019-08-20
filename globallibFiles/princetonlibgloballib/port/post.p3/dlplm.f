      SUBROUTINE DLPLM(XI, NX, V, NV, X, XXI, XXIV, XV, XVV, XT,
     1   XTV)
      INTEGER NV, NX
      DOUBLE PRECISION XI(NX), V(NV), X(NX), XXI(NX), XXIV(NX, NV), XV(
     1   NX, NV)
      DOUBLE PRECISION XVV(NX, NV, NV), XT(NX), XTV(NX, NV)
      INTEGER MIN0, I, IX
      REAL FLOAT
      DOUBLE PRECISION DBLE
      LOGICAL TEMP
C X(XI,V) IS A PIECEWISE LINEAR MAP IN XI, V == BREAKPOINTS.
C/6S
C     IF (NX .LT. 1) CALL SETERR(27HDLPLM - MUST HAVE NX .GE. 1, 27, 1
C    1   , 2)
C     IF (NV .LT. 2) CALL SETERR(27HDLPLM - MUST HAVE NV .GE. 2, 27, 2
C    1   , 2)
C/7S
      IF (NX .LT. 1) CALL SETERR('DLPLM - MUST HAVE NX .GE. 1', 27, 1
     1   , 2)
      IF (NV .LT. 2) CALL SETERR('DLPLM - MUST HAVE NV .GE. 2', 27, 2
     1   , 2)
C/
      CALL SETD(NX*NV, 0D0, XXIV)
      CALL SETD(NX*NV, 0D0, XV)
      CALL SETD(NX*NV**2, 0D0, XVV)
      CALL SETD(NX, 0D0, XT)
      CALL SETD(NX*NV, 0D0, XTV)
      DO  1 IX = 1, NX
         TEMP = XI(IX) .LT. 0D0
         IF (.NOT. TEMP) TEMP = XI(IX) .GT. DBLE(FLOAT(NV-1))
C/6S
C        IF (TEMP) CALL SETERR(30HDLPLM - XI(IX) NOT IN (0,NV-1), 30, 3,
C    1      2)
C/7S
         IF (TEMP) CALL SETERR('DLPLM - XI(IX) NOT IN (0,NV-1)', 30, 3,
     1      2)
C/
         I = XI(IX)
C THE INDEX OF XI(IX) IN V.
         I = I+1
C DO NOT RUN OFF THE END.
         I = MIN0(NV-1, I)
         X(IX) = V(I)+(V(I+1)-V(I))*(XI(IX)-DBLE(FLOAT(I-1)))
         XV(IX, I) = DBLE(FLOAT(I))-XI(IX)
         XV(IX, I+1) = XI(IX)-DBLE(FLOAT(I-1))
         XXI(IX) = V(I+1)-V(I)
         XXIV(IX, I) = -1
         XXIV(IX, I+1) = 1
   1     CONTINUE
      RETURN
      END
