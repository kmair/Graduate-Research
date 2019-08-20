      SUBROUTINE POSTU(XI, X, XT, XXI, XV, VT, NX, NV, UX, UT, NU,
     1   AX, FX)
      INTEGER NU, NV, NX
      REAL XI(NX), X(NX), XT(NX), XXI(NX), XV(NX, NV), VT(NV)
      REAL UX(NX, NU), UT(NX, NU), AX(NX, NU), FX(NX, NU)
      INTEGER I, J, IX
      REAL XVSUM
C TO PERFORM INTERVAL MAPPING FROM INTERNAL POST COORDINATES
C INTO USER COORDINATES.
C/6S
C     IF (NX .LT. 1) CALL SETERR(18H POSTU - NX .LT. 1, 18, 1, 2)
C     IF (NV .LT. 0) CALL SETERR(18H POSTU - NV .LT. 0, 18, 2, 2)
C     IF (NU .LT. 1) CALL SETERR(18H POSTU - NU .LT. 1, 18, 3, 2)
C/7S
      IF (NX .LT. 1) CALL SETERR(' POSTU - NX .LT. 1', 18, 1, 2)
      IF (NV .LT. 0) CALL SETERR(' POSTU - NV .LT. 0', 18, 2, 2)
      IF (NU .LT. 1) CALL SETERR(' POSTU - NU .LT. 1', 18, 3, 2)
C/
C MAP INTO USER SYSTEM.
      DO  5 IX = 1, NX
         XVSUM = 0
         J = 1
            GOTO  2
   1        J = J+1
   2        IF (J .GT. NV) GOTO  3
            XVSUM = XVSUM+VT(J)*XV(IX, J)
            GOTO  1
   3     DO  4 I = 1, NU
            UX(IX, I) = UX(IX, I)/XXI(IX)
            UT(IX, I) = UT(IX, I)-UX(IX, I)*(XT(IX)+XVSUM)
   4        CONTINUE
   5     CONTINUE
      CALL SETR(NX*NU, 0E0, AX)
C AX = 0 = FX BY DEFAULT.
      CALL SETR(NX*NU, 0E0, FX)
      RETURN
      END
