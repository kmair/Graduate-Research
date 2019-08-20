      SUBROUTINE DPOSTV(XI, X, XT, XXI, XV, VT, NV, UX, UT, NU,
     1   BX)
      INTEGER NU, NV
      DOUBLE PRECISION XI(2), X(2), XT(2), XXI(2), XV(2, NV), VT(NV)
      DOUBLE PRECISION UX(NU, 2), UT(NU, 2), BX(NU, 2)
      INTEGER I, J, IX
      DOUBLE PRECISION XVSUM
C TO PERFORM INTERVAL MAPPING FROM INTERNAL POST COORDINATES
C INTO USER COORDINATES.
C/6S
C     IF (NV .LT. 0) CALL SETERR(18HDPOSTV - NV .LT. 0, 18, 1, 2)
C     IF (NU .LT. 1) CALL SETERR(18HDPOSTV - NU .LT. 1, 18, 2, 2)
C/7S
      IF (NV .LT. 0) CALL SETERR('DPOSTV - NV .LT. 0', 18, 1, 2)
      IF (NU .LT. 1) CALL SETERR('DPOSTV - NU .LT. 1', 18, 2, 2)
C/
C MAP INTO USER SYSTEM.
      DO  5 IX = 1, 2
         XVSUM = 0
         J = 1
            GOTO  2
   1        J = J+1
   2        IF (J .GT. NV) GOTO  3
            XVSUM = XVSUM+VT(J)*XV(IX, J)
            GOTO  1
   3     DO  4 I = 1, NU
            UX(I, IX) = UX(I, IX)/XXI(IX)
            UT(I, IX) = UT(I, IX)-UX(I, IX)*(XT(IX)+XVSUM)
   4        CONTINUE
   5     CONTINUE
C BX = 0 BY DEFAULT.
      CALL SETD(2*NU, 0D0, BX)
      RETURN
      END
