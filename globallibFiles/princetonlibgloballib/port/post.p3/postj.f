      SUBROUTINE POSTJ(XI, X, XT, XXI, XV, XTV, XXIV, XVV, UX, UT,
     1   NU, V, VT, NV, IV, NVM, B, BX, BU, BUX, BUT, BUTX, BV, BVT)
      INTEGER NVM, NU, NV
      INTEGER IV
      REAL XI(2), X(2), XT(2), XXI(2), XV(2, NVM), XTV(2, NVM)
      REAL XXIV(2, NVM), XVV(2, NVM, NVM), UX(NU, 2), UT(NU, 2), V(NV)
     1   , VT(NV)
      REAL B(NU, 2), BX(NU, 2), BU(NU, NU, 2), BUX(NU, NU, 2), BUT(NU,
     1   NU, 2), BUTX(NU, NU, 2)
      REAL BV(NU, NV, 2), BVT(NU, NV, 2)
      INTEGER I, J, L, I1, IX
      REAL TERMBT, TERMBX, TERMVV, TERM
      INTEGER TEMP
      LOGICAL TEMP1
C TO PERFORM INTERVAL MAPPING FROM USER COORDINATES BACK INTO INTERNAL
C POST COORDINATES.
C/6S
C     IF (NU .LT. 1) CALL SETERR(18H POSTJ - NU .LT. 1, 18, 1, 2)
C     IF (NV .LT. 0) CALL SETERR(18H POSTJ - NV .LT. 0, 18, 2, 2)
C/7S
      IF (NU .LT. 1) CALL SETERR(' POSTJ - NU .LT. 1', 18, 1, 2)
      IF (NV .LT. 0) CALL SETERR(' POSTJ - NV .LT. 0', 18, 2, 2)
C/
      TEMP1 = IV .LT. 0
      IF (.NOT. TEMP1) TEMP1 = IV .GT. NV
C/6S
C     IF (TEMP1) CALL SETERR(29H POSTJ - IV MUST BE IN (0,NV), 29, 3, 2)
C/7S
      IF (TEMP1) CALL SETERR(' POSTJ - IV MUST BE IN (0,NV)', 29, 3, 2)
C/
      TEMP1 = NVM .LT. 0
      IF (.NOT. TEMP1) TEMP1 = IV+NVM-1 .GT. NV
C/6S
C     IF (TEMP1) CALL SETERR(30H POSTJ - NVM MUST BE IN (0,NV), 30, 4, 2
C    1   )
C/7S
      IF (TEMP1) CALL SETERR(' POSTJ - NVM MUST BE IN (0,NV)', 30, 4, 2
     1   )
C/
      DO  12 IX = 1, 2
         TERM = XT(IX)
         J = 1
            GOTO  2
   1        J = J+1
   2        IF (J .GT. NVM) GOTO  3
            TEMP = J+IV
            TERM = TERM+XV(IX, J)*VT(TEMP-1)
            GOTO  1
   3     DO  11 I = 1, NU
            TERMBX = 0
            TERMBT = 0
            DO  4 J = 1, NU
               TERMBX = TERMBX+BUX(I, J, IX)*UX(J, IX)
               TERMBT = TERMBT+BUT(I, J, IX)*UX(J, IX)
               BUX(I, J, IX) = BUX(I, J, IX)-TERM*BUT(I, J, IX)
               BUX(I, J, IX) = BUX(I, J, IX)/XXI(IX)
C/6S
C              IF (BUTX(I, J, IX) .NE. 0.) CALL SETERR(
C    1            27H POSTJ - MUST HAVE BUTX = 0, 27, 5, 2)
C/7S
               IF (BUTX(I, J, IX) .NE. 0.) CALL SETERR(
     1            ' POSTJ - MUST HAVE BUTX = 0', 27, 5, 2)
C/
   4           CONTINUE
            J = 1
               GOTO  6
   5           J = J+1
   6           IF (J .GT. NVM) GOTO  10
               I1 = J+IV-1
               TERMVV = XTV(IX, J)
               L = 1
                  GOTO  8
   7              L = L+1
   8              IF (L .GT. NVM) GOTO  9
                  TEMP = L+IV
                  TERMVV = TERMVV+XVV(IX, L, J)*VT(TEMP-1)
                  GOTO  7
   9           BV(I, I1, IX) = BV(I, I1, IX)+BX(I, IX)*XV(IX, J)-(
     1            TERMBX*XXIV(IX, J)+TERMBT*(TERMVV*XXI(IX)-TERM*XXIV(
     2            IX, J)))/XXI(IX)
               BVT(I, I1, IX) = BVT(I, I1, IX)-TERMBT*XV(IX, J)
               GOTO  5
  10        CONTINUE
  11        CONTINUE
  12     CONTINUE
      RETURN
      END
