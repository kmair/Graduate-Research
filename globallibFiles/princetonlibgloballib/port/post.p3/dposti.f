      SUBROUTINE DPOSTI(XI, X, XT, XXI, XV, XTV, XXIV, XVV, NX,
     1   UX, UT, NU, V, VT, NV, IV, NVM, A, AX, AU, AUX, AUT, AUTX, AV
     2   , AVT, F, FX, FU, FUX, FUT, FUTX, FV, FVT)
      INTEGER NVM, NU, NV, NX
      INTEGER IV
      DOUBLE PRECISION XI(NX), X(NX), XT(NX), XXI(NX), XV(NX, NVM), XTV(
     1   NX, NVM)
      DOUBLE PRECISION XXIV(NX, NVM), XVV(NX, NVM, NVM), UX(NX, NU), UT(
     1   NX, NU), V(NV), VT(NV)
      DOUBLE PRECISION A(NX, NU), AX(NX, NU), AU(NX, NU, NU), AUX(NX,
     1   NU, NU), AUT(NX, NU, NU), AUTX(NX, NU, NU)
      DOUBLE PRECISION AV(NX, NU, NV), AVT(NX, NU, NV), F(NX, NU), FX(
     1   NX, NU), FU(NX, NU, NU), FUX(NX, NU, NU)
      DOUBLE PRECISION FUT(NX, NU, NU), FUTX(NX, NU, NU), FV(NX, NU, NV)
     1   , FVT(NX, NU, NV)
      INTEGER I, J, L, I1, IX
      DOUBLE PRECISION TERMAT, TERMAX, TERMFT, TERMFX, TERMVV, TERM
      INTEGER TEMP
      LOGICAL TEMP1
C TO PERFORM INTERVAL MAPPING FROM USER COORDINATES BACK INTO INTERNAL
C POST COORDINATES.
C/6S
C     IF (NX .LT. 1) CALL SETERR(18HDPOSTI - NX .LT. 1, 18, 1, 2)
C     IF (NU .LT. 1) CALL SETERR(18HDPOSTI - NU .LT. 1, 18, 2, 2)
C     IF (NV .LT. 0) CALL SETERR(18HDPOSTI - NV .LT. 0, 18, 3, 2)
C/7S
      IF (NX .LT. 1) CALL SETERR('DPOSTI - NX .LT. 1', 18, 1, 2)
      IF (NU .LT. 1) CALL SETERR('DPOSTI - NU .LT. 1', 18, 2, 2)
      IF (NV .LT. 0) CALL SETERR('DPOSTI - NV .LT. 0', 18, 3, 2)
C/
      TEMP1 = IV .LT. 0
      IF (.NOT. TEMP1) TEMP1 = IV .GT. NV
C/6S
C     IF (TEMP1) CALL SETERR(29HDPOSTI - IV MUST BE IN (0,NV), 29, 4, 2)
C/7S
      IF (TEMP1) CALL SETERR('DPOSTI - IV MUST BE IN (0,NV)', 29, 4, 2)
C/
      TEMP1 = NVM .LT. 0
      IF (.NOT. TEMP1) TEMP1 = IV+NVM-1 .GT. NV
C/6S
C     IF (TEMP1) CALL SETERR(30HDPOSTI - NVM MUST BE IN (0,NV), 30, 5, 2
C    1   )
C/7S
      IF (TEMP1) CALL SETERR('DPOSTI - NVM MUST BE IN (0,NV)', 30, 5, 2
     1   )
C/
      DO  13 IX = 1, NX
         TERM = XT(IX)
         J = 1
            GOTO  2
   1        J = J+1
   2        IF (J .GT. NVM) GOTO  3
            TEMP = J+IV
            TERM = TERM+XV(IX, J)*VT(TEMP-1)
            GOTO  1
   3     DO  12 I = 1, NU
            DO  4 J = 1, NV
               FV(IX, I, J) = FV(IX, I, J)*XXI(IX)
               FVT(IX, I, J) = FVT(IX, I, J)*XXI(IX)
   4           CONTINUE
            TERMAX = 0
            TERMFX = 0
            TERMAT = 0
            TERMFT = 0
            DO  5 J = 1, NU
               TERMAX = TERMAX+AUX(IX, I, J)*UX(IX, J)
               TERMFX = TERMFX+FUX(IX, I, J)*UX(IX, J)
               TERMAT = TERMAT+AUT(IX, I, J)*UX(IX, J)
               TERMFT = TERMFT+FUT(IX, I, J)*UX(IX, J)
               AUX(IX, I, J) = (AUX(IX, I, J)-TERM*AUT(IX, I, J))/XXI(
     1            IX)
               FUX(IX, I, J) = FUX(IX, I, J)-TERM*FUT(IX, I, J)
               FUT(IX, I, J) = FUT(IX, I, J)*XXI(IX)
               FU(IX, I, J) = FU(IX, I, J)*XXI(IX)
               TEMP1 = AUTX(IX, I, J) .NE. 0D0
               IF (.NOT. TEMP1) TEMP1 = FUTX(IX, I, J) .NE. 0D0
C/6S
C              IF (TEMP1) CALL SETERR(
C    1            34HDPOSTI - MUST HAVE AUTX = 0 = FUTX, 34, 6, 2)
C/7S
               IF (TEMP1) CALL SETERR(
     1            'DPOSTI - MUST HAVE AUTX = 0 = FUTX', 34, 6, 2)
C/
   5           CONTINUE
            J = 1
               GOTO  7
   6           J = J+1
   7           IF (J .GT. NVM) GOTO  11
               I1 = J+IV-1
               TERMVV = XTV(IX, J)
               L = 1
                  GOTO  9
   8              L = L+1
   9              IF (L .GT. NVM) GOTO  10
                  TEMP = L+IV
                  TERMVV = TERMVV+XVV(IX, L, J)*VT(TEMP-1)
                  GOTO  8
  10           AV(IX, I, I1) = AV(IX, I, I1)+AX(IX, I)*XV(IX, J)-(
     1            TERMAX*XXIV(IX, J)+TERMAT*(TERMVV*XXI(IX)-TERM*XXIV(
     2            IX, J)))/XXI(IX)
               FV(IX, I, I1) = FV(IX, I, I1)+FX(IX, I)*XXI(IX)*XV(IX, J)
     1            -(TERMFX*XXIV(IX, J)+TERMFT*(TERMVV*XXI(IX)-TERM*XXIV(
     2            IX, J)))+XXIV(IX, J)*F(IX, I)
               AVT(IX, I, I1) = AVT(IX, I, I1)-TERMAT*XV(IX, J)
               FVT(IX, I, I1) = FVT(IX, I, I1)-TERMFT*XV(IX, J)*XXI(IX)
               GOTO  6
  11        F(IX, I) = F(IX, I)*XXI(IX)
  12        CONTINUE
  13     CONTINUE
      RETURN
      END
