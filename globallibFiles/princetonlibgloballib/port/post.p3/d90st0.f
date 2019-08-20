      LOGICAL FUNCTION D90ST0(K, X, NX, U, UT, NU, V, VT, NV, T,
     1   DT, UOFV, D1234, D, SCALE, DV, D40, D41, D4, DMAT, D50, D51,
     2   D5, JACDIM, NXMK, DIAG, PIVOT)
      INTEGER JACDIM, NV, NX
      EXTERNAL D1234, D, SCALE
      INTEGER K, NU, NXMK, PIVOT(NV)
      LOGICAL D1234
      DOUBLE PRECISION X(NX), U(JACDIM), UT(JACDIM), V(NV), VT(NV), T
      DOUBLE PRECISION DT, UOFV(JACDIM, 2), DV(NV), D40(NV, NV), D41(NV,
     1   NV), D4(NV, NV)
      DOUBLE PRECISION DMAT(NV, NV), D50(NV, JACDIM), D51(NV, JACDIM),
     1   D5(NV, JACDIM), DIAG(NV)
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      COMMON /D9OSTT/ TIME, DELTAT
      DOUBLE PRECISION TIME, DELTAT
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE
      LOGICAL GETJAC, SEPATE
      DOUBLE PRECISION TJ, DTJ
      COMMON /D9OSTF/ FNUM
      INTEGER FNUM
      INTEGER ISTKGT, NERROR, I, J, NERR, IS(1000)
      INTEGER ITEMP
      REAL RS(1000)
      LOGICAL GETACT, NEESUM, LS(1000)
      DOUBLE PRECISION WS(500)
      INTEGER TEMP
      LOGICAL TEMP1
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C (U,UT)(NX-K,NU),
C UOFV(NX-K,NU,NV+1).
      NEESUM = DT .NE. DTJ
      NEESUM = NEESUM .AND. SEPATE
      TEMP1 = SEPATE
      IF (TEMP1) TEMP1 = GETJAC
      IF (.NOT. TEMP1) GOTO 2
         TIME = TJ
         IF (.NOT. D1234(K, X, NX, NXMK, U, UT, NU, V, VT, NV, TJ,
     1      GETJAC, SEPATE, D, D50, D51, DV, D40, D41)) GOTO 1
            TIME = T
            D90ST0 = .TRUE.
            RETURN
   1     TIME = T
   2  TEMP1 = GETJAC
      IF (TEMP1) TEMP1 = .NOT. SEPATE
      GETACT = TEMP1
      IF (.NOT. D1234(K, X, NX, NXMK, U, UT, NU, V, VT, NV, T, GETACT,
     1   .FALSE., D, D5, D51, DV, D4, D41)) GOTO 3
         D90ST0 = .TRUE.
         RETURN
   3  DO  4 I = 1, NV
         DV(I) = -DV(I)
   4     CONTINUE
      IF (.NOT. NEESUM) GOTO 10
         DO  6 I = 1, NV
            DO  5 J = 1, NV
               D4(I, J) = D40(I, J)+D41(I, J)/DT
   5           CONTINUE
   6        CONTINUE
         IF (NU .LE. 0) GOTO 9
            DO  8 J = 1, NV
               DO  7 I = 1, JACDIM
                  D5(J, I) = D50(J, I)+D51(J, I)/DT
   7              CONTINUE
   8           CONTINUE
   9     CONTINUE
  10  TEMP1 = GETJAC
      IF (.NOT. TEMP1) TEMP1 = NEESUM
      IF (TEMP1) CALL MOVEFD(NV**2, D4, DMAT)
      TEMP1 = NEESUM
      IF (.NOT. TEMP1) TEMP1 = GETJAC
      IF (TEMP1) TEMP1 = NU .GT. 0
      IF (.NOT. TEMP1) GOTO 13
         ITEMP = ISTKGT(NV**2, 4)
C FORM TEMP = D5 * W.
         CALL DMMPY(D5, NV, JACDIM, UOFV(1, 2), NV, WS(ITEMP))
         DO  12 I = 1, NV
            DO  11 J = 1, NV
               TEMP = ITEMP+I-1+(J-1)*NV
               DMAT(I, J) = DMAT(I, J)-WS(TEMP)
  11           CONTINUE
  12        CONTINUE
         CALL ISTKRL(1)
  13  TEMP1 = NEESUM
      IF (.NOT. TEMP1) TEMP1 = GETJAC
      IF (.NOT. TEMP1) GOTO 15
         CALL SCALE(4, 1, DMAT, NV, NV)
C SCALE THE ODE JACOBIAN.
         CALL DQRD(NV, NV, DMAT, DIAG, PIVOT)
         IF (NERROR(NERR) .EQ. 0) GOTO 14
            CALL ERROFF
            FNUM = 7
            D90ST0 = .TRUE.
            RETURN
  14     CONTINUE
  15  IF (NU .LE. 0) GOTO 17
         ITEMP = ISTKGT(NV, 4)
C FORM DV += D5*W.
         CALL DMMPY(D5, NV, JACDIM, UOFV, 1, WS(ITEMP))
         DO  16 I = 1, NV
            TEMP = ITEMP+I
            DV(I) = DV(I)+WS(TEMP-1)
  16        CONTINUE
         CALL ISTKRL(1)
C SCALE THE ODE RHS.
  17  CALL SCALE(4, 2, DV, NV, 1)
      CALL DQRQTB(NV, NV, DMAT, DIAG, PIVOT, 1, DV, DV)
C/6S
C     IF (NERROR(NERR) .NE. 0) CALL SETERR(
C    1   32HD90STD - SINGULAR DJAC IN DQRQTB, 32, 1, 2)
C/7S
      IF (NERROR(NERR) .NE. 0) CALL SETERR(
     1   'D90STD - SINGULAR DJAC IN DQRQTB', 32, 1, 2)
C/
C UN-SCALE THE ODE SOLUTION.
      CALL SCALE(4, 3, DV, NV, 1)
      D90ST0 = .FALSE.
      RETURN
      END
