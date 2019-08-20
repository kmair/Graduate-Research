      LOGICAL FUNCTION D9OSTB(U, UT, NU, V, VT, NV, T, DT, K, X,
     1   NX, GETJAC, SEPATE, BC, ALFA, BETA, GAMMA, ALFAT, BETAT,
     2   GAMMAT, NXMK, NRHS, NRHSG, EU, EUX, EUT, EUTX)
      INTEGER NRHS, NXMK, NU, NX
      EXTERNAL BC
      INTEGER NV, K, NRHSG
      LOGICAL GETJAC, SEPATE
      DOUBLE PRECISION U(NXMK, NU), UT(NXMK, NU), V(1), VT(1), T, DT
      DOUBLE PRECISION X(NX), ALFA(NU, NU, 2), BETA(NU, NU, 2), GAMMA(
     1   NU, NRHS, 2), ALFAT(NU, NU, 2), BETAT(NU, NU, 2)
      DOUBLE PRECISION GAMMAT(NU, NRHS, 2), EU(NU, 2), EUX(NU, 2), EUT(
     1   NU, 2), EUTX(NU, 2)
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      COMMON /D9OSTM/ THETA, EGIVE, IZAP
      INTEGER IZAP(3)
      REAL EGIVE
      DOUBLE PRECISION THETA
      COMMON /D9OSTF/ FNUM
      INTEGER FNUM
      COMMON /DPOSTF/ FAILED
      LOGICAL FAILED
      INTEGER IGAMMA, ISTKGT, IGAMA0, IGAMA1, I, J
      INTEGER L, ID(1), IALFA, IBETA, IS(1000), IALFA1
      INTEGER IBETA1
      REAL RS(1000)
      LOGICAL BEULER, LS(1000)
      DOUBLE PRECISION LR(2), WS(500)
      INTEGER TEMP
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
      DATA ID(1)/1/
C SCRATCH SPACE ALLOCATED - S(D9OSTB) = MAX ( 6*K, S(BC) )
C                                       LONG REAL WORDS.
C (V,VT)(NV).
      CALL ENTER(1)
      BEULER = THETA .EQ. 1D0
      LR(1) = X(1)
C SO THAT BC CAN CLOBBER L AND R.
      LR(2) = X(NX)
      DO  1 I = 1, NU
         EU(I, 1) = U(1, I)
         EU(I, 2) = U(NXMK, I)
         CALL DSPLN1(K, X, NX, U(1, I), X(1), 1, ID, 1, EUX(I, 1))
         CALL DSPLN1(K, X, NX, U(1, I), X(NX), 1, ID, 1, EUX(I, 2))
         EUT(I, 1) = UT(1, I)
         EUT(I, 2) = UT(NXMK, I)
         CALL DSPLN1(K, X, NX, UT(1, I), X(1), 1, ID, 1, EUTX(I, 1))
         CALL DSPLN1(K, X, NX, UT(1, I), X(NX), 1, ID, 1, EUTX(I, 2))
   1     CONTINUE
      FAILED = .FALSE.
      IF (.NOT. GETJAC) GOTO 3
         IGAMMA = ISTKGT(2*NU, 4)
         CALL SETD(2*NU**2, 0D0, ALFA)
         CALL SETD(2*NU**2, 0D0, BETA)
         CALL SETD(2*NU, 0D0, WS(IGAMMA))
         CALL SETD(2*NU**2, 0D0, ALFAT)
         CALL SETD(2*NU**2, 0D0, BETAT)
         CALL SETD(2*NU*NRHS, 0D0, GAMMA)
         CALL SETD(2*NU*NRHS, 0D0, GAMMAT)
         CALL BC(T, LR(1), LR(2), EU, EUX, EUT, EUTX, NU, V, VT, NV, WS(
     1      IGAMMA), ALFA, BETA, ALFAT, BETAT, GAMMA(1, 2, 1), GAMMAT(1,
     2      2, 1))
         IF (NV .LE. 0) GOTO 2
            CALL MOVEBD(NU*NV, GAMMA(1, 1, 2), GAMMA(1, 2, 2))
            CALL MOVEBD(NU*NV, GAMMAT(1, 1, 2), GAMMAT(1, 2, 2))
   2     CALL SETD(NU, 0D0, GAMMAT(1, 1, 1))
         CALL SETD(NU, 0D0, GAMMAT(1, 1, 2))
         GOTO  4
   3     IGAMMA = ISTKGT(2*NU*(4*NU+1+2*NV), 4)
         IALFA = IGAMMA+2*NU
         IBETA = IALFA+2*NU**2
         IALFA1 = IBETA+2*NU**2
         IBETA1 = IALFA1+2*NU**2
         IGAMA0 = IBETA1+2*NU**2
         IGAMA1 = IGAMA0+2*NU*NV
         CALL SETD(2*NU*(4*NU+1+2*NV), 0D0, WS(IGAMMA))
         CALL BC(T, LR(1), LR(2), EU, EUX, EUT, EUTX, NU, V, VT, NV, WS(
     1      IGAMMA), WS(IALFA), WS(IBETA), WS(IALFA1), WS(IBETA1), WS(
     2      IGAMA0), WS(IGAMA1))
   4  IF (.NOT. FAILED) GOTO 5
         FNUM = 2
         CALL LEAVE
         D9OSTB = .TRUE.
         RETURN
   5  IF (NRHSG .LE. 0) GOTO 6
         CALL MOVEFD(NU, WS(IGAMMA), GAMMA)
         TEMP = IGAMMA+NU
         CALL MOVEFD(NU, WS(TEMP), GAMMA(1, 1, 2))
   6  DO  14 L = 1, 2
         DO  13 I = 1, NU
            GAMMA(I, 1, L) = -GAMMA(I, 1, L)
            IF (.NOT. GETJAC) GOTO  13
            DO  9 J = 1, NU
               IF (BEULER) GOTO 7
                  ALFA(I, J, L) = ALFA(I, J, L)*THETA
                  BETA(I, J, L) = BETA(I, J, L)*THETA
   7           IF (SEPATE) GOTO 8
                  ALFA(I, J, L) = ALFA(I, J, L)+ALFAT(I, J, L)/DT
                  BETA(I, J, L) = BETA(I, J, L)+BETAT(I, J, L)/DT
   8           CONTINUE
   9           CONTINUE
            IF (NV .EQ. 0) GOTO  13
            TEMP = NV+1
            DO  12 J = 2, TEMP
               IF (.NOT. BEULER) GAMMA(I, J, L) = GAMMA(I, J, L)*THETA
               IF (SEPATE) GOTO 10
                  GAMMA(I, J, L) = -(GAMMA(I, J, L)+GAMMAT(I, J, L)/DT)
                  GOTO  11
  10              GAMMA(I, J, L) = -GAMMA(I, J, L)
                  GAMMAT(I, J, L) = -GAMMAT(I, J, L)
  11           CONTINUE
  12           CONTINUE
  13        CONTINUE
  14     CONTINUE
      CALL LEAVE
      D9OSTB = .FALSE.
      RETURN
      END
