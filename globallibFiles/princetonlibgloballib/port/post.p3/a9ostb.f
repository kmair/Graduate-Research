      LOGICAL FUNCTION A9OSTB(U, UT, NU, V, VT, NV, T, DT, K, X,
     1   NX, GETJAC, SEPATE, BC, ALFA, BETA, GAMMA, ALFAT, BETAT,
     2   GAMMAT, NXMK, NRHS, NRHSG, EU, EUX, EUT, EUTX)
      INTEGER NRHS, NXMK, NU, NX
      EXTERNAL BC
      INTEGER NV, K, NRHSG
      REAL U(NXMK, NU), UT(NXMK, NU), V(1), VT(1), T, DT
      REAL X(NX), ALFA(NU, NU, 2), BETA(NU, NU, 2), GAMMA(NU, NRHS, 2)
     1   , ALFAT(NU, NU, 2), BETAT(NU, NU, 2)
      REAL GAMMAT(NU, NRHS, 2), EU(NU, 2), EUX(NU, 2), EUT(NU, 2), EUTX(
     1   NU, 2)
      LOGICAL GETJAC, SEPATE
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      COMMON /A9OSTM/ THETA, EGIVE, IZAP
      INTEGER IZAP(3)
      REAL THETA, EGIVE
      COMMON /A9OSTF/ FNUM
      INTEGER FNUM
      COMMON /POSTF/ FAILED
      LOGICAL FAILED
      INTEGER IGAMMA, ISTKGT, IGAMA0, IGAMA1, I, J
      INTEGER L, ID(1), IALFA, IBETA, IS(1000), IALFA1
      INTEGER IBETA1
      REAL LR(2), RS(1000), WS(500)
      LOGICAL BEULER, LS(1000)
      INTEGER TEMP
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
      DATA ID(1)/1/
C SCRATCH SPACE ALLOCATED - S(A9OSTB) = MAX ( 6*K, S(BC) )
C                                       REAL WORDS.
C (V,VT)(NV).
      CALL ENTER(1)
      BEULER = THETA .EQ. 1.
      LR(1) = X(1)
C SO THAT BC CAN CLOBBER L AND R.
      LR(2) = X(NX)
      DO  1 I = 1, NU
         EU(I, 1) = U(1, I)
         EU(I, 2) = U(NXMK, I)
         CALL SPLN1(K, X, NX, U(1, I), X(1), 1, ID, 1, EUX(I, 1))
         CALL SPLN1(K, X, NX, U(1, I), X(NX), 1, ID, 1, EUX(I, 2))
         EUT(I, 1) = UT(1, I)
         EUT(I, 2) = UT(NXMK, I)
         CALL SPLN1(K, X, NX, UT(1, I), X(1), 1, ID, 1, EUTX(I, 1))
         CALL SPLN1(K, X, NX, UT(1, I), X(NX), 1, ID, 1, EUTX(I, 2))
   1     CONTINUE
      FAILED = .FALSE.
      IF (.NOT. GETJAC) GOTO 3
         IGAMMA = ISTKGT(2*NU, 3)
         CALL SETR(2*NU**2, 0E0, ALFA)
         CALL SETR(2*NU**2, 0E0, BETA)
         CALL SETR(2*NU, 0E0, WS(IGAMMA))
         CALL SETR(2*NU**2, 0E0, ALFAT)
         CALL SETR(2*NU**2, 0E0, BETAT)
         CALL SETR(2*NU*NRHS, 0E0, GAMMA)
         CALL SETR(2*NU*NRHS, 0E0, GAMMAT)
         CALL BC(T, LR(1), LR(2), EU, EUX, EUT, EUTX, NU, V, VT, NV, WS(
     1      IGAMMA), ALFA, BETA, ALFAT, BETAT, GAMMA(1, 2, 1), GAMMAT(1,
     2      2, 1))
         IF (NV .LE. 0) GOTO 2
            CALL MOVEBR(NU*NV, GAMMA(1, 1, 2), GAMMA(1, 2, 2))
            CALL MOVEBR(NU*NV, GAMMAT(1, 1, 2), GAMMAT(1, 2, 2))
   2     CALL SETR(NU, 0E0, GAMMAT(1, 1, 1))
         CALL SETR(NU, 0E0, GAMMAT(1, 1, 2))
         GOTO  4
   3     IGAMMA = ISTKGT(2*NU*(4*NU+1+2*NV), 3)
         IALFA = IGAMMA+2*NU
         IBETA = IALFA+2*NU**2
         IALFA1 = IBETA+2*NU**2
         IBETA1 = IALFA1+2*NU**2
         IGAMA0 = IBETA1+2*NU**2
         IGAMA1 = IGAMA0+2*NU*NV
         CALL SETR(2*NU*(4*NU+1+2*NV), 0E0, WS(IGAMMA))
         CALL BC(T, LR(1), LR(2), EU, EUX, EUT, EUTX, NU, V, VT, NV, WS(
     1      IGAMMA), WS(IALFA), WS(IBETA), WS(IALFA1), WS(IBETA1), WS(
     2      IGAMA0), WS(IGAMA1))
   4  IF (.NOT. FAILED) GOTO 5
         FNUM = 2
         CALL LEAVE
         A9OSTB = .TRUE.
         RETURN
   5  IF (NRHSG .LE. 0) GOTO 6
         CALL MOVEFR(NU, WS(IGAMMA), GAMMA)
         TEMP = IGAMMA+NU
         CALL MOVEFR(NU, WS(TEMP), GAMMA(1, 1, 2))
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
      A9OSTB = .FALSE.
      RETURN
      END
