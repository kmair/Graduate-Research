      LOGICAL FUNCTION D9OSTA(X, NX, NU, NRHS, NRHSG, GETJAC,
     1   SEPATE, A1, A1T, A2, A2T, A3, A3T, A4, A4T, F1, F1T, F2, F2T,
     2   INTVAL, SBASIS, K, MESH, NMESH, AF)
      INTEGER K, NRHS, NU, NX, NMESH
      EXTERNAL AF
      INTEGER NRHSG, INTVAL
      LOGICAL GETJAC, SEPATE
      DOUBLE PRECISION X(NX), A1(NX, NU, NU), A1T(NX, NU, NU), A2(NX,
     1   NU, NU), A2T(NX, NU, NU), A3(NX, NU, NU)
      DOUBLE PRECISION A3T(NX, NU, NU), A4(NX, NU, NU), A4T(NX, NU, NU),
     1   F1(NX, NU, NRHS), F1T(NX, NU, NRHS), F2(NX, NU, NRHS)
      DOUBLE PRECISION F2T(NX, NU, NRHS), SBASIS(NX, K, 2), MESH(NMESH)
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      COMMON /DPOSTF/ FAILED
      LOGICAL FAILED
      COMMON /D9OSTT/ T, DT
      DOUBLE PRECISION T, DT
      COMMON /D9OSTM/ THETA, EGIVE, IZAP
      INTEGER IZAP(3)
      REAL EGIVE
      DOUBLE PRECISION THETA
      COMMON /D9OSTK/ IUTETA, IVTETA, IUT, IVT
      INTEGER IUTETA, IVTETA, IUT, IVT
      COMMON /D9OSTF/ FNUM
      INTEGER FNUM
      COMMON /D90STV/ IEU
      INTEGER IEU
      COMMON /D90STK/ IZAP1, NV, IZAP2
      INTEGER IZAP1, NV, IZAP2(2)
      INTEGER I, J, IS(1000), IX
      REAL RS(1000)
      LOGICAL BEULER, LS(1000)
      DOUBLE PRECISION WS(500)
      INTEGER TEMP, TEMP1, TEMP2
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C TO LINEARIZE THE AF RESULTS.
C SCRATCH SPACE ALLOCATED - S(D9OSTA) =
C                           S(AF) + 2*NX*NU*(NV+2)
C LONG REAL WORDS.
      BEULER = THETA .EQ. 1D0
      CALL DQSPLN(WS(IUTETA), NU, NMESH-K, K, X, NX, INTVAL, SBASIS, 2
     1   , WS(IEU))
      TEMP2 = IEU+2*NX*NU
      CALL DQSPLN(WS(IUT), NU, NMESH-K, K, X, NX, INTVAL, SBASIS, 2, WS(
     1   TEMP2))
      FAILED = .FALSE.
      TEMP2 = IEU+NX*NU
      TEMP1 = IEU+2*NX*NU
      TEMP = IEU+3*NX*NU
      CALL AF(T, X, NX, WS(IEU), WS(TEMP2), WS(TEMP1), WS(TEMP), NU, WS(
     1   IVTETA), WS(IVT), NV, F1, A2, A1, A2T, A1T, F1(1, 1, 2), F1T(1,
     2   1, 2), F2, A4, A3, A4T, A3T, F2(1, 1, 2), F2T(1, 1, 2))
      IF (.NOT. FAILED) GOTO 1
         FNUM = 1
         D9OSTA = .TRUE.
         RETURN
   1  DO  10 IX = 1, NX
         DO  9 I = 1, NU
            F1(IX, I, 1) = -F1(IX, I, 1)
            IF (.NOT. GETJAC) GOTO  9
            DO  4 J = 1, NU
               IF (BEULER) GOTO 2
                  A1(IX, I, J) = A1(IX, I, J)*THETA
                  A2(IX, I, J) = A2(IX, I, J)*THETA
                  A3(IX, I, J) = A3(IX, I, J)*THETA
                  A4(IX, I, J) = A4(IX, I, J)*THETA
   2           IF (SEPATE) GOTO 3
                  A1(IX, I, J) = A1(IX, I, J)+A1T(IX, I, J)/DT
                  A2(IX, I, J) = A2(IX, I, J)+A2T(IX, I, J)/DT
                  A3(IX, I, J) = A3(IX, I, J)+A3T(IX, I, J)/DT
                  A4(IX, I, J) = A4(IX, I, J)+A4T(IX, I, J)/DT
   3           CONTINUE
   4           CONTINUE
            IF (NV .EQ. 0) GOTO  9
            TEMP = NV+1
            DO  8 J = 2, TEMP
               IF (BEULER) GOTO 5
                  F1(IX, I, J) = F1(IX, I, J)*THETA
                  F2(IX, I, J) = F2(IX, I, J)*THETA
   5           IF (SEPATE) GOTO 6
                  F1(IX, I, J) = -(F1(IX, I, J)+F1T(IX, I, J)/DT)
                  F2(IX, I, J) = F2(IX, I, J)+F2T(IX, I, J)/DT
                  GOTO  7
   6              F1(IX, I, J) = -F1(IX, I, J)
                  F1T(IX, I, J) = -F1T(IX, I, J)
   7           CONTINUE
   8           CONTINUE
   9        CONTINUE
  10     CONTINUE
      D9OSTA = .FALSE.
      RETURN
      END
