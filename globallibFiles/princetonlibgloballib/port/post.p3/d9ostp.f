      SUBROUTINE D9OSTP(T0, S0, T1, S1, NS, N, X, NLEQS, AF, AF1
     1   , BC, BC1, D, D1234, OK, ERROR, ERRPAR, INMI, SCALE)
      INTEGER NS
      EXTERNAL NLEQS, AF, AF1, BC, BC1, D
      EXTERNAL D1234, ERROR, INMI, SCALE
      INTEGER N
      REAL ERRPAR(2)
      LOGICAL NLEQS, OK
      DOUBLE PRECISION T0, S0(NS), T1, S1(NS), X(1)
      COMMON /D90STY/ WV, RV, IV, LV
      INTEGER IV(40)
      REAL RV(30)
      LOGICAL LV(20)
      DOUBLE PRECISION WV(30)
      COMMON /D90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS
      INTEGER NNFS, NRS
      COMMON /D90STK/ NU, NV, K, NX
      INTEGER NU, NV, K, NX
      COMMON /D9OSTT/ TC, DTC
      DOUBLE PRECISION TC, DTC
      COMMON /D9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC
      INTEGER MINIT, MAXIT, KEEJAC
      REAL EGIVE
      DOUBLE PRECISION THETA
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE
      LOGICAL GETJAC, SEPATE
      DOUBLE PRECISION TJ, DTJ
      COMMON /D9OSTF/ FNUM
      INTEGER FNUM
      INTEGER ISTEP
      REAL FLOAT
      DOUBLE PRECISION TSTART, DBLE, T, DT
      LOGICAL TEMP, TEMP1
      EQUIVALENCE (TSTART, WV(1))
C TIME-STEPPING SCHEME FOR POSTS.
C SCRATCH SPACE ALLOCATED -
C     S(D9OSTP) = NU*(NX-K)*(2*K*NU-1) + NV**2 + S(NLEQS).
C LONG REAL WORDS.
      CALL ENTER(1)
      DT = (T1-T0)/DBLE(FLOAT(N))
      DTC = DT
C INITIAL APPROXIMATION FOR S1.
      CALL MOVEFD(NS, S0, S1)
      DO  4 ISTEP = 1, N
         T = T0+(DBLE(FLOAT(ISTEP-1))+THETA)*DT
         TC = T
         NSSS = NSSS+1
         IF (KEEJAC .NE. 1) GOTO 1
            GETJAC = .TRUE.
            TJ = T
   1     TEMP = DT .GT. 0D0
         IF (TEMP) TEMP = T .GT. T1
         IF (TEMP) GOTO 2
            TEMP1 = DT .LT. 0D0
            IF (TEMP1) TEMP1 = T .LT. T1
            TEMP = TEMP1
   2     IF (.NOT. TEMP) GOTO 3
            T = T1
            TC = T
   3     OK = NLEQS(S1(NV+1), NU, S1, NV, T, DT, K, X, NX, AF, AF1, BC
     1      , BC1, D, D1234, ERROR, INMI, SCALE, ERRPAR)
         IF (.NOT. OK) GOTO  5
   4     CONTINUE
   5  CALL LEAVE
      RETURN
      END
