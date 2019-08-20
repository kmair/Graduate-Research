      SUBROUTINE D9OSTH(T0, U0, V0, T, U, V, NU, NV, K, X, NX, DT,
     1   TSTOP, EU, EV, OK, HANDLE)
      INTEGER NX
      EXTERNAL HANDLE
      INTEGER NU, NV, K
      REAL EU(1), EV(1)
      LOGICAL OK
      DOUBLE PRECISION T0, U0(1), V0(1), T, U(1), V(1)
      DOUBLE PRECISION X(NX), DT, TSTOP
      COMMON /D9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC
      INTEGER MINIT, MAXIT, KEEJAC
      REAL EGIVE
      DOUBLE PRECISION THETA
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE
      LOGICAL GETJAC, SEPATE
      DOUBLE PRECISION TJ, DTJ
      COMMON /D9OSTF/ FNUM
      INTEGER FNUM
      COMMON /D90STT/ TGOOD
      DOUBLE PRECISION TGOOD
      COMMON /D90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS
      INTEGER NNFS, NRS
      LOGICAL TEMP
C OUTPUT FILTER FOR POSTS.
C SCRATCH SPACE ALLOCATED - S(D9OSTH) = S(HANDLE).
C (U0,U)(NX-K,NU),(V0,V)(NV).
C EU(NX-K,NU),EV(NV).
      IF (T0 .EQ. T) GOTO 1
         FNUM = 0
         TGOOD = T
         GOTO  2
   1     NRS = NRS+1
   2  TEMP = T0 .EQ. T
      IF (TEMP) TEMP = KEEJAC .EQ. 3
      IF (.NOT. TEMP) GOTO 3
         GETJAC = T0 .NE. TJ
         TJ = T0
   3  NTSS = NTSS+1
      CALL HANDLE(T0, U0, V0, T, U, V, NU, NX-K, NV, K, X, NX, DT,
     1   TSTOP)
      RETURN
      END