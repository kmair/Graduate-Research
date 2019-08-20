      SUBROUTINE DPOST2(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT,
     1   AF, BC, D, EQUIL, KMAX, XPOLY, KINIT, ERROR, ERRPAR, ERPUTS,
     2   HANDLE)
      EXTERNAL AF, BC, D, ERROR, HANDLE
      INTEGER NU, K, NX, NV, KMAX, KINIT
      REAL ERRPAR(2)
      LOGICAL EQUIL, XPOLY, ERPUTS
      DOUBLE PRECISION U(1), X(1), V(1), TSTART, TSTOP, DT
      EXTERNAL DPOSTN, DPOSTP
      INTEGER MGQ, NERROR, KEEJAC, NERR, MINIT, MAXIT
      DOUBLE PRECISION T0, T1, THETA
C THE THIRD LEVEL OF DPOSTS.
C U(NX-K,NU),X(NX),V(NV).
C CHECK THE INPUT FOR ERRORS.
      CALL ENTER(1)
      IF (.NOT. EQUIL) GOTO 1
         THETA = 1
         GOTO  2
   1     THETA = 0.5D0
   2  KEEJAC = 0
      IF (.NOT. EQUIL) GOTO 3
         MINIT = 1
         MAXIT = 1
         GOTO  4
   3     MINIT = 10
         MAXIT = 50
   4  T0 = TSTART
      T1 = TSTOP
      MGQ = K-1
         GOTO  6
   5     MGQ = MGQ+1
   6     IF (MGQ .GT. K) GOTO  7
C LOOP UNTIL GQ WORKS.
         CALL DPOST3(U, NU, K, X, NX, V, NV, T0, T1, DT, AF, BC, D,
     1      THETA, KEEJAC, MINIT, MAXIT, MGQ, KMAX, XPOLY, KINIT, ERROR,
     2      ERRPAR, ERPUTS, DPOSTN, DPOSTP, HANDLE)
         IF (NERROR(NERR) .NE. 1011) GOTO  7
         CALL ERROFF
         T0 = T1
         T1 = TSTOP
         GOTO  5
   7  TSTOP = T1
      CALL LEAVE
      RETURN
      END