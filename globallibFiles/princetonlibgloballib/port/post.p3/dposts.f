      SUBROUTINE DPOSTS(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT,
     1   AF, BC, D, ERRPAR, HANDLE)
      EXTERNAL AF, BC, D, HANDLE
      INTEGER NU, K, NX, NV
      REAL ERRPAR(2)
      DOUBLE PRECISION U(1), X(1), V(1), TSTART, TSTOP, DT
      EXTERNAL DPOSTE
      LOGICAL ERPUTS
C THE FIRST LEVEL OF DPOSTS.
C U(NX-K,NU),X(NX),V(NV).
      ERPUTS = .FALSE.
      CALL DPOST1(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT, AF, BC, D
     1   , DPOSTE, ERRPAR, ERPUTS, HANDLE)
      RETURN
      END
