      SUBROUTINE POSTS(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT
     1   , AF, BC, D, ERRPAR, HANDLE)
      EXTERNAL AF, BC, D, HANDLE
      INTEGER NU, K, NX, NV
      REAL U(1), X(1), V(1), TSTART, TSTOP, DT
      REAL ERRPAR(2)
      EXTERNAL POSTE
      LOGICAL ERPUTS
C THE FIRST LEVEL OF  POSTS.
C U(NX-K,NU),X(NX),V(NV).
      ERPUTS = .FALSE.
      CALL POST1(U, NU, K, X, NX, V, NV, TSTART, TSTOP, DT, AF, BC, D,
     1   POSTE, ERRPAR, ERPUTS, HANDLE)
      RETURN
      END
