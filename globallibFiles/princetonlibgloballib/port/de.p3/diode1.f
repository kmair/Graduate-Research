      SUBROUTINE DIODE1(V, NV, TSTART, TSTOP, DT, D, ERROR,
     1   ERRPAR, ERPUTS, HANDLE)
      INTEGER NV
      EXTERNAL D, ERROR, HANDLE
      REAL ERRPAR(2)
      LOGICAL ERPUTS
      DOUBLE PRECISION V(NV), TSTART, TSTOP, DT
      INTEGER KMAX, KINIT
      LOGICAL EQUIL, XPOLY
C THE SECOND LEVEL OF DIODES.
      KMAX = 10
      XPOLY = .FALSE.
      KINIT = 2
      EQUIL = .TRUE.
      CALL DIODE2(V, NV, TSTART, TSTOP, DT, D, EQUIL, KMAX, XPOLY,
     1   KINIT, ERROR, ERRPAR, ERPUTS, HANDLE)
      RETURN
      END
