      SUBROUTINE A90STX(TSTART, TSTOP, XA, F, MESH, AF, AF1, BC,
     1   BC1, D, D1, BETA, GAMMA, DELTA, X, NX, DT, N, KMAX, MMAX,
     2   XPOLY, SERROR, ERROR1, ERROR2, ERRPAR, INMI, SCALE, SOUT,
     3   OUTUT1, OUTUT2, PESPAR, HFRACT, KINIT)
      INTEGER MMAX, NX
      EXTERNAL XA, F, AF, AF1, BC, BC1
      EXTERNAL D, D1, SERROR, ERROR1, ERROR2, INMI
      EXTERNAL SCALE, SOUT, OUTUT1, OUTUT2
      INTEGER N(MMAX), KMAX, KINIT
      REAL TSTART, TSTOP, MESH(1), BETA, GAMMA, DELTA
      REAL X(NX), DT, ERRPAR(2), PESPAR, HFRACT
      LOGICAL XPOLY, SERROR
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      COMMON /A90STY/ WV, RV, IV, LV
      INTEGER IV(40)
      REAL WV(30), RV(30)
      LOGICAL LV(20)
      COMMON /A9OSTM/ THETA, EGIVE, MINIT, MAXIT, KEEJAC
      INTEGER MINIT, MAXIT, KEEJAC
      REAL THETA, EGIVE
      COMMON /A9OSTG/ TJ, DTJ, GETJAC, SEPATE
      REAL TJ, DTJ
      LOGICAL GETJAC, SEPATE
      INTEGER NERROR, M, NERR, IE, IS(1000), IX1
      REAL T0, T1, RS(1000), WS(500)
      LOGICAL A4SSOR, A4SSOX, DONE, OK, LS(1000), A4SSOE
      LOGICAL A4SSOI, A4SSOM
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
      EQUIVALENCE (WV(10), T0)
      EQUIVALENCE (WV(11), T1)
      EQUIVALENCE (IV(12), IX1)
      EQUIVALENCE (IV(22), IE)
      EQUIVALENCE (LV(2), OK)
      EQUIVALENCE (LV(7), DONE)
C SCRATCH SPACE ALLOCATED -
C     S(A90STX) <= 2*MMAX + 1 + NX*(KMAX+1) +
C     ( 5*KMAX + 2*MMAX + 3 ) INTEGER +
C     MAX ( S(XA), NX*(KMAX+1) REAL +
C           MAX ( KMAX + KMAX INTEGER, S(SERROR) ),
C           NX REAL + S(SOUT) )
C REAL.
      CALL ENTER(1)
      IF (.NOT. A4SSOI(WV, RV, IV, LV, TSTART, TSTOP, BETA, GAMMA,
     1   DELTA, NX, DT, N, KMAX, MMAX, XPOLY, ERRPAR, PESPAR, HFRACT,
     2   KINIT)) GOTO 1
         CALL LEAVE
         RETURN
   1  IF (T0 .EQ. TSTOP) GOTO  7
C TAKE THE TIME-STEPS.
         IF (KEEJAC .NE. 2) GOTO 2
            GETJAC = T0 .NE. TJ
            TJ = T0
C BUILD THE EXTRAPLOATION LOZENGE.
   2     DO  5 M = 1, MMAX
C GET XA((T1-T0)/N(M)).
            OK = .TRUE.
            CALL XA(T0, X, T1, WS(IX1), NX, N(M), MESH, F, AF, AF1, BC
     1         , BC1, D, D1, OK, ERROR2, ERRPAR, INMI, SCALE)
            IF (OK) GOTO 4
               IF (NERROR(NERR) .EQ. 0) GOTO 3
                  CALL LEAVE
                  RETURN
   3           CONTINUE
C     EXTRAPOLATE THE RESULTS.
   4        IF (A4SSOX(WV, RV, IV, LV, N, M)) GOTO  6
            IF (M .GT. 1) DONE = SERROR(WS(IX1), NX, MESH, T1, DT,
     1         ERRPAR, DELTA, RS(IE), ERROR1, ERROR2)
C CHECK FOR CONVERGENCE.
C     CHECK FOR A RESTART.
            IF (A4SSOR(WV, RV, IV, LV, ERRPAR)) GOTO  6
   5        CONTINUE
C   GET OPTIMAL DT AND ORDER ( LOZENGE SIZE ).
   6     IF (A4SSOM(WV, RV, IV, LV, DT)) GOTO  7
C   OUTPUT THE RESULTS FOR THIS TIME-STEP.
         CALL SOUT(T0, X, MESH, T1, WS(IX1), NX, DT, TSTOP, OK, RS(IE)
     1      , OUTUT1, OUTUT2)
C   WIND-UP THIS TIME-STEP.
         IF (A4SSOE(WV, RV, IV, LV, X, TSTOP, DT)) GOTO  7
         GOTO  1
   7  CALL LEAVE
      RETURN
      END