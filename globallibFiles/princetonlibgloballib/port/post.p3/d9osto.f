      LOGICAL FUNCTION D9OSTO(U, NU, V, NV, T, DT, K, X, NX, AF,
     1   AF1, BC, BC1, D, D1234, ERROR, INMI, SCALE, ERRPAR, UTHETA, UT,
     2   VTHETA, VT, UOLD, VOLD, EU, EV, EU1, EV1, EU2, EV2, DU, DV,
     3   NXMK, THETA, MINIT, MAXIT, KEEJAC, EGIVE)
      INTEGER NXMK, NX
      EXTERNAL AF, AF1, BC, BC1, D, D1234
      EXTERNAL ERROR, INMI, SCALE
      INTEGER NU, NV, K, MINIT, MAXIT, KEEJAC
      REAL ERRPAR(2), EU(NXMK, 1), EV(1), EU1(NXMK, 1), EV1(1), EU2(
     1   NXMK, 1)
      REAL EV2(1), EGIVE
      LOGICAL ERROR
      DOUBLE PRECISION U(NXMK, 1), V(1), T, DT, X(NX), UTHETA(NXMK, 1)
      DOUBLE PRECISION UT(NXMK, 1), VTHETA(1), VT(1), UOLD(NXMK, 1),
     1   VOLD(1), DU(NXMK, 1)
      DOUBLE PRECISION DV(1), THETA
      COMMON /D90STR/ NJS, NFS, NTSS, NSSS, NNITS, NNDS, NNFS, NRS
      INTEGER NJS, NFS, NTSS, NSSS, NNITS, NNDS
      INTEGER NNFS, NRS
      COMMON /D9OSTL/ ERPUTS
      LOGICAL ERPUTS
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE
      LOGICAL GETJAC, SEPATE
      DOUBLE PRECISION TJ, DTJ
      COMMON /D9OSTF/ FNUM
      INTEGER FNUM
      INTEGER I, J, ITER
      REAL RHO, PROD, TEMP, POWER, R1MACH
      LOGICAL DONE, NCGCE, D90STC
      DOUBLE PRECISION DABS
      INTEGER TEMP2, TEMP3
      LOGICAL TEMP1
C U(NXMK,NU),V(NV).
C           (UTHETA,UT)(NXMK,NU),(VTHETA,VT)(NV).
C UOLD(NXMK,NU),VOLD(NV).
C DU(NXMK,NU),DV(NV).
C REAL EU(NXMK,NU),EV(NV),EU1(NXMK,NU),EV1(NV),EU2(NXMK,NU),EV2(NV).
      IF (NU .LE. 0) GOTO 1
         CALL MOVEFD(NU*(NX-K), U, UOLD)
         CALL SETD(NU*(NX-K), 0D0, UT)
   1  IF (NV .LE. 0) GOTO 2
         CALL MOVEFD(NV, V, VOLD)
         CALL SETD(NV, 0D0, VT)
C GET INITIAL NEWTON METHOD GUESS.
   2  CALL INMI(NU, NV, NX-K, K, X, NX, T, DT, UOLD, VOLD, U, UT, V, VT)
      DO  49 ITER = 1, MAXIT
         IF (KEEJAC .NE. 0) GOTO 3
            GETJAC = .TRUE.
            TJ = T
   3     IF (GETJAC) NJS = NJS+1
         NNITS = NNITS+1
         J = 1
            GOTO  5
   4        J = J+1
   5        IF (J .GT. NU) GOTO  7
            DO  6 I = 1, NXMK
               UTHETA(I, J) = THETA*(U(I, J)-UOLD(I, J))+UOLD(I, J)
   6           CONTINUE
            GOTO  4
   7     I = 1
            GOTO  9
   8        I = I+1
   9        IF (I .GT. NV) GOTO  10
            VTHETA(I) = THETA*(V(I)-VOLD(I))+VOLD(I)
            GOTO  8
  10     DONE = D90STC(UTHETA, UT, NU, VTHETA, VT, NV, T, DT, K, X, NX
     1      , AF, AF1, BC, BC1, D, D1234, SCALE, DU, DV)
         IF (.NOT. DONE) GOTO 11
            DONE = .FALSE.
            GOTO  50
  11     J = 1
            GOTO  13
  12        J = J+1
  13        IF (J .GT. NU) GOTO  15
            DO  14 I = 1, NXMK
               U(I, J) = U(I, J)+DU(I, J)
               EU(I, J) = EGIVE*DABS(DU(I, J))
  14           CONTINUE
            GOTO  12
  15     I = 1
            GOTO  17
  16        I = I+1
  17        IF (I .GT. NV) GOTO  18
            V(I) = V(I)+DV(I)
            EV(I) = EGIVE*DABS(DV(I))
            GOTO  16
  18     IF (MAXIT .NE. 1) GOTO 19
            DONE = .TRUE.
            GOTO  50
  19     CALL MOVEFR(NU*NXMK+NV, EV, EV2)
         DONE = ERROR(U, NU, NX-K, K, X, NX, V, NV, T, DT, ERRPAR,
     1      ERPUTS, EU, EV)
C CHECK FOR NEGATIVE ERROR REQUESTS.
         TEMP2 = NU*NXMK+NV
         DO  21 I = 1, TEMP2
            TEMP1 = EV(I) .EQ. 0.
            IF (TEMP1) TEMP1 = EV2(I) .NE. 0.
            IF (.NOT. TEMP1) TEMP1 = EV(I) .LT. 0.
            IF (.NOT. TEMP1) GOTO 20
C/6S
C              CALL SETERR(37HDESSOM - E(I).LE.0 RETURNED BY SERROR, 37,
C    1            19, 1)
C/7S
               CALL SETERR('DESSOM - E(I).LE.0 RETURNED BY SERROR', 37,
     1            19, 1)
C/
               D9OSTO = .FALSE.
               RETURN
  20        CONTINUE
  21        CONTINUE
         IF (.NOT. DONE) GOTO 22
            GOTO  50
  22        IF (ITER .NE. MAXIT) GOTO 23
               FNUM = 9
               NNFS = NNFS+1
               GOTO  50
  23     CONTINUE
C NO CONVERGENCE.
  24     NCGCE = .FALSE.
         TEMP2 = NU*NXMK+NV
         DO  34 I = 1, TEMP2
            TEMP1 = ITER .GT. MINIT
            IF (TEMP1) TEMP1 = EV(I) .LT. EV2(I)
            IF (.NOT. TEMP1) GOTO 33
               IF (EV1(I) .LE. EV2(I)) GOTO 25
                  RHO = EV2(I)/EV1(I)
C CAN CHECK CONVERGENCE RATE.
                  GOTO  26
  25              RHO = 1
  26           IF (RHO .LT. 1.) GOTO 27
                  NCGCE = .TRUE.
C DIVERGING.
                  GOTO  32
  27              IF (KEEJAC .NE. 0) GOTO 30
                     PROD = 1
C CONVERGING.
C CHECK QUADRATIC CONVERGENCE RATE.
                     POWER = RHO**2
C < 1.
                     TEMP = EV(I)/EV2(I)
                     TEMP3 = MAXIT-ITER
                     DO  28 J = 1, TEMP3
                        PROD = PROD*POWER
                        POWER = POWER**2
                        IF (PROD .LE. TEMP) GOTO  29
  28                    CONTINUE
  29                 IF (PROD .GT. TEMP) NCGCE = .TRUE.
C SLOW CONVERGENCE.
                     GOTO  31
  30                 IF (RHO**(MAXIT-ITER)*EV2(I) .GT. EV(I)) NCGCE =
     1                  .TRUE.
C KEEPJAC > 0 AND SHOULD CHECK LINEAR CONVERGENCE RATE.
C SLOW CONVERGENCE.
  31              CONTINUE
  32           IF (NCGCE) GOTO  35
  33        EV1(I) = EV2(I)
  34        CONTINUE
  35     TEMP1 = NCGCE
         IF (TEMP1) TEMP1 = KEEJAC .EQ. 4
         IF (.NOT. TEMP1) GOTO 39
            IF (T .NE. TJ) GOTO 36
               NNDS = NNDS+1
               FNUM = 8
               D9OSTO = DONE
               RETURN
C HAVE NEW JACOBIAN, DIE.
  36        IF (NU .LE. 0) GOTO 37
               CALL MOVEFD(NU*(NX-K), UOLD, U)
               CALL SETD(NU*(NX-K), 0D0, UT)
  37        IF (NV .LE. 0) GOTO 38
               CALL MOVEFD(NV, VOLD, V)
               CALL SETD(NV, 0D0, VT)
C     GET INITIAL NEWTON METHOD GUESS.
  38        CALL INMI(NU, NV, NX-K, K, X, NX, T, DT, UOLD, VOLD, U, UT
     1         , V, VT)
            TJ = T
            GETJAC = .TRUE.
            FNUM = 0
C EV1 == BIG FOR NEXT ITERATION.
            CALL SETR(NU*NXMK+NV, R1MACH(2), EV1)
            GOTO  49
  39        IF (.NOT. NCGCE) GOTO 40
               NNDS = NNDS+1
               FNUM = 8
               D9OSTO = DONE
               RETURN
  40     CONTINUE
  41     J = 1
            GOTO  43
  42        J = J+1
  43        IF (J .GT. NU) GOTO  45
            DO  44 I = 1, NXMK
               UT(I, J) = UT(I, J)+DU(I, J)/DT
  44           CONTINUE
            GOTO  42
  45     I = 1
            GOTO  47
  46        I = I+1
  47        IF (I .GT. NV) GOTO  48
            VT(I) = VT(I)+DV(I)/DT
            GOTO  46
  48     CONTINUE
  49     CONTINUE
  50  D9OSTO = DONE
      RETURN
      END
