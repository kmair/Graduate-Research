      LOGICAL FUNCTION DPOSTC(U, NU, NXMK, K, X, NX, V, NV, T, DT,
     1   ERRPAR, ERPUTS, EU, EV)
      INTEGER NXMK, NX
      INTEGER NU, K, NV
      REAL ERRPAR(2), EU(NXMK, 1), EV(1)
      LOGICAL ERPUTS
      DOUBLE PRECISION U(NXMK, 1), X(NX), V(1), T, DT
      INTEGER I, J
      REAL TEMP, DTPOW
      LOGICAL CONGED
      DOUBLE PRECISION DABS
C THE COMPONENT ERROR PROCEDURE FOR DPOSTS.
C SCRATCH SPACE ALLOCATED - NONE.
C U(NXMK,NU),V(NV).
C EU(NXMK,NU),EV(NV).
      IF (.NOT. ERPUTS) GOTO 1
         DTPOW = DABS(DT)
         GOTO  2
   1     DTPOW = 1
   2  CONGED = .TRUE.
      I = 1
         GOTO  4
   3     I = I+1
   4     IF (I .GT. NV) GOTO  5
C ERROR FOR V.
         TEMP = DTPOW*(ERRPAR(1)*DABS(V(I))+ERRPAR(2))
         IF (TEMP .LT. EV(I)) CONGED = .FALSE.
         EV(I) = TEMP
         GOTO  3
   5  J = 1
         GOTO  7
   6     J = J+1
   7     IF (J .GT. NU) GOTO  9
C ERROR FOR U.
         DO  8 I = 1, NXMK
            TEMP = DTPOW*(ERRPAR(1)*DABS(U(I, J))+ERRPAR(2))
            IF (TEMP .LT. EU(I, J)) CONGED = .FALSE.
            EU(I, J) = TEMP
   8        CONTINUE
         GOTO  6
   9  DPOSTC = CONGED
      RETURN
      END
