      SUBROUTINE DZ1PG(N, F, F2, QT, R, P, G, P2, G2, SING, AUX)
      INTEGER N
      LOGICAL SING
      DOUBLE PRECISION F(N), F2, QT(N, N), R(N, N), P(N), G(N)
      DOUBLE PRECISION P2, G2, AUX(N)
      INTEGER J, IROLD, NERROR, NERR
      DOUBLE PRECISION DNRM2, D1MACH
C GET P=NEWTON STEP, P2= ITS 2-NORM,
C     G=STEEPEST-DESCENT STEP, G2=ITS 2-NORM
C INPUTS
C N = NUMBER OF COMPONENTS
C F = FUNCTION VECTOR
C F2 = ITS NORM
C Q*R = APPROXIMATE JACOBIAN AT F
C Q IS ORTHOGONAL, TRANSPOSE STORED IN QT
C R IS UPPER TRIANGULAR
C SCRATCH ARRAY AUX
C OUTPUTS
C G AND G2
C IF R IS NON-SINGULAR, P AND P2 ARE CALCULATED, SING LEFT ALONE.
C IF R IS SINGULAR, SET P2=LARGE NUMBER, P=0-VECTOR, SING=TRUE.
      DO  1 J = 1, N
C USE P FOR SCRATCH
         P(J) = F(J)/F2
   1     CONTINUE
C NEGATIVE OF STEEPEST DESCENT DIRECTION
      CALL DZ1VM(N, P, QT, R, AUX, G)
      G2 = DNRM2(N, G, 1)
      IF (G2 .LE. 0.0D0) RETURN
      DO  2 J = 1, N
         G(J) = (-G(J))/G2
   2     CONTINUE
C USE P FOR SCRATCH
      CALL DZ1MV(N, QT, R, G, AUX, P)
      P2 = DNRM2(N, P, 1)
      G2 = (F2*(G2/P2))/P2
      IF (G2 .LE. 0.0D0) RETURN
      DO  3 J = 1, N
C NOW G IS OF PROPER LENGTH
         G(J) = G2*G(J)
   3     CONTINUE
      IF (SING) GOTO 7
         CALL ENTSRC(IROLD, 1)
C ENTER RECOVERY MODE
C NEGATIVE OF NEWTON DIRECTION
         CALL DZ1SOL(N, QT, R, F, AUX, P)
         IF (NERROR(NERR) .EQ. 0) GOTO 4
            SING = .TRUE.
C MATRIX R IS SINGULAR
            CALL ERROFF
            GOTO  6
   4        P2 = DNRM2(N, P, 1)
            DO  5 J = 1, N
               P(J) = -P(J)
   5           CONTINUE
C RESTORE OLD RECOVERY MODE
   6     CALL RETSRC(IROLD)
   7  IF (.NOT. SING) GOTO 9
         P2 = D1MACH(2)
         DO  8 J = 1, N
            P(J) = 0.0D0
   8        CONTINUE
   9  RETURN
      END
