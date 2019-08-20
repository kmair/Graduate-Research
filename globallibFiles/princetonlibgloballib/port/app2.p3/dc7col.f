      SUBROUTINE DC7COL(C, J, M, P, PP, X)
      INTEGER M, P, PP
      INTEGER J
      DOUBLE PRECISION C(PP, M), X(P)
      EXTERNAL DV7SCL,DV7CPY
      INTEGER I
      DOUBLE PRECISION ONE, NEGONE, ZERO
      DATA NEGONE/-1.D+0/
      DATA ONE/1.D+0/
      DATA ZERO/0.D+0/
C ***  EXTRACT COLUMN J FROM C  WITH PREPENDED IDENTITY MATRIX  ***
      I = IABS(J)
      IF (I .LE. P) GOTO 30
         I = I-P
         IF (J .LE. 0) GOTO 10
            CALL DV7CPY(P, X, C(1, I))
            GOTO  20
  10        CALL DV7SCL(P, X, NEGONE, C(1, I))
  20     CONTINUE
         GOTO  40
  30     CALL DV7SCP(P, X, ZERO)
         X(I) = ONE
         IF (J .LT. 0) X(I) = NEGONE
  40  RETURN
      END
