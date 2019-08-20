      DOUBLE PRECISION FUNCTION DD7TP1(C, J, P, PP, X)
      INTEGER P, PP
      INTEGER J
      DOUBLE PRECISION C(PP, 1), X(P)
      EXTERNAL DD7TPR
      INTEGER I
      DOUBLE PRECISION DD7TPR, T
      I = IABS(J)
      IF (I .GT. P) GOTO 10
         T = X(I)
         GOTO  20
  10     I = I-P
         T = DD7TPR(P, C(1, I), X)
  20  IF (J .LT. 0) T = -T
      DD7TP1 = T
      RETURN
      END
