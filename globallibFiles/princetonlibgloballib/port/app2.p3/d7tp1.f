      REAL FUNCTION  D7TP1(C, J, P, PP, X)
      INTEGER P, PP
      INTEGER J
      REAL C(PP, 1), X(P)
      EXTERNAL  D7TPR
      INTEGER I
      REAL  D7TPR, T
      I = IABS(J)
      IF (I .GT. P) GOTO 10
         T = X(I)
         GOTO  20
  10     I = I-P
         T =  D7TPR(P, C(1, I), X)
  20  IF (J .LT. 0) T = -T
       D7TP1 = T
      RETURN
      END
