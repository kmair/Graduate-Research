      INTEGER FUNCTION L5NKF(TOP, LLIST, K)
      INTEGER TOP
      INTEGER LLIST(2, TOP), K
      INTEGER I, J
      I = TOP
      J = 1
         GOTO  2
   1     J = J+1
   2     IF (J .GT. K) GOTO  3
         I = LLIST(1, I)
         GOTO  1
   3  L5NKF = I
      RETURN
      END
