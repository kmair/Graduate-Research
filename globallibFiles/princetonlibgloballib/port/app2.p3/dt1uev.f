      SUBROUTINE DT1UEV ( X, L, S, N, A, F )
      INTEGER N, S, R, J, L
      DOUBLE PRECISION X, Y, A(1), F(S), FD, Z1,Z2,Z3
      J = 1
      IF(L.LT.1)GOTO 50
      IF(L.LT.N)GOTO 90
        J = 2
   50 Y = X - DBLE(FLOAT(J-1))*(1.D0-1.D0/DBLE(FLOAT(N)))
      DO 60 R = 1, S
          FD = (A(J+1)-A(J))*DBLE(FLOAT(N))
          F(R) = A(J) + Y * FD
   60     J = J + 3
      RETURN
   90 Y=DBLE(FLOAT(N))*X+0.5D0-DBLE(FLOAT(L))
      Z3=0.5D0*Y*Y
      Z2=(-2.D0*Z3+Y+0.5D0)
      Z1=(Z3-Y+0.5D0)
      DO 190 R = 1, S
         F(R)  = Z1*A(J) + Z2*A(J+1) + Z3*A(J+2)
  190    J = J + 3
      RETURN
      END
