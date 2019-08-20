      SUBROUTINE T1UEV ( X, L, S, N, A, F )
      INTEGER N, S, R, J, L
      REAL X, Y, A(1), F(S), FD, Z1,Z2,Z3
      J = 1
      IF(L.LT.1)GOTO 50
      IF(L.LT.N)GOTO 90
        J = 2
   50 Y = X - FLOAT(J-1)*(1.-1./FLOAT(N))
      DO 60 R = 1, S
          FD = (A(J+1)-A(J))*FLOAT(N)
          F(R) = A(J) + Y * FD
   60     J = J + 3
      RETURN
   90 Y=FLOAT(N)*X+0.5-FLOAT(L)
      Z3=0.5*Y*Y
      Z2=(-2.*Z3+Y+0.5)
      Z1=(Z3-Y+0.5)
      DO 190 R = 1, S
         F(R)  = Z1*A(J) + Z2*A(J+1) + Z3*A(J+2)
  190    J = J + 3
      RETURN
      END
