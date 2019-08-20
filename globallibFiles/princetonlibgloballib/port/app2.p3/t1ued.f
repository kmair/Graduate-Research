      SUBROUTINE T1UED ( X, L, S, N, A, F, FD )
C     X = EVALUATION POINT, SCALED FOR (0,1)
C     L = INT(N*X+0.5)
C     S = NUMBER OF REPETITIONS
C     N = NUMBER OF INTERVALS  ( = DIM SPLINE SPACE - 1 )
C     A(3,3,3) = SPLINE COEFFICIENTS
C     F,FD = FUNCTION, DERIVATIVE
      INTEGER N, S, R, J, L
      REAL X, Y, A(1), F(S), FD(S), Z1,Z2,Z3,D1,D2,D3
      J = 1
      IF(L.LT.1)GOTO 50
      IF(L.LT.N)GOTO 90
        J = 2
   50 Y = X - FLOAT(J-1)*(1.-1./FLOAT(N))
      DO 60 R = 1, S
          FD(R) = (A(J+1)-A(J))*FLOAT(N)
          F(R) = A(J) + Y * FD(R)
   60     J = J + 3
      RETURN
   90 Y=FLOAT(N)*X+0.5-FLOAT(L)
      Z3=0.5*Y*Y
      Z2=(-2.*Z3+Y+0.5)
      Z1=(Z3-Y+0.5)
      D3=FLOAT(N)*Y
      D2=FLOAT(N)*(1.-2.*Y)
      D1=FLOAT(N)*(Y-1.)
      DO 190 R = 1, S
         F(R)  = Z1*A(J) + Z2*A(J+1) + Z3*A(J+2)
         FD(R) = D1*A(J) + D2*A(J+1) + D3*A(J+2)
  190    J = J + 3
      RETURN
      END
