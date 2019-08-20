      SUBROUTINE DT1UED ( X, L, S, N, A, F, FD )
C     X = EVALUATION POINT, SCALED FOR (0,1)
C     L = IDINT(N*X+0.5D0)
C     S = NUMBER OF REPETITIONS
C     N = NUMBER OF INTERVALS  ( = DDIM SPLINE SPACE - 1 )
C     A(3,3,3) = SPLINE COEFFICIENTS
C     F,FD = FUNCTION, DERIVATIVE
      INTEGER N, S, R, J, L
      DOUBLE PRECISION X, Y, A(1), F(S), FD(S), Z1,Z2,Z3,D1,D2,D3
      J = 1
      IF(L.LT.1)GOTO 50
      IF(L.LT.N)GOTO 90
        J = 2
   50 Y = X - DBLE(FLOAT(J-1))*(1.D0-1.D0/DBLE(FLOAT(N)))
      DO 60 R = 1, S
          FD(R) = (A(J+1)-A(J))*DBLE(FLOAT(N))
          F(R) = A(J) + Y * FD(R)
   60     J = J + 3
      RETURN
   90 Y=DBLE(FLOAT(N))*X+0.5D0-DBLE(FLOAT(L))
      Z3=0.5D0*Y*Y
      Z2=(-2.D0*Z3+Y+0.5D0)
      Z1=(Z3-Y+0.5D0)
      D3=DBLE(FLOAT(N))*Y
      D2=DBLE(FLOAT(N))*(1.D0-2.D0*Y)
      D1=DBLE(FLOAT(N))*(Y-1.D0)
      DO 190 R = 1, S
         F(R)  = Z1*A(J) + Z2*A(J+1) + Z3*A(J+2)
         FD(R) = D1*A(J) + D2*A(J+1) + D3*A(J+2)
  190    J = J + 3
      RETURN
      END
