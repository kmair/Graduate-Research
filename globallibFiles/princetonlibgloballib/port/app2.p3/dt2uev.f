      SUBROUTINE DT2UEV ( X, N1,N2, A, NA1, F,F1,F2 )
C     EVALUATE TENSOR PRODUCT SPLINE AND FIRST PARTIALS
C        2-DDIM, K=3, UNIF KNOT
C     INPUT
C        X            POINT AT WHICH SPLINE IS TO BE EVALUATED
C        N1,N2        NUMBER OF SAMPLE POINTS
C        A            SPLINE COEFFICIENTS
C     OUTPUT
C        F,F1,F2      VALUE (AND DERIVATIVES) OF SPLINE AT X
      INTEGER N1,N2,L1,L2,J1,J2,I1,I2,N,J,NA1
      DOUBLE PRECISION X(2), A(NA1,N2), F,F1,F2, W(3,3)
      DOUBLE PRECISION FY(3), F1Y(3), Z1,Z2,Z3,FD,Y
      L1=IDINT(DBLE(FLOAT(N1-1))*X(1)+0.5D0)
      L2=IDINT(DBLE(FLOAT(N2-1))*X(2)+0.5D0)
      J1=MAX(0,MIN(N1-3,L1-1))
      J2=MAX(0,MIN(N2-3,L2-1))
      DO 100 I1 = 1, 3
         DO 100 I2 = 1, 3
  100        W(I1,I2) = A(I1+J1,I2+J2)
      CALL DT1UED ( X(1), L1, 3, N1-1, W, FY, F1Y )
C     CALL DT1UED ( X(2), L2, 1, N2-1, FY, F, F2 )
C     CALL DT1UEV ( X(2), L2, 1, N2-1, F1Y, F1 )
      N=N2-1
      J = 1
      IF(L2.LT.1)GOTO 250
      IF(L2.LT.N)GOTO 290
        J = 2
  250 Y = X(2) - DBLE(FLOAT(J-1))*(1.D0-1.D0/DBLE(FLOAT(N)))
      F2 = (FY(J+1)-FY(J))*DBLE(FLOAT(N))
      F = FY(J) + Y * F2
      FD = (F1Y(J+1)-F1Y(J))*DBLE(FLOAT(N))
      F1 = F1Y(J) + Y * FD
      RETURN
  290 Y=DBLE(FLOAT(N))*X(2)+0.5D0-DBLE(FLOAT(L2))
      Z3=0.5D0*Y*Y
      Z2=(-2.D0*Z3+Y+0.5D0)
      Z1=(Z3-Y+0.5D0)
      F  = Z1*FY(1) + Z2*FY(2) + Z3*FY(3)
      F1 = Z1*F1Y(1) + Z2*F1Y(2) + Z3*F1Y(3)
      F2 = DBLE(FLOAT(N))*(Y*FY(3) + (1.D0-2.D0*Y)*FY(2) +
     1          (Y-1.D0)*FY(1))
      RETURN
      END
