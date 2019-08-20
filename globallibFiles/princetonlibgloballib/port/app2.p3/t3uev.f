      SUBROUTINE T3UEV ( X, N1,N2,N3, A, NA1, NA2, F,F1,F2,F3 )
C     EVALUATE TENSOR PRODUCT SPLINE AND FIRST PARTIALS
C        3-DIM, K=3, UNIF KNOT
C     INPUT
C        X            POINT AT WHICH SPLINE IS TO BE EVALUATED
C        N1,N2,N3     NUMBER OF SAMPLE POINTS
C        A            SPLINE COEFFICIENTS
C     OUTPUT
C        F,F1,F2,F3   VALUE (AND DERIVATIVES) OF SPLINE AT X
      INTEGER N1,N2,N3,L1,L2,L3,J1,J2,J3,I1,I2,I3,N,J,NA1,NA2
      REAL X(3), A(NA1,NA2,N3), F,F1,F2,F3, W(3,3,3)
      REAL FYZ(9), F1YZ(9), FZ(3), F1Z(3), F2Z(3), Z1,Z2,Z3,FD,Y
      L1=INT(FLOAT(N1-1)*X(1)+0.5)
      L2=INT(FLOAT(N2-1)*X(2)+0.5)
      L3=INT(FLOAT(N3-1)*X(3)+0.5)
      J1=MAX(0,MIN(N1-3,L1-1))
      J2=MAX(0,MIN(N2-3,L2-1))
      J3=MAX(0,MIN(N3-3,L3-1))
      DO 100 I1 = 1, 3
         DO 100 I2 = 1, 3
            DO 100 I3 = 1, 3
  100          W(I1,I2,I3) = A(I1+J1,I2+J2,I3+J3)
      CALL T1UED ( X(1), L1, 9, N1-1, W, FYZ, F1YZ )
      CALL T1UED ( X(2), L2, 3, N2-1, FYZ, FZ, F2Z )
      CALL T1UEV ( X(2), L2, 3, N2-1, F1YZ, F1Z )
C     CALL T1UED ( X(3), L3, 1, N3-1, FZ, F, F3 )
C     CALL T1UEV ( X(3), L3, 1, N3-1, F1Z, F1 )
C     CALL T1UEV ( X(3), L3, 1, N3-1, F2Z, F2 )
C       (THIS INLINE CODE SAVES 1MS ON SWIFT)
      N=N3-1
      J = 1
      IF(L3.LT.1)GOTO 250
      IF(L3.LT.N)GOTO 290
        J = 2
  250 Y = X(3) - FLOAT(J-1)*(1.-1./FLOAT(N))
      F3 = (FZ(J+1)-FZ(J))*FLOAT(N)
      F = FZ(J) + Y * F3
      FD = (F1Z(J+1)-F1Z(J))*FLOAT(N)
      F1 = F1Z(J) + Y * FD
      FD = (F2Z(J+1)-F2Z(J))*FLOAT(N)
      F2 = F2Z(J) + Y * FD
      RETURN
  290 Y=FLOAT(N)*X(3)+0.5-FLOAT(L3)
      Z3=0.5*Y*Y
      Z2=(-2.*Z3+Y+0.5)
      Z1=(Z3-Y+0.5)
      F  = Z1*FZ(1) + Z2*FZ(2) + Z3*FZ(3)
      F1 = Z1*F1Z(1) + Z2*F1Z(2) + Z3*F1Z(3)
      F2 = Z1*F2Z(1) + Z2*F2Z(2) + Z3*F2Z(3)
      F3 = FLOAT(N)*(Y*FZ(3) + (1.-2.*Y)*FZ(2) + (Y-1.)*FZ(1))
      RETURN
      END
