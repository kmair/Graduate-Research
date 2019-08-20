      SUBROUTINE G6TH2(A, B, V1, V2, U2, C)
      REAL A, B, V1, V2, U2, C
      REAL ABS, RN, SS, SQRT, U1
C
C THIS SUBROUTINE GENERATES A HOUSEHOLDER TRANSFORMATIONS
C WHICH ZEROES THE ELEMENT B
C
      SS = ABS(A)+ABS(B)
      U1 = A/SS
      U2 = B/SS
      RN = SQRT(U1*U1+U2*U2)
      IF (U1 .LT. 0.E0) RN = -RN
      V1 = (-(U1+RN))/RN
      V2 = (-U2)/RN
      U2 = V2/V1
      C = (-RN)*SS
      RETURN
      END
