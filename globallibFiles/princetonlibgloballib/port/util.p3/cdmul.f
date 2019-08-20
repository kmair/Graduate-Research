      SUBROUTINE CDMUL(A,B,C)
      DOUBLE PRECISION A(2),B(2),C(2),T(2)
C
      T(1) = A(1)*B(1) - A(2)*B(2)
      T(2) = A(2)*B(1) + A(1)*B(2)
C
      C(1) = T(1)
      C(2) = T(2)
C
      RETURN
      END
