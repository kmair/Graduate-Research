      SUBROUTINE DPOSTB(T, L, R, U, UX, UT, UTX, NU, V, VT, NV, B,
     1   BU, BUX, BUT, BUTX, BV, BVT)
      INTEGER NU, NV
      DOUBLE PRECISION T, L, R, U(NU, 2), UX(NU, 2), UT(NU, 2)
      DOUBLE PRECISION UTX(NU, 2), V(NV), VT(NV), B(NU, 2), BU(NU, NU, 2
     1   ), BUX(NU, NU, 2)
      DOUBLE PRECISION BUT(NU, NU, 2), BUTX(NU, NU, 2), BV(NU, NV, 2),
     1   BVT(NU, NV, 2)
C THE DEFAULT, NULL BC ROUTINE FOR POST.
      RETURN
      END
