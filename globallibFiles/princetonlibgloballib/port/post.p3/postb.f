      SUBROUTINE POSTB(T, L, R, U, UX, UT, UTX, NU, V, VT, NV, B
     1   , BU, BUX, BUT, BUTX, BV, BVT)
      INTEGER NU, NV
      REAL T, L, R, U(NU, 2), UX(NU, 2), UT(NU, 2)
      REAL UTX(NU, 2), V(NV), VT(NV), B(NU, 2), BU(NU, NU, 2), BUX(NU,
     1   NU, 2)
      REAL BUT(NU, NU, 2), BUTX(NU, NU, 2), BV(NU, NV, 2), BVT(NU, NV, 2
     1   )
C THE DEFAULT, NULL BC ROUTINE FOR POST.
      RETURN
      END
