      SUBROUTINE POSTA(T, X, NX, U, UX, UT, UTX, NU, V, VT, NV, A,
     1   AU, AUX, AUT, AUTX, AV, AVT, F, FU, FUX, FUT, FUTX, FV, FVT)
      INTEGER NU, NV, NX
      REAL T, X(NX), U(NX, NU), UX(NX, NU), UT(NX, NU), UTX(NX, NU)
      REAL V(NV), VT(NV), A(NX, NU), AU(NX, NU, NU), AUX(NX, NU, NU),
     1   AUT(NX, NU, NU)
      REAL AUTX(NX, NU, NU), AV(NX, NU, NV), AVT(NX, NU, NV), F(NX, NU),
     1   FU(NX, NU, NU), FUX(NX, NU, NU)
      REAL FUT(NX, NU, NU), FUTX(NX, NU, NU), FV(NX, NU, NV), FVT(NX,
     1   NU, NV)
C THE DEFAULT, NULL AF ROUTINE FOR POST.
      RETURN
      END
