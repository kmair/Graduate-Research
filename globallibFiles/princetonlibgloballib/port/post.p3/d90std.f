      LOGICAL FUNCTION D90STD(K, X, NX, U, UT, NU, V, VT, NV, T,
     1   DT, UOFV, UOFVT, JACDIM, D1234, D, SCALE, DU, DV)
      INTEGER JACDIM, NV
      EXTERNAL D1234, D, SCALE
      INTEGER K, NX, NU
      DOUBLE PRECISION X(1), U(1), UT(1), V(NV), VT(NV), T
      DOUBLE PRECISION DT, UOFV(JACDIM, 1), UOFVT(JACDIM, 1), DU(1), DV(
     1   NV)
      COMMON /CSTAK/ DS
      DOUBLE PRECISION DS(500)
      COMMON /D9OSTG/ TJ, DTJ, GETJAC, SEPATE
      LOGICAL GETJAC, SEPATE
      DOUBLE PRECISION TJ, DTJ
      COMMON /D9OSTJ/ IZAP, ID4, ID5, IZAP1, ID, IDIAG, IPIVOT
      INTEGER IZAP(18), ID4(3), ID5(3), IZAP1(10), ID, IDIAG
      INTEGER IPIVOT
      INTEGER I, L, IS(1000)
      REAL RS(1000)
      LOGICAL FAILED, D90ST0, LS(1000)
      DOUBLE PRECISION WS(500)
      INTEGER TEMP, TEMP1, TEMP2, TEMP3, TEMP4, TEMP5
      EQUIVALENCE (DS(1), WS(1), RS(1), IS(1), LS(1))
C SCRATCH SPACE ALLOCATED - S(D90STD) =
C     2*K*(K+NU*(NV+1)) + NV**2 +  NV*(2*NV+1) +
C     MAX ( S(D1234), 3*K, 2*NV + NV INTEGER )
C LONG REAL WORDS.
C X(NX),(U,UT)(NX-K,NU),
C           (UOFV,UOFVT)(JACDIM,NV+1), DU(NX-K,NU).
      CALL ENTER(1)
      CALL SETD(NV, 0D0, DV)
      TEMP5 = ID4(1)
      TEMP4 = ID4(2)
      TEMP3 = ID4(3)
      TEMP2 = ID5(1)
      TEMP1 = ID5(2)
      TEMP = ID5(3)
      FAILED = D90ST0(K, X, NX, U, UT, NU, V, VT, NV, T, DT, UOFV,
     1   D1234, D, SCALE, DV, WS(TEMP5), WS(TEMP4), WS(TEMP3), WS(ID),
     2   WS(TEMP2), WS(TEMP1), WS(TEMP), JACDIM, NX-K, WS(IDIAG), IS(
     3   IPIVOT))
      IF (NU .LE. 0) GOTO 3
         DO  2 L = 1, NV
            DO  1 I = 1, JACDIM
               DU(I) = DU(I)+DV(L)*UOFVT(I, L+1)
   1           CONTINUE
   2        CONTINUE
   3  CALL LEAVE
      D90STD = FAILED
      RETURN
      END
