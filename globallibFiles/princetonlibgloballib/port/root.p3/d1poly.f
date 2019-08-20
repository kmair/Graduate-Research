      SUBROUTINE D1POLY(BOOL,HR,HI,QHR,QHI)
C COMPUTES  T = -P(S)/H(S).
C BOOL   - LOGICAL, SET TRUE IF H(S) IS ESSENTIALLY ZERO.
C COMMON AREA
      COMMON/P99PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN
C
      DOUBLE PRECISION SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN
      DOUBLE PRECISION HR(1),HI(1),QHR(1),QHI(1)
      DOUBLE PRECISION HVR,HVI,CD1MOD
      LOGICAL BOOL
      N = NN-1
C EVALUATE H(S).
      CALL D7POLY(N,SR,SI,HR,HI,QHR,QHI,HVR,HVI)
      BOOL = CD1MOD(HVR,HVI) .LE. ARE*10.0D0*CD1MOD(HR(N),HI(N))
      IF (BOOL) GO TO 10
          CALL CD1DIV(-PVR,-PVI,HVR,HVI,TR,TI)
          RETURN
   10 TR = 0.0D0
      TI = 0.0D0
      RETURN
      END