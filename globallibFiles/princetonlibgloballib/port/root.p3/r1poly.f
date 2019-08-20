      SUBROUTINE R1POLY(BOOL,HR,HI,QHR,QHI)
C COMPUTES  T = -P(S)/H(S).
C BOOL   - LOGICAL, SET TRUE IF H(S) IS ESSENTIALLY ZERO.
C COMMON AREA
      COMMON/P88PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN
C
      REAL SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN
      REAL HR(1),HI(1),QHR(1),QHI(1)
      REAL HVR,HVI,CR1MOD
      LOGICAL BOOL
      N = NN-1
C EVALUATE H(S).
      CALL R7POLY(N,SR,SI,HR,HI,QHR,QHI,HVR,HVI)
      BOOL = CR1MOD(HVR,HVI) .LE. ARE*10.0E0*CR1MOD(HR(N),HI(N))
      IF (BOOL) GO TO 10
          CALL CR1DIV(-PVR,-PVI,HVR,HVI,TR,TI)
          RETURN
   10 TR = 0.0E0
      TI = 0.0E0
      RETURN
      END
