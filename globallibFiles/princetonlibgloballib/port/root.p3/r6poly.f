      SUBROUTINE R6POLY(L1,PR,PI,HR,HI)
C COMPUTES  THE DERIVATIVE  POLYNOMIAL AS THE INITIAL H
C POLYNOMIAL AND COMPUTES L1 NO-SHIFT H POLYNOMIALS.
C COMMON AREA
      COMMON/P88PLY/SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,NN
C
      REAL SR,SI,TR,TI,PVR,PVI,ARE,MRE,ETA,INFIN,
     1    PR(1),PI(1),HR(1),HI(1)
      REAL XNI,T1,T2,CR1MOD
      N = NN-1
      NM1 = N-1
      DO 10 I = 1,N
          XNI = NN-I
          HR(I) = XNI*PR(I)/FLOAT(N)
          HI(I) = XNI*PI(I)/FLOAT(N)
   10 CONTINUE
      DO 50 JJ = 1,L1
          IF (CR1MOD(HR(N),HI(N)) .LE. ETA*10.0E0*CR1MOD(PR(N),PI(N)))
     *    GO TO 30
          CALL CR1DIV(-PR(NN),-PI(NN),HR(N),HI(N),TR,TI)
          DO 20 I = 1,NM1
               J = NN-I
               T1 = HR(J-1)
               T2 = HI(J-1)
               HR(J) = TR*T1-TI*T2+PR(J)
               HI(J) = TR*T2+TI*T1+PI(J)
   20     CONTINUE
          HR(1) = PR(1)
          HI(1) = PI(1)
          GO TO 50
C IF THE CONSTANT TERM IS ESSENTIALLY ZERO, SHIFT H COEFFICIENTS.
   30     DO 40 I = 1,NM1
               J = NN-I
               HR(J) = HR(J-1)
               HI(J) = HI(J-1)
   40     CONTINUE
          HR(1) = 0.0E0
          HI(1) = 0.0E0
   50 CONTINUE
      RETURN
      END
