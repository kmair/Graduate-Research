      SUBROUTINE DM1FT(A,B,C,D,T,IT,
     *           IC1,IC2,IC3,IC4,NNS,N,IFAC,LA,SGN)
C
C  SUBROUTINE DM1FT PERFORMS ONE PASS OF RADIX IFAC THROUGH DATA
C  IN THE COMPUTATION OF A MULTIPLE DISCRETE FOURIER TRANSFORM.
C  EACH CALLED SUBROUTINE DM2FT, M3FT, ETC. ARE RESPONSIBLE FOR
C  ACTUAL COMPUTATION OF RADIX IFAC = 2, IFAC = 3, ETC. IN THE
C  CASE THAT IFAC.GE.7, A GENERAL ODD FACTOR (PRIME) SUBROUTINE
C  M7FT IS CALLED. THIS IS A LOW LEVEL ROUTINE, WITH NO ERROR
C  CONDITIONS.
C
      DOUBLE PRECISION A(1),B(1),C(1),D(1),T(1)
      DOUBLE PRECISION SGN
      DOUBLE PRECISION C36,S36,C72,S72,S60
      INTEGER IA,IB,IC,IC1,IC2,IC3,IC4,ID,IE,IFAC,IGO,II,IT,IFACT
      INTEGER JA,JB,JC,JD,JE,JJ,LA,M,N,NNS
C
      COMMON /DM55FT/C36,S36,C72,S72,S60
C
      M   = N/IFAC
      II  = M*IC1
      JJ  = LA*IC2
      IFACT = IFAC+1
      IF(IFAC.GT.7)IFACT=8
      IGO = IFACT/2
      GO TO (10,20,30,40),IGO
C
C      FACTOR 2
C
 10   IA = 1
      JA = 1
      IB = IA+II
      JB = JA+JJ
      CALL DM2FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,
     *          A(IA),B(IA),A(IB),B(IB),
     *          C(JA),D(JA),C(JB),D(JB))
      GO TO 50
C
C      FACTOR 3
C
 20   IA = 1
      JA = 1
      IB = IA+II
      JB = JA+JJ
      IC = IB+II
      JC = JB+JJ
      CALL DM3FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,
     *          A(IA),B(IA),A(IB),B(IB),A(IC),B(IC),
     *          C(JA),D(JA),C(JB),D(JB),C(JC),D(JC))
      GO TO 50
C
C      FACTOR 5
C
 30   IA = 1
      JA = 1
      IB = IA+II
      JB = JA+JJ
      IC = IB+II
      JC = JB+JJ
      ID = IC+II
      JD = JC+JJ
      IE = ID+II
      JE = JD+JJ
      CALL DM5FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,
     *    A(IA),B(IA),A(IB),B(IB),A(IC),B(IC),A(ID),B(ID),A(IE),B(IE),
     *    C(JA),D(JA),C(JB),D(JB),C(JC),D(JC),C(JD),D(JD),C(JE),D(JE))
      GO TO 50
C
C      GENERAL ODD FACTOR
C
 40   CONTINUE
      CALL DM7FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,
     *          A(1),B(1),A(1),B(1),A(1),B(1),
     *          C(1),D(1),C(1),D(1),C(1),D(1))
 50   CONTINUE
      RETURN
      END
