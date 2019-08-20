      SUBROUTINE DM2FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,
     *                AIA,BIA,AIB,BIB,CJA,DJA,CJB,DJB)
C
C  RADIX 2 SUBROUTINE FOR MULTIPLE DISCRETE FOURIER TRANSFORM
C
      DOUBLE PRECISION AIA(1),BIA(1),AIB(1),BIB(1)
      DOUBLE PRECISION CJA(1),DJA(1),CJB(1),DJB(1)
      DOUBLE PRECISION T(1)
      DOUBLE PRECISION SGN,C1,S1
      INTEGER I,IB0,IC1,IC2,IC3,IC4,IFAC,IINK,IJK,IT
      INTEGER J,JB0,JINK,JUMP,K,KB,L,LA,LA1,M,N,NNS
C
      M    = N/IFAC
      IINK = M*IC1
      JINK = LA*IC2
      JUMP = (IFAC-1)*JINK
      IB0  = 1
      JB0  = 1
      DO 20 L = 1,LA
         I = IB0
         J = JB0
         DO 10 IJK = 1,NNS
            CJA(J) = AIA(I)+AIB(I)
            DJA(J) = BIA(I)+BIB(I)
            CJB(J) = AIA(I)-AIB(I)
            DJB(J) = BIA(I)-BIB(I)
            I = I+IC3
            J = J+IC4
 10      CONTINUE
         IB0 = IB0+IC1
         JB0 = JB0+IC2
 20   CONTINUE
      IF (LA.EQ.M) RETURN
      LA1 = LA+1
      JB0 = JB0+JUMP
      DO 50 K = LA1,M,LA
         KB = (K+K-2)*IT
         C1 = T(KB+1)
         S1 = SGN*T(KB+2)
         DO 40 L = 1,LA
            I = IB0
            J = JB0
            DO 30 IJK = 1,NNS
               CJA(J) = AIA(I)+AIB(I)
               DJA(J) = BIA(I)+BIB(I)
               CJB(J) = C1*(AIA(I)-AIB(I))-S1*(BIA(I)-BIB(I))
               DJB(J) = S1*(AIA(I)-AIB(I))+C1*(BIA(I)-BIB(I))
               I = I+IC3
               J = J+IC4
 30         CONTINUE
            IB0 = IB0+IC1
            JB0 = JB0+IC2
 40      CONTINUE
         JB0 = JB0+JUMP
 50   CONTINUE
      RETURN
      END
