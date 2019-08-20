      SUBROUTINE DM3FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,
     *                AIA,BIA,AIB,BIB,AIC,BIC,
     *                CJA,DJA,CJB,DJB,CJC,DJC)
C
C  RADIX 3 SUBROUTINE FOR MULTIPLE DISCRETE FOURIER TRANSFORM
C
      DOUBLE PRECISION AIA(1),BIA(1),AIB(1),BIB(1),AIC(1),BIC(1)
      DOUBLE PRECISION CJA(1),DJA(1),CJB(1),DJB(1),CJC(1),DJC(1)
      DOUBLE PRECISION T(1)
      DOUBLE PRECISION C1,C2,S1,S2,SGN
      DOUBLE PRECISION C36,S36,C72,S72,S60,SIN60
      INTEGER I,IB0,IC1,IC2,IC3,IC4,IFAC,IINK,IJK,IT
      INTEGER J,JB0,JINK,JUMP,K,KB,KC,L,LA,LA1,M,N,NNS
C
      COMMON /DM55FT/C36,S36,C72,S72,S60
C
      M     = N/IFAC
      IINK  = M*IC1
      JINK  = LA*IC2
      JUMP  = (IFAC-1)*JINK
      IB0   = 1
      JB0   = 1
      SIN60 = S60*SGN
C
      DO 20 L = 1,LA
         I = IB0
         J = JB0
         DO 10 IJK = 1,NNS
            CJA(J) = AIA(I)+(AIB(I)+AIC(I))
            DJA(J) = BIA(I)+(BIB(I)+BIC(I))
            CJB(J) = (AIA(I)-0.5D0*(AIB(I)+AIC(I)))
     *               -(SIN60*(BIB(I)-BIC(I)))
            CJC(J) = (AIA(I)-0.5D0*(AIB(I)+AIC(I)))
     *               +(SIN60*(BIB(I)-BIC(I)))
            DJB(J) = (BIA(I)-0.5D0*(BIB(I)+BIC(I)))
     *               +(SIN60*(AIB(I)-AIC(I)))
            DJC(J) = (BIA(I)-0.5D0*(BIB(I)+BIC(I)))
     *               -(SIN60*(AIB(I)-AIC(I)))
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
         KC = KB+KB
         C1 = T(KB+1)
         S1 = SGN*T(KB+2)
         C2 = T(KC+1)
         S2 = SGN*T(KC+2)
         DO 40 L = 1,LA
            I = IB0
            J = JB0
            DO 30 IJK = 1,NNS
               CJA(J) = AIA(I)+(AIB(I)+AIC(I))
               DJA(J) = BIA(I)+(BIB(I)+BIC(I))
               CJB(J) =
     *                  C1*((AIA(I)-0.5D0*(AIB(I)+AIC(I)))
     *                 -(SIN60*(BIB(I)-BIC(I))))
     *                 -S1*((BIA(I)-0.5D0*(BIB(I)+BIC(I)))
     *                 +(SIN60*(AIB(I)-AIC(I))))
               DJB(J) =
     *                  S1*((AIA(I)-0.5D0*(AIB(I)+AIC(I)))
     *                 -(SIN60*(BIB(I)-BIC(I))))
     *                 +C1*((BIA(I)-0.5D0*(BIB(I)+BIC(I)))
     *                 +(SIN60*(AIB(I)-AIC(I))))
               CJC(J) =
     *                  C2*((AIA(I)-0.5D0*(AIB(I)+AIC(I)))
     *                 +(SIN60*(BIB(I)-BIC(I))))
     *                 -S2*((BIA(I)-0.5D0*(BIB(I)+BIC(I)))
     *                 -(SIN60*(AIB(I)-AIC(I))))
               DJC(J) =
     *                  S2*((AIA(I)-0.5D0*(AIB(I)+AIC(I)))
     *                 +(SIN60*(BIB(I)-BIC(I))))
     *                 +C2*((BIA(I)-0.5D0*(BIB(I)+BIC(I)))
     *                 -(SIN60*(AIB(I)-AIC(I))))
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
