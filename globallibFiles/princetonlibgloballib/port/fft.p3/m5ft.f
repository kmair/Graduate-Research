      SUBROUTINE M5FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,
     *                AIA,BIA,AIB,BIB,AIC,BIC,AID,BID,AIE,BIE,
     *                CJA,DJA,CJB,DJB,CJC,DJC,CJD,DJD,CJE,DJE)
C
C   RADIX 5 SUBROUTINE FOR MULTIPLE DISCRETE FOURIER TRANSFORM
C
      REAL AIA(1),BIA(1),AIB(1),BIB(1),AIC(1)
      REAL BIC(1),AID(1),BID(1),AIE(1),BIE(1)
      REAL CJA(1),DJA(1),CJB(1),DJB(1),CJC(1)
      REAL DJC(1),CJD(1),DJD(1),CJE(1),DJE(1)
      REAL T(1)
      REAL SGN
      REAL C1,C2,C3,C4,COS36,COS72,S1,S2,S3,S4,SIN36,SIN72
      REAL C36,S36,C72,S72,S60
      INTEGER I,IB0,IC1,IC2,IC3,IC4,IFAC,IINK,IJK,IT
      INTEGER J,JB0,JINK,JUMP,K,KB,KC,KD,KE,L,LA,LA1,M,N,NNS
C
      COMMON /M55FT/C36,S36,C72,S72,S60
C
      M     = N/IFAC
      IINK  = M*IC1
      JINK  = LA*IC2
      JUMP  = (IFAC-1)*JINK
      IB0   = 1
      JB0   = 1
      COS36 = C36
      SIN36 = SGN*S36
      COS72 = C72
      SIN72 = SGN*S72
C
      DO 20 L = 1,LA
         I = IB0
         J = JB0
         DO 10 IJK = 1,NNS
            CJA(J) = AIA(I)+(AIB(I)+AIE(I))+(AIC(I)+AID(I))
            DJA(J) = BIA(I)+(BIB(I)+BIE(I))+(BIC(I)+BID(I))
            CJB(J) = (AIA(I)+COS72*(AIB(I)+AIE(I))
     *               -COS36*(AIC(I)+AID(I)))
     *               -(SIN72*(BIB(I)-BIE(I))
     *               +SIN36*(BIC(I)-BID(I)))
            CJE(J) = (AIA(I)+COS72*(AIB(I)+AIE(I))
     *               -COS36*(AIC(I)+AID(I)))
     *               +(SIN72*(BIB(I)-BIE(I))
     *               +SIN36*(BIC(I)-BID(I)))
            DJB(J) = (BIA(I)+COS72*(BIB(I)+BIE(I))
     *               -COS36*(BIC(I)+BID(I)))
     *               +(SIN72*(AIB(I)-AIE(I))
     *               +SIN36*(AIC(I)-AID(I)))
            DJE(J) = (BIA(I)+COS72*(BIB(I)+BIE(I))
     *               -COS36*(BIC(I)+BID(I)))
     *               -(SIN72*(AIB(I)-AIE(I))
     *               +SIN36*(AIC(I)-AID(I)))
            CJC(J) = (AIA(I)-COS36*(AIB(I)+AIE(I))
     *               +COS72*(AIC(I)+AID(I)))
     *               -(SIN36*(BIB(I)-BIE(I))
     *               -SIN72*(BIC(I)-BID(I)))
            CJD(J) = (AIA(I)-COS36*(AIB(I)+AIE(I))
     *               +COS72*(AIC(I)+AID(I)))
     *               +(SIN36*(BIB(I)-BIE(I))
     *               -SIN72*(BIC(I)-BID(I)))
            DJC(J) = (BIA(I)-COS36*(BIB(I)+BIE(I))
     *               +COS72*(BIC(I)+BID(I)))
     *               +(SIN36*(AIB(I)-AIE(I))
     *               -SIN72*(AIC(I)-AID(I)))
            DJD(J) = (BIA(I)-COS36*(BIB(I)+BIE(I))
     *               +COS72*(BIC(I)+BID(I)))
     *               -(SIN36*(AIB(I)-AIE(I))
     *               -SIN72*(AIC(I)-AID(I)))
            I = I+IC3
            J = J+IC4
 10      CONTINUE
         IB0 = IB0+IC1
         JB0 = JB0+IC2
 20   CONTINUE
      IF (LA.EQ.M) GOTO 60
      LA1 = LA+1
      JB0 = JB0+JUMP
      DO 50 K = LA1,M,LA
         KB = (K+K-2)*IT
         KC = KB+KB
         KD = KC+KB
         KE = KD+KB
         C1 = T(KB+1)
         S1 = SGN*T(KB+2)
         C2 = T(KC+1)
         S2 = SGN*T(KC+2)
         C3 = T(KD+1)
         S3 = SGN*T(KD+2)
         C4 = T(KE+1)
         S4 = SGN*T(KE+2)
         DO 40 L = 1,LA
            I = IB0
            J = JB0
            DO 30 IJK = 1,NNS
               CJA(J) = AIA(I)+(AIB(I)+AIE(I))+(AIC(I)+AID(I))
               DJA(J) = BIA(I)+(BIB(I)+BIE(I))+(BIC(I)+BID(I))
               CJB(J) =
     *             C1*((AIA(I)
     *             +COS72*(AIB(I)+AIE(I))-COS36*(AIC(I)+AID(I)))
     *             -(SIN72*(BIB(I)-BIE(I))+SIN36*(BIC(I)-BID(I))))
     *             -S1*((BIA(I)
     *             +COS72*(BIB(I)+BIE(I))-COS36*(BIC(I)+BID(I)))
     *             +(SIN72*(AIB(I)-AIE(I))+SIN36*(AIC(I)-AID(I))))
               DJB(J) =
     *             S1*((AIA(I)
     *             +COS72*(AIB(I)+AIE(I))-COS36*(AIC(I)+AID(I)))
     *             -(SIN72*(BIB(I)-BIE(I))+SIN36*(BIC(I)-BID(I))))
     *             +C1*((BIA(I)
     *             +COS72*(BIB(I)+BIE(I))-COS36*(BIC(I)+BID(I)))
     *             +(SIN72*(AIB(I)-AIE(I))+SIN36*(AIC(I)-AID(I))))
               CJE(J) =
     *             C4*((AIA(I)
     *             +COS72*(AIB(I)+AIE(I))-COS36*(AIC(I)+AID(I)))
     *             +(SIN72*(BIB(I)-BIE(I))+SIN36*(BIC(I)-BID(I))))
     *             -S4*((BIA(I)
     *             +COS72*(BIB(I)+BIE(I))-COS36*(BIC(I)+BID(I)))
     *             -(SIN72*(AIB(I)-AIE(I))+SIN36*(AIC(I)-AID(I))))
               DJE(J) =
     *             S4*((AIA(I)
     *             +COS72*(AIB(I)+AIE(I))-COS36*(AIC(I)+AID(I)))
     *             +(SIN72*(BIB(I)-BIE(I))+SIN36*(BIC(I)-BID(I))))
     *             +C4*((BIA(I)
     *             +COS72*(BIB(I)+BIE(I))-COS36*(BIC(I)+BID(I)))
     *             -(SIN72*(AIB(I)-AIE(I))+SIN36*(AIC(I)-AID(I))))
               CJC(J) =
     *             C2*((AIA(I)
     *             -COS36*(AIB(I)+AIE(I))+COS72*(AIC(I)+AID(I)))
     *             -(SIN36*(BIB(I)-BIE(I))-SIN72*(BIC(I)-BID(I))))
     *             -S2*((BIA(I)
     *             -COS36*(BIB(I)+BIE(I))+COS72*(BIC(I)+BID(I)))
     *             +(SIN36*(AIB(I)-AIE(I))-SIN72*(AIC(I)-AID(I))))
               DJC(J) =
     *             S2*((AIA(I)
     *             -COS36*(AIB(I)+AIE(I))+COS72*(AIC(I)+AID(I)))
     *             -(SIN36*(BIB(I)-BIE(I))-SIN72*(BIC(I)-BID(I))))
     *             +C2*((BIA(I)
     *             -COS36*(BIB(I)+BIE(I))+COS72*(BIC(I)+BID(I)))
     *             +(SIN36*(AIB(I)-AIE(I))-SIN72*(AIC(I)-AID(I))))
               CJD(J) =
     *             C3*((AIA(I)
     *             -COS36*(AIB(I)+AIE(I))+COS72*(AIC(I)+AID(I)))
     *             +(SIN36*(BIB(I)-BIE(I))-SIN72*(BIC(I)-BID(I))))
     *             -S3*((BIA(I)
     *             -COS36*(BIB(I)+BIE(I))+COS72*(BIC(I)+BID(I)))
     *             -(SIN36*(AIB(I)-AIE(I))-SIN72*(AIC(I)-AID(I))))
               DJD(J) =
     *             S3*((AIA(I)
     *             -COS36*(AIB(I)+AIE(I))+COS72*(AIC(I)+AID(I)))
     *             +(SIN36*(BIB(I)-BIE(I))-SIN72*(BIC(I)-BID(I))))
     *             +C3*((BIA(I)
     *             -COS36*(BIB(I)+BIE(I))+COS72*(BIC(I)+BID(I)))
     *             -(SIN36*(AIB(I)-AIE(I))-SIN72*(AIC(I)-AID(I))))
               I = I+IC3
               J = J+IC4
 30         CONTINUE
            IB0 = IB0+IC1
            JB0 = JB0+IC2
 40      CONTINUE
         JB0 = JB0+JUMP
 50   CONTINUE
 60   CONTINUE
      RETURN
      END
