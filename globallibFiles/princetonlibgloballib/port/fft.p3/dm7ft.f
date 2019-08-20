      SUBROUTINE DM7FT(NNS,N,IFAC,LA,IC1,IC2,IC3,IC4,T,IT,SGN,
     *                A1,B1,A2,B2,A3,B3,C1,D1,C2,D2,C3,D3)
C
C   GENERAL ODD PRIME FACTOR SUBROUTINE FOR MULTIPLE DISCRETE
C   FOURIER TRANSFORM.
C
      DOUBLE PRECISION A1(1),B1(1),A2(1),B2(1),A3(1),B3(1)
      DOUBLE PRECISION C1(1),D1(1),C2(1),D2(1),C3(1),D3(1)
      DOUBLE PRECISION T(1)
      DOUBLE PRECISION SGN,T1,T2,TEMPI,TEMPR
      INTEGER IA,IAI,IB,IC1,IC2,IC3,IC4,IFAC,II,IINK,IJK,IM,IP,IPI
      INTEGER IQ,IQI,IT,IZ,J,JA,JAJ,JINK,JJ,JP,JPJ,JQ,JQJ,JUMP
      INTEGER K,KINK,KT,KU,KV,KX,L,LA,LINK,M,MINK,N,N2,NN,NNS
C
      M    = N/IFAC
      IINK = M*IC1
      JINK = LA*IC2
      JUMP = (IFAC-1)*JINK
      IA   = 1
      JA   = 1
      KT   = M+M
      LINK = (IFAC-1)*IINK
      MINK = LINK/2
      N2   = (N+1)/2
      NN   = N+N
      DO 140 K = 1,M,LA
         KINK = K+K-2
         DO 130 L = 1,LA
            IB = IA+IINK
            IM = IA+MINK
            IZ = IA+LINK
            IQ = IZ
            IAI = IA
            JAJ = JA
            DO 10 IJK = 1,NNS
               C1(JAJ) = A1(IAI)
               D1(JAJ) = B1(IAI)
               IAI = IAI+IC3
               JAJ = JAJ+IC4
 10         CONTINUE
            DO 30 IP = IB,IM,IINK
               IPI = IP
               IQI = IQ
               JAJ = JA
               DO 20 IJK = 1,NNS
                  TEMPR = A1(IPI)+A1(IQI)
                  A2(IQI) = A1(IPI)-A1(IQI)
                  A3(IPI) = TEMPR
                  C1(JAJ) = C1(JAJ)+TEMPR
                  TEMPI = B1(IPI)+B1(IQI)
                  B2(IQI) = B1(IPI)-B1(IQI)
                  B3(IPI) = TEMPI
                  D1(JAJ) = D1(JAJ)+TEMPI
                  IPI = IPI+IC3
                  IQI = IQI+IC3
                  JAJ = JAJ+IC4
 20            CONTINUE
               IQ = IQ-IINK
 30         CONTINUE
            JP = JA+JINK
            JQ = JA+JUMP
            KU = KT
            DO 90 II = IB,IM,IINK
               IAI = IA
               JPJ = JP
               JQJ = JQ
               DO 40 IJK = 1,NNS
                  C2(JPJ) = A1(IAI)
                  D2(JPJ) = B1(IAI)
                  C3(JQJ) = A1(IAI)
                  D3(JQJ) = B1(IAI)
                  IAI = IAI+IC3
                  JPJ = JPJ+IC4
                  JQJ = JQJ+IC4
 40            CONTINUE
               KV = KU
               IQ = IZ
               DO 80 IP = IB,IM,IINK
                  KX = MOD(KV,NN)
                  IF(KX.LE.N) GO TO 50
                     KX = (NN - KX)*IT
                     T1 = T(KX+1)
                     T2 = -SGN*T(KX+2)
                     GO TO 60
 50               CONTINUE
                     KX = KX*IT
                     T1 = T(KX+1)
                     T2 = SGN*T(KX+2)
 60               CONTINUE
                  IPI = IP
                  IQI = IQ
                  JPJ = JP
                  JQJ = JQ
                  DO 70 IJK = 1,NNS
                     C2(JPJ) = C1(JPJ)+A1(IPI)*T1
     *                                -B1(IQI)*T2
                     D2(JPJ) = D1(JPJ)+B1(IPI)*T1
     *                                +A1(IQI)*T2
                     C3(JQJ) = C1(JQJ)+A1(IPI)*T1
     *                                +B1(IQI)*T2
                     D3(JQJ) = D1(JQJ)+B1(IPI)*T1
     *                                -A1(IQI)*T2
                     IPI = IPI+IC3
                     IQI = IQI+IC3
                     JPJ = JPJ+IC4
                     JQJ = JQJ+IC4
 70               CONTINUE
                  KV = KV+KU
                  IQ = IQ-IINK
 80            CONTINUE
               JP = JP+JINK
               JQ = JQ-JINK
               KU = KU+KT
 90         CONTINUE
            IA = IA+IC1
            IF(KINK.EQ.0)GO TO 120
            KU = KINK
            JP = JA+JINK
            JQ = JA+JUMP
            DO 110 JJ = JP,JQ,JINK
               J = JJ
               KX = KU*IT
               T1 = T(KX+1)
               T2 = SGN*T(KX+2)
               DO 100 IJK = 1,NNS
                  TEMPR = C1(J)*T1-D1(J)*T2
                  TEMPI = C1(J)*T2+D1(J)*T1
                  C2(J) = TEMPR
                  D2(J) = TEMPI
                  J = J+IC4
 100           CONTINUE
               KU = KU+KINK
 110        CONTINUE
 120        JA = JA+IC2
 130     CONTINUE
         JA = JA+JUMP
 140  CONTINUE
      RETURN
      END
