         SUBROUTINE M44FT(N,NNS,A,B,IAB,JAB,W1,W2,IW,JW,T,SGN)
C
C   THIS SUBROUTINE PERFORMS A PRE-PROCESSING PASS IN A MULTIPLE
C   DISCRETE FOURIER TRANSFORM, FOR HALF-COMPLEX INPUT DATA. IT
C   RELIES ON THE METHOD OF COOLEY, TUKEY, LEWIS, AND WELCH.
C
         REAL A(1),B(1),W1(1),W2(1),T(2,1)
         REAL GI,GR,HI,HR,SGN,T1,T2
         INTEGER I,IAB,IAM,IAP,IW,IW1,J,JAB,JW,N,N2,NNS
C**COOLEY, TUKEY, LEWIS, AND WELCH
         N2  = N/2+1
         DO 20 J = 1,N2
            IAP = (J-1)*IAB + 1
            IAM = (N2-J)*IAB + 1
            IW1 = (J-1)*IW + 1
            T1  = T(1,J)
            T2  = SGN*T(2,J)
            DO 10 I = 1,NNS
               HR      = A(IAP) + A(IAM)
               HI      = B(IAP) - B(IAM)
               GR      = B(IAP) + B(IAM)
               GI      = A(IAM) - A(IAP)
               W1(IW1) = HR - T1*GR + T2*GI
               W2(IW1) = HI - T2*GR - T1*GI
               IAP     = IAP + JAB
               IAM     = IAM + JAB
               IW1     = IW1 + JW
 10         CONTINUE
 20      CONTINUE
C
         RETURN
         END
