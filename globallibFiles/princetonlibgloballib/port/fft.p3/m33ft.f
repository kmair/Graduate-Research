         SUBROUTINE M33FT(N,NNS,W1,W2,W3,W4,IW,JW,Y1,Y2,IY,JY,T,SGN)
C
C   THIS SUBROUTINE PERFORMS THE LAST PASS THROUGH DATA FOR A
C   MULTIPLE DISCRETE FOURIER TRANSFORM, FOR REAL INPUT DATA.
C   IT RELIES ON THE METHOD OF COOLEY, TUKEY, LEWIS, AND WELCH.
C
         REAL W1(1),W2(1),W3(1),W4(1)
         REAL Y1(1),Y2(1)
         REAL T(2,1)
         REAL QI,QR,RI,RR,SGN,T1,T2
         INTEGER I,I1,I3,I4,IA,IB,IW,IY,J,JW,JY,N,N2,NNS
C
C**SET LAST ELEMENTS OF W
C
         IA  = 1
         IB  = (N/2)*IW + 1
         DO 10 J = 1,NNS
            W3(IB) = W1(IA)
            W4(IB) = W2(IA)
            IA = IA + JW
            IB = IB + JW
 10      CONTINUE
C
C**COOLEY, TUKEY, LEWIS, AND WELCH
C
         N2  = N/2 + 1
         DO 30 J = 1,N2
            I1  = (J-1)*IY + 1
            I3  = (J-1)*IW + 1
            I4  = (N2-J)*IW + 1
            T1  = T(1,J)
            T2  = SGN*T(2,J)
            DO 20 I = 1,NNS
               QR     = 0.5E0*(W1(I3) + W1(I4))
               QI     = 0.5E0*(W2(I3) - W2(I4))
               RR     = 0.5E0*(W2(I3) + W2(I4))
               RI     = -0.5E0*(W1(I3) - W1(I4))
               Y1(I1) = QR + T1*RR - T2*RI
               Y2(I1) = QI + T1*RI + T2*RR
               I1     = I1 + JY
               I3     = I3 + JW
               I4     = I4 + JW
 20         CONTINUE
 30      CONTINUE
         RETURN
         END
