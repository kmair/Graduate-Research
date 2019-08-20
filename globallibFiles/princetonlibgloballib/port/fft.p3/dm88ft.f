         SUBROUTINE DM88FT(N,NNS,A,B,IAB,JAB,C,D,ICD,JCD)
C
C   THIS ROUTINE IS A DOUBLE ARRAY COPY FOR MULTIPLE FOURIER
C   TRANSFORM PACKAGE - MFT..
C
         DOUBLE PRECISION A(1),B(1),C(1),D(1)
         INTEGER I,IAB,ICD,J,J1,J2,JAB,JCD,N,NNS
C
         DO 1 J = 1,NNS
            J1 = (J-1)*JCD+1
            J2 = (J-1)*JAB+1
            DO 2 I = 1,N
               C(J1) = A(J2)
               D(J1) = B(J2)
               J1 = J1 + ICD
               J2 = J2 + IAB
 2          CONTINUE
 1       CONTINUE
         RETURN
         END
