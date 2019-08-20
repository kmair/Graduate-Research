      SUBROUTINE DC2LSQ(C,W,B,V,N,M,IRHS,NA)
C
C                    T         T
C COMPUTE SOLUTION VW B WHERE W (I)=0 IF W(I) IS SMALL AND
C OTHERWISE IS 1/W(I)
      INTEGER I,J,K
      INTEGER N,M,IRHS,NA
      DOUBLE PRECISION C(N),W(N),B(NA,IRHS),V(N,N)
      DOUBLE PRECISION BB
      DO 100 K=1,IRHS
      DO 101 I=1,N
         C(I) = 0.D0
 101  CONTINUE
      DO 102 I=1,N
         BB = 0.D0
         IF (W(I).GT.0.D0) BB = B(I,K)/W(I)
         DO 102 J=1,N
            C(J) = C(J) + BB*V(J,I)
 102  CONTINUE
      DO 103 I=1,N
         B(I,K) = C(I)
 103  CONTINUE
 100  CONTINUE
      RETURN
      END
