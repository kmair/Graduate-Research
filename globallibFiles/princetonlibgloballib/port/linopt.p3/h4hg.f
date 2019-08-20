      SUBROUTINE H4HG(N,U,H,G,K)
C
C THIS SUBROUTINE GENERATES A HOUSEHOLD TRANSFORMATION
C
       REAL G,U(N)
       REAL H,S,SASUM,SCALE
        IF (N.LT.1)RETURN
       SCALE=SASUM(N,U,1)
       S=0.0
       H=1.0
       IF (SCALE.EQ.0.0)RETURN
        DO 10 I=1,N
          U(I)=U(I)/SCALE
          S=S+U(I)*U(I)
 10    CONTINUE
       G=SQRT(S)
       IF(U(K).GT.0.0)G=-G
       H=-(S-G*U(K))
       U(K)=U(K)-G
       G=G*SCALE
       RETURN
       END
