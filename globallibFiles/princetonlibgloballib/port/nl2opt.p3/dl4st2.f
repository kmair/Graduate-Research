      SUBROUTINE DL4ST2(X,N,P,F,B,Y,W,R,RLEN,IPVT)
C
C EVALUATES NORM OF LAST (N-P) ELEMENTS OF QY
C
      INTEGER P,PI,RLEN
      DOUBLE PRECISION B(N,P),Y(N),W(P),R(RLEN), X,F
      INTEGER IPVT(P)
      DOUBLE PRECISION S, DSQRT, R2N
      CALL GETAY(N,P,X,B,Y)
C
C QR DECOMPOSITION
C
      CALL DQ7RFH(IER,IPVT,N,N,0,P,B,R,RLEN,W)
      IF(IER .EQ. 0) PI=P
      IF(IER .EQ. 0) GO TO 100
      PI=IABS(IER)-1
C
C QY
C
  100 CALL DQ7APL(N,N,PI,B,Y,IER)
C
C CALCULATE NORM
C
      S=0.D0
      PI=PI+1
      DO 200 I=PI,N
  200    S=S+Y(I)*(Y(I))
      R2N=DSQRT(S)
      F=R2N
      RETURN
      END
