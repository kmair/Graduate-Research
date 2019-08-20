      SUBROUTINE DGQ0IN(N,X,W)
C
C  TO COMPUTE THE ABCISSAE (X) AND WEIGHTS (W) FOR AN N POINT
C  GAUSS-LAGUERRE QUADRATURE RULE ON (0,+INFINITY).
C
C  SCRATCH SPACE ALLOCATED - 17*N DOUBLE PRECISION WORDS.
C
C  ERROR STATES -
C
C    1 - N.LT.1.
C
      DOUBLE PRECISION X(N),W(N)
C
      COMMON /CSTAK/S
      DOUBLE PRECISION S(500)
C
C/6S
C     IF (N.LT.1)
C    1   CALL SETERR(15HDGQ0IN - N.LT.1,15,1,2)
C/7S
      IF (N.LT.1)
     1   CALL SETERR('DGQ0IN - N.LT.1',15,1,2)
C/
C
      N2=2*N
C
C ... ALLOCATE SCRATCH SPACE FOR A,B,C AND NU.
C
      IA=ISTKGT(8*N,4)-1
      IB=IA+N2
      IC=IB+N2
      INU=IC+N2
C
C ... COMPUTE A,B,C AND NU FOR CALL TO DGAUSQ.
C
      DO 10 I=1,N2
         IDXA=IA+I
         IDXB=IB+I
         IDXC=IC+I
         IDXNU=INU+I
         S(IDXA)=DBLE(FLOAT(I))
         S(IDXB)=DBLE(FLOAT(2*I-1))
         S(IDXC)=DBLE(FLOAT(I-1))
 10      S(IDXNU)=0.0D0
      S(INU+1)=1.0D0
C
C ... GET X AND W.
C
      CALL DGAUSQ(N,S(IA+1),S(IB+1),S(IC+1),S(INU+1),X,W)
C
      CALL ISTKRL(1)
C
      RETURN
C
      END
