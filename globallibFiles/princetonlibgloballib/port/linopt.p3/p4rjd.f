      SUBROUTINE P4RJD(N, AS, AG, E, Q,IQ, IPTS, C, P, TEMP, RHS)
       INTEGER N
      INTEGER AS, AG, IPTS(1)
      REAL Q(IQ, 1), C(N),RHS(N)
      INTEGER NMS, ASP1, E, I, K, AGPE
      REAL P(N), TEMP(1)
      NMS = N-AS
      ASP1 = AS + 1
      AGPE = AG+E
       IAGP1=AGPE+1
       IF (AS.EQ.0 ) GO TO 20
       DO 10 I=1,AS
          K=IPTS(I)
          P(K)=0.0
 10    CONTINUE
 20    CONTINUE
          DO 30 I=ASP1,N
             TEMP(I)=0.0
 30       CONTINUE
          IF (AGPE.GT.0)CALL M5TOP(IQ,N,Q,1,AGPE,1,NMS,RHS,2,TEMP(ASP1))
          DO 40 I=ASP1,N
             K=IPTS(I)
             P(K)=C(K)-TEMP(I)
 40       CONTINUE
          RETURN
          END
