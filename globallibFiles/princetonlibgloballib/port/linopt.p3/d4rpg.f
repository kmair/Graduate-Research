       SUBROUTINE D4RPG(M,N,KK,Q,IQ,LT,AG,AS,E,IPTG,INDX2,DVECG,RHS,
     1  C,IPTS)
        INTEGER DVECG(1)
       INTEGER M,N,KK,AG,AS,E,IPTG(1),AGM1,AGPE,ITEMP,INDX2,NMS
       REAL Q(IQ, 1), LT( 1),RHS(N), C(N)
       INTEGER IPTS(1)
       AGPE = AG +E
       AGM1=AGPE-1
C
C THIS SUBROUTINE UPDATES THE VECTORS AND CALLS A
C SUBROUTINE TO UPDATE THE LQ DECOMPOSITION WHEN
C A GENERAL CONSTRAINT IS DROPPED
C
       NMS = N - AS
       ITEMP = IPTG(INDX2)
       IF (AGPE .EQ. INDX2) GOTO 20
       DO 10 I = INDX2,AGM1
            IPTG(I) = IPTG(I+1)
 10         CONTINUE
       IPTG(AGPE) = ITEMP
 20    CONTINUE
       DVECG(ITEMP) = 1
       IF (AGM1.GT.0) GO TO 50
          DO 40 I=1,NMS
             DO 30 J=1,NMS
                Q(I,J)=0.0
 30         CONTINUE
            Q(I,I)=1.0
            IPAS=I+AS
            IK=IPTS(IPAS)
            RHS(I)=C(IK)
 40       CONTINUE
          GO TO 60
 50       CONTINUE
       CALL D4GQR(IQ,NMS,AGPE,Q,LT,INDX2,RHS)
 60    AG = AG - 1
       KK = KK - 1
       RETURN
       END