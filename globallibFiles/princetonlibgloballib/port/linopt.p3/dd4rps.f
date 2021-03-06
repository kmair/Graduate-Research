       SUBROUTINE DD4RPS(A,M,N,IA,AMAN,KK,Q,IQ,LT,AG,AS,E,IPTG,IPTS,
     1   DVECS,SIMP,ISIMP,INDD2,TEMP,B,RHS)
C
C THIS SUBROUTINE UPDATES THE VECTORS AND CALLS A SUBROUTINE
C TO UPDATE THE LQ DECOMPOSTION WHEN A SIMPLE CONSTRAINT IS
C DROPPED
C
       INTEGER ISIMP(1),IPTG(1),IA(1)
       INTEGER M,N,KK,AG,AS,E,IPTS(1),ITEMP,INDX2,AGPE,DVECS(1)
       DOUBLE PRECISION A(N),SIMP(N),Q(IQ,1),LT(1),B(N),TEMP(N),RHS(N)
       DOUBLE PRECISION TEMP1,TOUT, T1
       EXTERNAL AMAN
       INDX2 = INDD2 - AG-E
        CALL AMAN(.FALSE.,A,IA,N,1,TEMP,TOUT)
       ITEMP = IPTS(INDX2)
       ITEMP1 = ISIMP(INDX2)
       TEMP1 = SIMP(INDX2)
       NMS = N - AS
       AS = AS - 1
       IF (AS .LT. INDX2) GOTO  20
       DO 10 I = INDX2,AS
            DVECS(I) = DVECS(I+1)
            SIMP(I) = SIMP(I+1)
            ISIMP(I) = ISIMP(I+1)
 10         CONTINUE
       ISIMP(AS + 1) = ITEMP1
       SIMP(AS+1) = TEMP1
 20    CONTINUE
       NM1=N-1
       DO 30 I=INDX2,NM1
          IPTS(I)=IPTS(I+1)
 30    CONTINUE
       IPTS(N) = ITEMP
       DVECS(AS+1) = 1
C
C A NEW ROW IS TO BE ADDED TO THE DECOMPXOSTITION
C DETERMINE IT
C
       AGPE = AG + E
       IF (AGPE .EQ. 0) GOTO 50
       DO 40 II = 1, AGPE
            I = IPTG(II)
            CALL AMAN(.FALSE.,A,IA,N,I,TEMP,T1)
            B(II) = TEMP(ITEMP)
 40         CONTINUE
       CALL DD4SQR (IQ, NMS, AGPE, Q, LT, B, RHS)
 50    CONTINUE
       KK = KK - 1
       RETURN
       END
