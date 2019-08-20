        REAL FUNCTION GENM(N,A,IA)
        REAL SUM,A(IA,1),SASUM
C THIS FUNCTION DETERMINES THE 1 NORM OF AN N X N MATRIX A OF
C OF ROW DIMENSION IA
C/6S
C       IF (N.LT.1) CALL SETERR(12H GENM-N.LT.1,12,1,2)
C       IF (IA.LT.N) CALL SETERR(13H GENM-IA.LT.N,13,2,2)
C/7S
        IF (N.LT.1) CALL SETERR(' GENM-N.LT.1',12,1,2)
        IF (IA.LT.N) CALL SETERR(' GENM-IA.LT.N',13,2,2)
C/
        GENM=0.0
        DO 10 I=1,N
           SUM=SASUM(N,A(1,I),1)
           IF (SUM.GT.GENM) GENM=SUM
 10      CONTINUE
         RETURN
         END