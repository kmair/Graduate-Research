        REAL FUNCTION CGENM(N,A,IA)
        REAL SUM
        COMPLEX A(IA,1)
C THIS SUBROUTINE DETERMINES THE 1 NORM OF A MATRIX
C/6S
C        IF (N.LT.1) CALL SETERR(12HCGENM-N.LT.1,12,1,2)
C        IF (IA.LT.N)CALL SETERR(13HCGENM-IA.LT.N,13,2,2)
C/7S
         IF (N.LT.1) CALL SETERR('CGENM-N.LT.1',12,1,2)
         IF (IA.LT.N)CALL SETERR('CGENM-IA.LT.N',13,2,2)
C/
        CGENM=0.0
        DO 10 I=1,N
           SUM=0.0
           DO 5 J=1,N
              SUM=SUM+CABS(A(J,I))
 5         CONTINUE
           IF (SUM.GT.CGENM) CGENM=SUM
 10      CONTINUE
         RETURN
         END
