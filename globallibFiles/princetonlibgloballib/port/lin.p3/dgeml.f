       SUBROUTINE DGEML(N,A,IA,X,B)
C GENERAL MATRIX BY VECTOR MULTIPLICATION
C
         DOUBLE PRECISION A(IA,N),X(N),B(N)
C/6S
C        IF(N.LT.1) CALL SETERR(12HDGEML-N.LT.1,12,1,2)
C        IF (IA.LT.N) CALL SETERR(13HDGEML-IA.LT.N,13,2,2)
C/7S
         IF(N.LT.1) CALL SETERR('DGEML-N.LT.1',12,1,2)
         IF (IA.LT.N) CALL SETERR('DGEML-IA.LT.N',13,2,2)
C/
         DO 10 I=1,N
             B(I)=0.D0
  10     CONTINUE
         DO 20 I=1,N
            CALL DAXPY(N,X(I),A(1,I),1,B,1)
 20      CONTINUE
         RETURN
         END
