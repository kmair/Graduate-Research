          REAL FUNCTION CHENM(N,A)
          COMPLEX A(1)
          REAL SUM
C THIS SUBROUTINE COMPUTES THE 1 NORM OF A HERMETIAN MATRIX
C STORED IN PACKED FORM
C/6S
C         IF (N.LT.1) CALL SETERR(12HCHENM-N.LT.1,12,1,2)
C/7S
          IF (N.LT.1) CALL SETERR('CHENM-N.LT.1',12,1,2)
C/
          L=0
          CHENM=0.0
          DO 10 I=1,N
             SUM=0.E0
             IF(I.EQ.1) GO TO 4
             IM1=I-1
             JJ=I
            DO 3 J=1,IM1
                SUM=SUM+CABS(A(JJ))
                JJ=JJ+N-J
 3           CONTINUE
 4           CONTINUE
             DO 5 J=I,N
                L=L+1
                SUM=SUM+CABS(A(L))
 5           CONTINUE
             IF (SUM.GT.CHENM) CHENM=SUM
 10        CONTINUE
           RETURN
           END
