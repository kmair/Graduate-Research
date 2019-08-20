          REAL FUNCTION SYNM(N,A)
           REAL A(1),SUM
C THIS SUBROUTINE FINDS THE NORM OF A, A SYMMETRIC
C MATRIX IN PACKED FORM
C/6S
C           IF (N.LT.1) CALL SETERR(12H SYNM-N.LT.1,12,1,2)
C/7S
            IF (N.LT.1) CALL SETERR(' SYNM-N.LT.1',12,1,2)
C/
          L=0
          SYNM=0.0
          DO 10 I=1,N
             SUM=0.0
             IF(I.EQ.1) GO TO 4
             IM1=I-1
             JJ=I
            DO 3 J=1,IM1
                SUM=SUM+ABS(A(JJ))
                JJ=JJ+N-J
 3           CONTINUE
 4           CONTINUE
             DO 5 J=I,N
                L=L+1
                SUM=SUM+ABS(A(L))
 5           CONTINUE
             IF (SUM.GT.SYNM) SYNM=SUM
 10        CONTINUE
            RETURN
           END
