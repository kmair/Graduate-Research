         SUBROUTINE CHEML(N,C,X,B)
C
C THIS SUBROUTINE SET B=AX WHERE X IS A VECTOR
C AND A IS A SYMMETRIC MATRIX PACKED INTO C
C
C INPUT PARAMETERS
C N      LENGTH OF VECTOR
C C      ARRAY INTO WHICH SYMMETRIC MATRIX IS PACKED ACCORDING
C        TO THE SCHEME
C        1
C        2 5
C        3 6 8
C        4 7 9 10
C X      N-VECTOR TO BE MULTPLIED
C OUTPUT PARAMETER
C B      AX
C ERROR CONDITIONS
C  1     N<1 FATAL
C
       COMPLEX C(1),X(N),B(N)
       COMPLEX SUM,XI
C/6S
C      IF (N.LT.1)CALL SETERR(13H CHEML-N.LT.1,13,1,2)
C/7S
       IF (N.LT.1)CALL SETERR(' CHEML-N.LT.1',13,1,2)
C/
       DO 10 I=1,N
          B(I)=CMPLX(0.0,0.0)
 10    CONTINUE
C
C L POINTS TO POSITION IN C ARRAY
C
       L=0
       DO 30 I=1,N
          L=L+1
          XI=X(I)
          B(I)=B(I)+C(L)*XI
          IF (I.EQ.N) RETURN
          IP1=I+1
          SUM=CMPLX(0.0,0.0)
          DO 20 J=IP1,N
             L=L+1
             SUM=SUM+CONJG(C(L))*X(J)
             B(J)=B(J)+C(L)*XI
 20       CONTINUE
          B(I)=B(I)+SUM
 30   CONTINUE
      RETURN
      END
